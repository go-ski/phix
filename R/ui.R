# ============================================================================
#  ui.R — Shiny UI definition
#
#  All external-package calls are fully namespace-qualified (pkg::fun()) so
#  this file works regardless of which packages are attached at the time
#  shiny::runApp() auto-sources R/*.R.
#
#  Extensibility: add new panels (e.g., a caption editor, batch-rename tab)
#  as helper functions here, then compose them into the page layout below.
# ============================================================================

ui <- bslib::page_sidebar(
  title = "Photo GPS Editor",
  fillable = FALSE,

  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
    .loc-info { font-size:13px; line-height:1.6em; }

    #dir-autocomplete {
      position: absolute;
      z-index: 9999;
      background: #fff;
      border: 1px solid #ccc;
      border-top: none;
      border-radius: 0 0 4px 4px;
      box-shadow: 0 4px 8px rgba(0,0,0,.15);
      max-height: 220px;
      overflow-y: auto;
      display: none;
      box-sizing: border-box;
    }
    #dir-autocomplete .ac-item {
      padding: 5px 10px;
      font-size: 13px;
      cursor: pointer;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    #dir-autocomplete .ac-item:hover,
    #dir-autocomplete .ac-item.ac-active {
      background: #0d6efd;
      color: #fff;
    }
    ")),
    shiny::tags$script(shiny::HTML("
      // Returns true when the focused element is a text-entry control so that
      // arrow-key shortcuts don't interfere with typing or date pickers.
      function inTextInput() {
        var el = document.activeElement;
        if (!el) return false;
        var tag = el.tagName.toUpperCase();
        return tag === 'TEXTAREA' || tag === 'SELECT' ||
               (tag === 'INPUT' &&
                !/^(button|checkbox|radio|submit|reset)$/i.test(el.type || ''));
      }

      document.addEventListener('keydown', function(e) {
        // Enter in the place-search box clicks the Search button.
        if (e.key === 'Enter' && document.activeElement &&
            document.activeElement.id === 'search_q') {
          e.preventDefault();
          document.getElementById('search_go').click();
          return;
        }
        // ArrowLeft / ArrowRight navigate photos when not typing.
        if ((e.key === 'ArrowLeft' || e.key === 'ArrowRight') && !inTextInput()) {
          e.preventDefault();
          var btn = document.getElementById(e.key === 'ArrowLeft' ? 'prev' : 'nxt');
          if (btn) btn.click();
        }
      });

      (function() {
        var dropdown, inp, activeIdx = -1;

        function positionDropdown() {
          var r = inp.getBoundingClientRect();
          dropdown.style.left  = (r.left + window.scrollX) + 'px';
          dropdown.style.top   = (r.bottom + window.scrollY) + 'px';
          dropdown.style.width = r.width + 'px';
        }

        function hideDropdown() {
          dropdown.style.display = 'none';
          activeIdx = -1;
        }

        function showDropdown() {
          if (dropdown.children.length === 0) { hideDropdown(); return; }
          positionDropdown();
          dropdown.style.display = 'block';
        }

        function setActive(idx) {
          var items = dropdown.querySelectorAll('.ac-item');
          items.forEach(function(el, i) {
            el.classList.toggle('ac-active', i === idx);
          });
          if (idx >= 0 && idx < items.length) {
            items[idx].scrollIntoView({ block: 'nearest' });
          }
          activeIdx = idx;
        }

        document.addEventListener('DOMContentLoaded', function() {
          inp = document.getElementById('dir');
          if (!inp) return;

          dropdown = document.createElement('div');
          dropdown.id = 'dir-autocomplete';
          document.body.appendChild(dropdown);

          inp.setAttribute('autocomplete', 'off');

          // Keyboard navigation inside the dropdown.
          inp.addEventListener('keydown', function(e) {
            if (dropdown.style.display === 'none') return;
            var items = dropdown.querySelectorAll('.ac-item');
            if (e.key === 'ArrowDown') {
              e.preventDefault();
              setActive(Math.min(activeIdx + 1, items.length - 1));
            } else if (e.key === 'ArrowUp') {
              e.preventDefault();
              setActive(Math.max(activeIdx - 1, 0));
            } else if (e.key === 'Enter' && activeIdx >= 0) {
              e.preventDefault();
              var val = items[activeIdx].dataset.value;
              Shiny.setInputValue('dir', val, {priority: 'event'});
              inp.value = val;
              hideDropdown();
            } else if (e.key === 'Escape') {
              hideDropdown();
            }
          });

          // Hide on outside click.
          document.addEventListener('mousedown', function(e) {
            if (e.target !== inp && !dropdown.contains(e.target)) hideDropdown();
          });

          // Reposition on scroll/resize.
          window.addEventListener('scroll', function() {
            if (dropdown.style.display !== 'none') positionDropdown();
          }, true);
          window.addEventListener('resize', function() {
            if (dropdown.style.display !== 'none') positionDropdown();
          });
        });

        // Handler: receive an array of path strings and render the dropdown.
        Shiny.addCustomMessageHandler('dir_completions', function(paths) {
          if (!dropdown) return;
          dropdown.innerHTML = '';
          activeIdx = -1;
          paths.forEach(function(p) {
            var item = document.createElement('div');
            item.className = 'ac-item';
            item.textContent = p;
            item.dataset.value = p;
            item.addEventListener('mousedown', function(e) {
              e.preventDefault();          // keep focus on inp
              Shiny.setInputValue('dir', p, {priority: 'event'});
              inp.value = p;
              hideDropdown();
            });
            dropdown.appendChild(item);
          });
          showDropdown();
        });
      })();

      // ---- Photo popup window ---------------------------------------------
      // The popup always loads viewer.html — an HTML document — so the
      // browser never navigates to a raw image URL (which Safari downloads).
      // Photo changes are sent via postMessage; viewer.html signals
      // 'viewer_ready' on load so we know it is safe to post.
      //   force=true  => open / bring to front  (View photo button)
      //   force=false => update only if already open  (Prev / Next)
      var photoWin      = null;
      var pendingPhoto  = null;   // buffered URL waiting for viewer_ready

      // Compute the app's base URL once; works for sub-path deployments too.
      var _pathDir = window.location.pathname.replace(/[^/]*$/, '');
      var appBase  = window.location.origin + _pathDir;  // e.g. http://127.0.0.1:7465/
      var viewerUrl = appBase + 'thumbs/viewer.html';

      // Receive the handshake from viewer.html after it finishes loading.
      window.addEventListener('message', function(e) {
        if (e.data && e.data.type === 'viewer_ready' && pendingPhoto) {
          photoWin.postMessage({ photoUrl: pendingPhoto }, '*');
          pendingPhoto = null;
        }
      });

      Shiny.addCustomMessageHandler('photo_window_update', function(msg) {
        var absUrl  = appBase + msg.url;
        var already = photoWin && !photoWin.closed;
        if (msg.force || already) {
          if (already) {
            // viewer.html is loaded — update the image src directly.
            photoWin.postMessage({ photoUrl: absUrl }, '*');
            if (msg.force) photoWin.focus();
          } else {
            // Open viewer.html; send the photo URL once it signals ready.
            pendingPhoto = absUrl;
            photoWin = window.open(
              viewerUrl, 'shinyPhotoViewer',
              'width=900,height=700,resizable=yes,scrollbars=yes'
            );
          }
        }
      });
    "))
  ),

  # ---- Sidebar: load controls, navigation, edit panels --------------------
  sidebar = bslib::sidebar(
    width = 350,

    # Directory loader
    shiny::textInput("dir", "Photo directory", value = "",
                     placeholder = "/path/to/photos"),
    shiny::actionButton("load", "Load photos", class = "btn-primary w-100"),

    shiny::hr(style = "margin: 10px 0;"),

    # Compact photo counts
    shiny::uiOutput("counts_compact"),
    # Current photo status + navigation
    shiny::uiOutput("status"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::actionButton("prev", "\u25C0 Prev", class = "w-100"),
      shiny::actionButton("nxt",  "Next \u25B6", class = "w-100")
    ),

    shiny::hr(style = "margin: 10px 0;"),

    # Location readout
    shiny::uiOutput("locinfo"),

    shiny::hr(style = "margin: 10px 0;"),

    # Creation-date editor
    shiny::tags$strong("Creation date / time (UTC)"),
    shiny::div(class = "d-flex gap-2 align-items-end mt-1",
      shiny::div(class = "flex-grow-1",
                 shiny::dateInput("edit_date", label = NULL, value = Sys.Date())),
      shiny::div(shiny::textInput("edit_time", label = NULL, value = "00:00:00",
                                  placeholder = "HH:MM:SS"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::actionButton("copy_date",  "Copy date",  class = "w-100"),
      shiny::actionButton("paste_date", "Paste & save date", class = "w-100")
    ),

    # Single save action: writes whichever of the selected map point and the
    # date/time inputs above has actually changed, in one ExifTool call, then
    # advances.
    shiny::actionButton("save_both", "Save changes \u2192 photo",
                        class = "btn-success w-100 mt-2"),

    shiny::hr(style = "margin: 10px 0;"),

    # Copy / paste location
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::actionButton("copy",  "Copy location", class = "w-100"),
      shiny::actionButton("paste", "Paste & save",  class = "w-100")
    ),
    shiny::p(class = "text-muted mt-2", style = "font-size:12px;",
      "Search or click the map to choose a point, then Save changes. ",
      "Copy grabs the current photo\u2019s location; ",
      "Paste & save writes it to the photo you\u2019re on and moves to the next."),

    shiny::hr(style = "margin: 10px 0;"),

    # Photo viewer launcher
    shiny::actionButton("view_photo", "\U0001F4F7 View photo",
                        class = "btn-outline-secondary w-100"),
    shiny::p(class = "text-muted mt-1", style = "font-size:12px;",
      "Opens the current photo in a separate resizable window. ",
      "The window updates automatically as you navigate.")
  ),

  # ---- Main content --------------------------------------------------------

  # Map — full width now that the thumbnail lives in its own popup window
  bslib::card(
    bslib::card_body(
      class = "p-2",
      shiny::div(class = "d-flex gap-2 mb-2",
        shiny::div(class = "flex-grow-1",
          shiny::textInput("search_q", label = NULL,
                           placeholder = "Search a place, then press Enter or click Search")),
        shiny::actionButton("search_go", "Search", class = "btn-primary")
      ),
      leaflet::leafletOutput("map", height = "72vh")
    )
  ),

  # Photo list table
  bslib::card(
    bslib::card_header("Photo list"),
    DT::DTOutput("tbl")
  )
)
