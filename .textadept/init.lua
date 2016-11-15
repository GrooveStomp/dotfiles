-- Disable visual elements that I don't like.
hide_ui_elements = function()
  for i=1, #(_G._VIEWS) do
    _VIEWS[i].buffer.h_scroll_bar = false
    _VIEWS[i].buffer.v_scroll_bar = false
  end
end
hide_ui_elements()

require 'emacs'

ui.set_theme(not CURSES and 'dark' or 'term', {font = 'Deja Vu Sans Mono', fontsize = 12})
ui.tabs = false
textadept.editing.strip_trailing_spaces = true
events.connect(events.INITIALIZED, function()
  textadept.menu.menubar = nil
end)
