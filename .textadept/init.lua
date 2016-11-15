ui.set_theme(not CURSES and 'dark' or 'term', {font = 'Deja Vu Sans Mono', fontsize = 12})
ui.tabs = false
textadept.editing.strip_trailing_spaces = true

-- Disable visual elements that I don't like.
hide_ui_elements = function()
  for i=1, #(_G._VIEWS) do
    _VIEWS[i].buffer.h_scroll_bar = false
    _VIEWS[i].buffer.v_scroll_bar = false
  end
end
hide_ui_elements()

-- Use Emacs key bindings.
keys.CLEAR = 'cg'

keys['ca'] = buffer.home
keys['ce'] = buffer.line_end
keys['cn'] = buffer.line_down
keys['cp'] = buffer.line_up
keys['co'] = function()
    buffer.line_end()
    buffer.new_line()
end
keys['c_'] = buffer.undo
keys['cf'] = buffer.char_right
keys['cb'] = buffer.char_left
keys['cv'] = buffer.page_down

keys['ag'] = {
    ['ag'] = function()
      _,line = ui.dialogs.inputbox{title = 'Goto Line', informative_text = 'Line:', text = ''}
      buffer.goto_line(buffer,line)
    end
}

keys['cx'] = {
    ['cc'] = quit,
    ['cs'] = io.save_file,
    ['cw'] = io.save_file_as,
    ['k'] = function()
      local res = view.unsplit(view)
      if (not res) then
        io.close_buffer()
      else
        hide_ui_elements()
      end
    end,
    ['cf'] = io.open_file,
    ['2'] = function()
      view.split(view, false)
      hide_ui_elements()
    end,
    ['3'] = function()
      view.split(view, true)
      hide_ui_elements()
    end,
    ['o'] = function() ui.goto_view(1) end,
    ['b'] = ui.switch_buffer,
}

keys['a<'] = buffer.document_start
keys['a>'] = buffer.document_end

keys.an = function()
    buffer.line_down()
    buffer.para_down()
    buffer.line_up()
end
keys.ap = function()
    buffer.line_up()
    buffer.para_up()
    buffer.line_up()
end
keys.ao = function()
    buffer.line_up()
    buffer.line_end()
    buffer.new_line()
end
keys['af'] = buffer.word_right
keys['ab'] = buffer.word_left
keys['av'] = buffer.page_up