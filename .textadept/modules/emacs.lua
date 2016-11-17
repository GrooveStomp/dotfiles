--[[
TODO(AARON):
- M-o doesn't work properly on the first line of the file.
- Write buffer name when saving with "C-x s".
- Implement incremental forward search.
- Implement incremental backward search.
- Fix buggy C-x k closing of views.
- See if it's possible to properly hide all scrollbars all the time.
- Implement C-x 0 to close the current view.
- M-f should go to the end of the word, not the beginning of the next one.
- Don't put closing single or double quote when typing one.
- Don't put closing parenthesis, bracket or brace when typing one.
- C-k should cut to the clipboard.
- M-g <tab> should go to column number on current line after prompt.
--]]

_EMACS = {}

keys.CLEAR = 'cg'

keys['cg'] = function()
  if _EMACS["search"] then
    _EMACS["search"] = nil
  end
end

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
keys['cd'] = buffer.clear
keys['ck'] = function()
  buffer.del_line_right()
  buffer.clear()
end

keys['ag'] = {
    ['ag'] = function()
      _,line = ui.dialogs.inputbox{title = 'Goto Line', informative_text = 'Line:', text = ''}
      buffer.goto_line(buffer,line - 1)
    end
}

keys['cs'] = function()
  ui.find.find_incremental(nil, true)
end
keys['cr'] = function()
  ui.find.find_incremental(nil, false)
end

keys['cx'] = {
    ['k'] = io.close_buffer,
    ['cc'] = quit,
    ['cs'] = function()
      io.save_file()
      ui.statusbar_text = 'Saved Buffer' -- TODO(AARON): Get name of buffer that was saved.
    end,
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
      current_view = view
      view.split(view, false)
      ui.goto_view(current_view)
      hide_ui_elements()
    end,
    ['3'] = function()
      current_view = view
      view.split(view, true)
      ui.goto_view(current_view)
      hide_ui_elements()
    end,
    ['o'] = function() ui.goto_view(1) end,
    ['b'] = ui.switch_buffer,
    ['0'] = function()
      unsplit_view = nil
      for i = 1, #_VIEWS do
        if not (_VIEWS[i] == _G.view) then
          unsplit_view = _VIEWS[i]
          break
        end
      end
      if unsplit_view then
        unsplit_view.unsplit(unsplit_view)
      end
    end,
    ['1'] = function()
      view.unsplit(view)
    end,
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
keys['ad'] = buffer.del_word_right
keys['a:'] = function() ui.command_entry.enter_mode('lua_command') end
