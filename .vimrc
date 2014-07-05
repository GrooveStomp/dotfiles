let mapleader = ' '
call pathogen#infect()
call pathogen#helptags()

filetype plugin indent on " Enable indenting.
set number " Show line numbers.
set shell=/bin/bash
set noerrorbells "Error sounds
set novisualbell
set vb t_vb=""
" We need VimEnter because gvim seems to reset t_vb whenever a file is opened.
autocmd VimEnter * set vb t_vb="" " Disable visual bell
set bs=2 " Make backspace work correctly
set smartindent
set backup " Enable backup files
set backupdir=~/.backup " Set the backup directory
set noswapfile " Disable swap files
set wildmode=list:longest " Full-list file completion
set foldmethod=marker " Set fold configuration
set nowrap " Disable text wrapping
set textwidth=0
set tabstop=8 " Number of spaces a tab is displayed as.
set expandtab " Convert tabs into spaces.
set softtabstop=4 " Insert 4 spaces when tab is pressed.
set smarttab " Indent instead of tab at start of line.
set shiftwidth=2
set shiftround " Round spaces to nearest shiftwidth multiple.
set nojoinspaces " Don't convert spaces to tabs.
set autoindent " Indent new line at same indent as current line
set hlsearch " Show matched search items as highlighted text
set showmatch " Jump to matches as they are made
set incsearch " Highlights matches as you type them
set nocompatible " Disable compatibility with Vi.  Allows more advanced features.
set showmode
set laststatus=2 " Always show the status line (includes the filename)

set pastetoggle=<F2>
" Set a key to toggle auto-indenting when pasting
nnoremap <F2> :set invpaste paste?<CR>
" Set the status line format
" set statusline=%F\ [%{&ff}]\ %y\ %=(%l,%c)\ %=%p%%\
" set t_Co=256 " Set 256 color mode
" let loaded_matchparen = 1 " Disable highlighting the matching bracket/parenthesis/brace
syntax on " Enable syntax highlighting
" map <S-t> :TlistToggle<cr>

" Add Less support (Less is a superset of CSS)
au BufRead,BufNewFile *.less setfiletype css

let g:clj_highlight_builtins=1 " Highlight clojure's builtins
let g:clj_paren_rainbow=1 " Alternating colors for Lisp parenthesis

let g:lisp_rainbow=1
" let g:ackprg = 'ag --nogroup --nocolor --column'
let g:syntastic_ruby_exec = 'ruby2.1.1'

autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()
autocmd FileType java setlocal shiftwidth=4 tabstop=4

" Remove trailing whitespace on save.
autocmd FileType c,cpp,python,ruby,java autocmd BufWritePre <buffer> :%s/\s\+$//e

" Enable Hardmode.
autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()
" nnoremap <leader>h <Esc>:call ToggleHardMode()<CR>
nnoremap <leader>b :Bufferlist<Cr>
" nnoremap <leader>b :CtrlPBuffer<Cr>
nnoremap <leader>a :CtrlPMixed<Cr>
let g:ctrlp_map = '<leader>t'
nmap <leader>= :TagbarToggle<CR>
let g:tagbar_autofocus = 1
let g:haddock_browser = '/usr/bin/firefox'
let g:haddock_docdir = '/usr/share/doc/ghc-haddock'

colorscheme solarized
let term_bg_color = readfile('/home/aoman/.backup/.termbg', '', 1)[0]
if term_bg_color == "light"
  set background=light
else
  set background=dark
endif

colorscheme solarized

" Highlight trailing whitespace.
highlight ExtraWhitespace ctermbg=white ctermfg=black guibg=DarkMagenta guifg=black
"guibg=red
match ExtraWhitespace /\s\+$/

highlight ColorColumn ctermbg=white ctermfg=black guibg=DarkMagenta guifg=black
call matchadd('ColorColumn', '\%81v', 100)

if has('gui_running')
  set guioptions=Pac
  set guioptions-=T
  set guioptions-=m
  set guifont=Ubuntu\ Mono\ 12
else
  set mouse=a " Enable [a]ll mouse modes.
endif

set listchars=tab:>~,nbsp:_,trail:.
set list

set cursorline
imap hh <Esc>
