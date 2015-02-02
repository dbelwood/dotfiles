" The basics
set nocompatible | filetype indent plugin on | syn on

" Setup VAM
fun! SetupVAM()
  let c = get(g:, 'vim_addon_manager', {})
  let g:vim_addon_manager = c
  let c.plugin_root_dir = expand($HOME, 1) . '/.vim/vim-addons'

  let &rtp.=(empty(&rtp)?'':',').c.plugin_root_dir.'/vim-addon-manager'
  if !isdirectory(c.plugin_root_dir.'/vim-addon-manager/autoload')
    execute '!git clone --depth=1 git://github.com/MarcWeber/vim-addon-manager '
        \ shellescape(c.plugin_root_dir.'/vim-addon-manager', 1)
  endif

  call vam#ActivateAddons([], {})
endfun
call SetupVAM()

" Load plugins
call vam#Scripts('./vimrc.plugins', {'tag_regex':'.*'})

" Options
set autoindent
set autoread
set autowriteall
set backspace=2
set backupcopy=yes
set clipboard=unnamed                 " Use OS X clipboard
set directory-=.
set encoding=utf-8
set expandtab
set ignorecase
set incsearch
set laststatus=2
set list
set listchars=tab:▸\ ,trail:▫         " Display trailing characters
set mouse=a                           " Basic mouse interaction
set number
set ruler
set shell=/bin/zsh
set shiftwidth=2
set showcmd
set smartcase
set softtabstop=2
set tabstop=4

" Mappings
let mapleader = ","

" Buffer movements
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k

" Text handling
noremap <leader>= :Align

" Vim Shell
noremap <leader>-> :VimShell<CR>

" Reload this file (I know Inception)
noremap <silent> <leader>V :source $MYVIMRC<CR>:filetype detect<CR>:exe ":echo 'vim source reloaded'"<CR>

autocmd BufRead,BufNewFile *.md set filetype=markdown
autocmd BufRead,BufNewFile *.md set spell

" Abbreviations
