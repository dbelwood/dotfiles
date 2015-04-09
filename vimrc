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
call vam#Scripts(expand($HOME, 1) . '/.vimrc.plugins', {'tag_regex':'.*'})

" Options
set t_Co=256
let g:solarized_termcolors=256
colorscheme solarized
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

" Highlight columns past 80 in a file
" let &colorcolumn=join(range(81,999),",")
highlight OverLength ctermbg=red ctermfg=white
match OverLength /\%>80v.\+/

" Mappings
let mapleader = ","

" Buffer movements
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
nnoremap <leader>b :buffers<CR>:buffer<Space>

" Go mappings
au FileType go nmap <leader>s <Plug>(go-implements)
au FileType go nmap <leader>i <Plug>(go-info)
au FileType go nmap <leader>gd <Plug>(go-doc)
au FileType go nmap <leader>gv <Plug>(go-doc-vertical)
au FileType go nmap <leader>gb <Plug>(go-doc-browser)
au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>b <Plug>(go-build)
au FileType go nmap <leader>t <Plug>(go-test)
au FileType go nmap <leader>c <Plug>(go-coverage)
au FileType go nmap <leader>ds <Plug>(go-def-split)
au FileType go nmap <leader>dv <Plug>(go-def-vertical)
au FileType go nmap <leader>dt <Plug>(go-def-tab)

" Text handling
noremap <leader>= :Align

" Vim Shell
noremap <leader>-> :VimShell<CR>

" Reload this file (I know Inception)
noremap <silent> <leader>V :source $MYVIMRC<CR>:filetype detect<CR>:exe ":echo 'vim source reloaded'"<CR>

autocmd BufRead,BufNewFile *.md set filetype=markdown
autocmd BufRead,BufNewFile *.md set spell
autocmd BufWritePre * :%s/\s\+$//e " Strip whitespace upon save

" Abbreviations

" Local settings
source ~/.vimrc.local
