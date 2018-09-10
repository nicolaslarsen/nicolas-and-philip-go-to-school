" Key mappings

no <down> ddp
no <up> ddkP
ino <down> <Nop>
ino <left> <Nop>
ino <right> <Nop>
ino <up> <Nop>

nmap <TAB> <ESC>:NERDTreeToggle<CR>

nmap <S-TAB> <ESC>:tabnew<CR>
nmap ½ <ESC>:tabnext<CR>
no <left> <ESC>:tabprevious<CR>
no <right> <ESC>:tabnext<CR>

" Random stuff

set tabstop=2
execute pathogen#infect()
syntax on
filetype plugin indent on

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
colorscheme molokai
hi MatchParen   ctermfg=208  ctermbg=233 cterm=bold
set number
set expandtab
