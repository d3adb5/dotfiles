set nocompatible
set background=dark
set smartindent noexpandtab tabstop=4 shiftwidth=4 textwidth=80
set scrolloff=15 sidescrolloff=5 updatetime=750
set laststatus=2 fo+=t
set ttimeoutlen=0
set incsearch ruler nohlsearch splitbelow splitright number rnu
set fillchars+=vert:\ ,fold:\  backspace=indent,eol,start
set foldmethod=marker ignorecase smartcase
set listchars=tab:⁞\ ,trail:· list
set cursorline hidden
set wildignore=*.o,*.ho,*.hi,*.pdf,*.obj,*.jpg,*.png
set updatetime=100
set diffopt=internal,filler,closeoff,foldcolumn:0
let mapleader="\<Tab>"

filetype plugin on
syntax on

" {{{ colors
highlight Pmenu         ctermfg=7  ctermbg=0
highlight PmenuSel      ctermfg=15 ctermbg=8
highlight Todo          ctermfg=15 ctermbg=7
highlight TabLineFill   term=none  cterm=none ctermbg=none
highlight TabLine       term=none  cterm=none ctermbg=none
highlight TabLineSel    term=none  cterm=none ctermbg=8
highlight VertSplit     term=bold  cterm=none ctermfg=7  ctermbg=none
highlight StatusLine    term=none  cterm=bold ctermfg=0  ctermbg=7
highlight StatusLineNC  term=none  cterm=none ctermfg=none ctermbg=0
highlight Folded        ctermfg=15 ctermbg=0
highlight CursorLine    cterm=none ctermbg=0
highlight CursorLineNr  cterm=none
highlight SignColumn    ctermbg=none
highlight FileName      ctermfg=12 ctermbg=0
highlight DiffAdd       ctermbg=22
highlight DiffDelete    ctermbg=88
highlight DiffText      ctermbg=91
highlight DiffChange    ctermbg=24
" }}}
" {{{ plugins

" {{{ install vim-plug if it's not present

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
	silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dir https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" }}}

call plug#begin('~/.local/share/nvim/plugged')

	" {{{ essentials

	Plug 'tpope/vim-repeat'      " smarter repetition for .
	Plug 'tpope/vim-eunuch'      " files + some unix commands
	Plug 'tpope/vim-unimpaired'  " shortcuts for []
	Plug 'tpope/vim-surround'    " commands for dealing with surrounding chars
	Plug 'tpope/vim-commentary'  " deal with commenting
	Plug 'junegunn/fzf.vim'      " fuzzy finder for lines, files, commits, etc.

	Plug 'kana/vim-textobj-user' " lets users create text objects
	Plug 'somini/vim-textobj-fold' " fold text objects (az/iz)

	Plug 'junegunn/vim-easy-align' " align text based on markers

		xmap ga <Plug>(EasyAlign)
		nmap ga <Plug>(EasyAlign)

	" }}}
	" {{{ additional highlighting and convenience

	Plug 'lilydjwg/colorizer'              " color strings like #56dd00
	Plug 'keith/tmux.vim'                  " .tmux.conf syntax highlighting
	Plug 'martinda/Jenkinsfile-vim-syntax' " syntax highlighting for Jenkins
	" Plug 'mikelue/vim-maven-plugin'        " convenient functions from Apache Maven
	Plug 'machakann/vim-highlightedyank'   " highlight yanked lines

		let g:highlightedyank_highlighted_duration = 1000

	Plug 'plasticboy/vim-markdown'         " markdown stuff
	Plug 'dkarter/bullets.vim'             " markdown bullets convenience mappings
	Plug 'ntpeters/vim-better-whitespace'  " better... whitespace

		au BufRead * EnableStripWhitespaceOnSave

	Plug 'junegunn/goyo.vim'               " distraction-free view for editing
	Plug 'hashivim/vim-terraform'          " terraform related convenience

	" }}}
	" {{{ to do with git

	Plug 'tpope/vim-fugitive' " git wrapper

		nnoremap <leader>gd :Gvdiffsplit!<CR>
		nnoremap gch :diffget //2<CR>
		nnoremap gcl :diffget //3<CR>

	Plug 'tpope/vim-rhubarb'  " redirects to GitHub
	Plug 'junegunn/gv.vim'    " git commit browser

	Plug 'mhinz/vim-signify'               " diff showing in real time

		highlight SignifySignAdd ctermfg=green ctermbg=none cterm=none
		highlight SignifySignDelete ctermfg=red ctermbg=none cterm=none
		highlight SignifySignChange ctermfg=yellow ctermbg=none cterm=none

		autocmd User SignifyHunk call s:show_current_hunk()

		function! s:show_current_hunk() abort
			let h = sy#util#get_hunk_stats()
			if !empty(h)
				echo printf('[Hunk %d/%d]', h.current_hunk, h.total_hunks)
			endif
		endfunction

	" }}}
	" {{{ snippets

	Plug 'Sirver/ultisnips'   " snippet engine
	Plug 'honza/vim-snippets' " a collection of nice UltiSnips snippets

		let g:UltiSnipsExpandTrigger = "<tab>"
		let g:UltiSnipsJumpForwardTrigger = "<c-j>"
		let g:UltiSnipsJumpBackwardTrigger = "<c-k>"
		let g:UltiSnipsEditSplit = "vertical"
		let g:UltiSnipsSnippetDirectories = ["UltiSnips"]

	" }}}
	" {{{ the nerd (file) tree

	Plug 'scrooloose/nerdtree'         " the NERDtree

		let g:NERDTreeDirArrowExpandable = '+'
		let g:NERDTreeDirArrowCollapsible = '-'
		let g:NERDTreeMinimalUI = 1
		let g:NERDTreeIgnore = ['\.hi$','\.o$','\.class$']

	Plug 'Xuyuanp/nerdtree-git-plugin' " NERDtree git flags

	" }}}
	" {{{ completion, linting, andlanguage server protocol

	Plug 'neovim/nvim-lspconfig'    " language server configuration quickstart
	Plug 'mfussenegger/nvim-lint'   " asynchronous linter plugin
"	Plug 'nvim-lua/completion-nvim' " better completion framework for built-in LSP
	Plug 'hrsh7th/cmp-nvim-lsp'
	Plug 'hrsh7th/cmp-buffer'
	Plug 'hrsh7th/cmp-path'
	Plug 'hrsh7th/nvim-cmp'
	Plug 'quangnguyen30192/cmp-nvim-ultisnips'

		set completeopt=menu,menuone,noinsert,noselect
		set shortmess+=c
		let g:completion_enable_snippet = 'UltiSnips'
		let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']
		let g:completion_matching_smart_case = 1
		let g:completion_trigger_keyword_length = 3

	" {{{ key mappings

	nnoremap K  <cmd>lua vim.lsp.buf.hover()<cr>
	nnoremap gd <cmd>lua vim.lsp.buf.definition()<cr>
	nnoremap gi <cmd>lua vim.lsp.buf.implementation()<cr>
	nnoremap gr <cmd>lua vim.lsp.buf.references()<cr>
	nnoremap gD <cmd>lua vim.lsp.buf.declaration()<cr>

	nnoremap <C-s> <cmd>lua vim.lsp.buf.signature_help()<cr>

	nnoremap <leader>f  <cmd>lua vim.lsp.buf.formatting()<cr>
	nnoremap <leader>D  <cmd>lua vim.lsp.buf.type_definition()<cr>
	nnoremap <leader>rn <cmd>lua vim.lsp.buf.rename()<cr>
	nnoremap <leader>ca <cmd>lua vim.lsp.buf.code_action()<cr>

	nnoremap <silent> <C-k> :lua vim.lsp.diagnostic.goto_prev()<CR>
	nnoremap <silent> <C-j> :lua vim.lsp.diagnostic.goto_next()<CR>

	" }}}
	"{{{ autocommands

	augroup lsp
		au!

		" au BufEnter * lua require('completion').on_attach()
		" au BufEnter * lua require('lint').linters_by_ft = {
		" 			\ vim = {'vint'}, sh = {'shellcheck'}
		" 			\ }

		" au BufWritePost * lua require('lint').try_lint()
		" au FileType java call CurrentGitBranch()
		" au FileType java lua require('local-java-lsp')
	augroup end

	"}}
	" }}}

	" }}}

call plug#end()

" {{{ lua config

lua <<EOF
	local cmp = require'cmp'

	cmp.setup({
		snippet = {
			expand = function(args)
				vim.fn["UltiSnips#Anon"](args.body)
			end,
		},
		mapping = {
			['<C-d>'] = cmp.mapping.scroll_docs(-4),
			['<C-f>'] = cmp.mapping.scroll_docs(4),
			['<C-Space>'] = cmp.mapping.complete(),
			['<C-e>'] = cmp.mapping.close(),
		},
		sources = {
			{ name = 'nvim_lsp' },
			{ name = 'ultisnips' },
			{ name = 'buffer' },
			{ name = 'path' },
		}
	})

	local capabilities = require('cmp_nvim_lsp')
		.update_capabilities(vim.lsp.protocol.make_client_capabilities())

	require('lspconfig').terraformls.setup({ capabilities = capabilities })
	require('lspconfig').pyright.setup({ capabilities = capabilities })
	require('lspconfig').groovyls.setup({ capabilities = capabilities })
	-- require('lspconfig').jdtls.setup({ capabilities = capabilities })
	-- require('lspconfig').java_language_server.setup({
	-- 	cmd = {'java-language-server'},
	-- 	capabilities = capabilities
	-- })

EOF

	" }}}

" }}}
" {{{ mappings
" close/open current fold
nnoremap <Space> za

" going to the first window (usually NERDTree)
nnoremap <C-t> 1<C-w>w
inoremap <C-t> 1<C-w>w

" exit the terminal window with Esc
tnoremap <Esc> <C-\><C-n>

" swap backtick and apostrophe because of stupid keyboard layouts
noremap ' `
noremap ` '

" convenient mappings from the fzf plugin TODO: move these?
nnoremap <C-f> <cmd>Files<cr>
nnoremap <C-l> <cmd>Lines<cr>
" }}}

" {{{ some functions
function! CurrentGitBranch()
	let output = system(
				\ 'git status --porcelain -b ' . shellescape(expand('%')) . ' 2>/dev/null'
				\. '|tr . '' '' | sed 1q | awk ''{print $2}'' | tr -d ''\n''')
	if len(output) > 0
		let b:gitbranch = '  ' . l:output . ' '
	else
		let b:gitbranch = ''
	endif
endfunction

function! s:IsAnEmptyListItem()
	return getline('.') =~ '\v^\s*%([-*+]|\d\.)\s*$'
endfunction

function! s:IsAnEmptyQuote()
	return getline('.') =~ '\v^\s*(\s?\>)+\s*$'
endfunction

function! s:Indent(indent)
	if getline('.') =~ '\v^\s*%([-*+]|\d\.)\s*$'
		if a:indent
			normal >>
		else
			normal <<
		endif
		call setline('.', substitute(getline('.'), '\([-*+]\|\d\.\)\s*$', '\1 ', ''))
		normal $
	elseif getline('.') =~ '\v^\s*(\s?\>)+\s*$'
		if a:indent
			call setline('.', substitute(getline('.'), '>\s*$', '>> ', ''))
		else
			call setline('.', substitute(getline('.'), '\s*>\s*$', ' ', ''))
			call setline('.', substitute(getline('.'), '^\s\+$', '', ''))
		endif
		normal $
	endif
endfunction
" }}}
" {{{ the status line
set statusline=
set statusline+=%#PmenuSel#
set statusline+=%{b:gitbranch}
set statusline+=%#FileName#
set statusline+=\ %<%f\ %m\ %=
set statusline+=%#PmenuSel#
set statusline+=\ %y\ %6(L%l%)\ %-6(C%v%)\ %P\ %{''}
" }}}

augroup neomutt "{{{
	au!
	au BufRead /tmp/neomutt-* set tw=72 noautoindent filetype=mail
	au BufRead /tmp/neomutt-* DisableStripWhitespaceOnSave
augroup END "}}}
augroup haskell "{{{
	au!
	au FileType haskell set ts=2 sw=2 et
augroup END "}}}
augroup racket "{{{
	au!
	au FileType scheme set ts=2 sw=2 et
augroup END "}}}
augroup markdown "{{{
	au!
	au FileType markdown set foldlevelstart=99 foldlevel=3
	au FileType markdown set ts=2 sw=2 et
augroup END "}}}

augroup common " {{{
	au BufWritePost * if &makeprg != 'make' | make | endif
	au BufEnter,BufWritePost * call CurrentGitBranch()
	au VimLeave * set guicursor=a:hor20
	au ColorScheme * hi CursorLine cterm=none ctermbg=0
augroup END " }}}

" vim: set ts=2 sw=2 noet foldmethod=marker :
