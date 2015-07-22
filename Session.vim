let SessionLoad = 1
if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <silent> <Plug>(table-mode-tableize) |:call tablemode#TableizeInsertMode()a
map <NL> j_
map  k_
nmap o <Plug>ZoomWin
nnoremap k k
vmap k zk
omap k zk
map j zj
xmap ,T <Plug>(table-mode-tableize-delimiter)
xmap ,tt <Plug>(table-mode-tableize)
nmap ,tt <Plug>(table-mode-tableize)
nnoremap <silent> ,tm :call tablemode#Toggle()
nnoremap ,fc :hi CursorLine cterm=none ctermbg=234 guibg=#202020 guifg=none
nnoremap ,h :set cursorline!
nmap ,gq :tabclose!
nmap ,gd :tabedit %:Gdiff
map , :Ex
nnoremap ,m :match
nnoremap <silent> ,l ml:execute 'match Search /\%'.line('.').'l/'
nnoremap ,E :set fo+=a
nnoremap ,e :set fo-=a
map ,ss :s/\s\+$//
map ,ws :call HighlightTrailingWhitespace()
nmap ,sy :SignifyToggle
vmap ,u :call UnCommentLines()
vmap ,c :call CommentLines()
vmap ,vs y:@"
nmap ,vs :source %
nmap ,2 yypVr-
nmap ,1 yypVr=
map ,z :ZoomWin
map ,Q :qa
map ,q :q
map ,wq :wq
map ,w :w
map ,n :set nu!
xmap S <Plug>VSurround
nmap [c <Plug>(signify-prev-hunk)
map \, :emenu Slimv.	
noremap <silent> \- :call SlimvClearReplBuffer()
noremap <silent> \y :call SlimvInterrupt()
noremap <silent> \g :call SlimvSetPackage()
noremap <silent> \] :call SlimvGenerateTags()
noremap <silent> \h :call SlimvHyperspec()
noremap <silent> \A :call SlimvApropos()
noremap <silent> \s :call SlimvDescribeSymbol()
noremap <silent> \X :call SlimvProfileReset()
noremap <silent> \o :call SlimvProfileReport()
noremap <silent> \? :call SlimvShowProfiled()
noremap <silent> \U :call SlimvUnprofileAll()
noremap <silent> \P :call SlimvProfileSubstring()
noremap <silent> \xe :call SlimvXrefCallees()
noremap <silent> \xl :call SlimvXrefCallers()
noremap <silent> \xp :call SlimvXrefSpecializes()
noremap <silent> \xm :call SlimvXrefMacroexpands()
noremap <silent> \xb :call SlimvXrefBinds()
noremap <silent> \xs :call SlimvXrefSets()
noremap <silent> \xr :call SlimvXrefReferences()
noremap <silent> \xc :call SlimvXrefCalls()
noremap <silent> \R :call SlimvCompileRegion()
noremap <silent> \F :call SlimvCompileFile()
noremap <silent> \L :call SlimvCompileLoadFile()
noremap <silent> \D :call SlimvCompileDefun()
noremap <silent> \G :call SlimvDebugThread()
noremap <silent> \K :call SlimvKillThread()
noremap <silent> \H :call SlimvListThreads()
noremap <silent> \N :call SlimvDebugRestartFrame()
noremap <silent> \q :call SlimvDebugQuit()
noremap <silent> \a :call SlimvDebugAbort()
noremap <silent> \i :call SlimvInspect()
noremap <silent> \l :call SlimvDisassemble()
noremap <silent> \E :call SlimvBreakOnException()
noremap <silent> \B :call SlimvBreak()
noremap <silent> \T :call SlimvUntrace()
noremap <silent> \t :call SlimvTrace()
noremap <silent> \m :call SlimvMacroexpandAll()
noremap <silent> \1 :call SlimvMacroexpand()
noremap <silent> \u :call SlimvUndefineFunction()
noremap <silent> \v :call SlimvInteractiveEval()
noremap <silent> \b :call SlimvEvalBuffer()
noremap <silent> \r :call SlimvEvalRegion()
noremap <silent> \( :call PareditToggle()
noremap <silent> \) :call SlimvCloseForm()
noremap <silent> \p :call SlimvProfile()
noremap <silent> \n :call SlimvDebugContinue()
nmap \\u <Plug>CommentaryUndo:echomsg '\\ is deprecated. Use gc'
nmap \\\ <Plug>CommentaryLine:echomsg '\\ is deprecated. Use gc'
nmap \\ :echomsg '\\ is deprecated. Use gc'<Plug>Commentary
xmap \\ <Plug>Commentary:echomsg '\\ is deprecated. Use gc'
noremap <silent> \d :call SlimvEvalDefun()
noremap <silent> \c :call SlimvConnectServer()
noremap <silent> \e :call SlimvEvalExp()
nmap ]c <Plug>(signify-next-hunk)
nmap cS <Plug>CSurround
nmap cs <Plug>Csurround
nmap cgc <Plug>ChangeCommentary
nmap ds <Plug>Dsurround
xmap gS <Plug>VgSurround
nmap gx <Plug>NetrwBrowseX
nmap gcu <Plug>Commentary<Plug>Commentary
nmap gcc <Plug>CommentaryLine
omap gc <Plug>Commentary
nmap gc <Plug>Commentary
xmap gc <Plug>Commentary
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
vnoremap | :!column -t
nnoremap <silent> <Plug>(table-mode-sort) :call tablemode#spreadsheet#Sort()
nnoremap <silent> <Plug>(table-mode-eval-formula) :call tablemode#spreadsheet#formula#EvaluateFormulaLine()
nnoremap <silent> <Plug>(table-mode-add-formula) :call tablemode#spreadsheet#formula#Add()
nnoremap <silent> <Plug>(table-mode-delete-column) :call tablemode#spreadsheet#DeleteColumn()
nnoremap <silent> <Plug>(table-mode-delete-row) :call tablemode#spreadsheet#DeleteRow()
xnoremap <silent> <Plug>(table-mode-cell-text-object-i) :call tablemode#spreadsheet#cell#TextObject(1)
xnoremap <silent> <Plug>(table-mode-cell-text-object-a) :call tablemode#spreadsheet#cell#TextObject(0)
onoremap <silent> <Plug>(table-mode-cell-text-object-i) :call tablemode#spreadsheet#cell#TextObject(1)
onoremap <silent> <Plug>(table-mode-cell-text-object-a) :call tablemode#spreadsheet#cell#TextObject(0)
nnoremap <silent> <Plug>(table-mode-motion-right) :call tablemode#spreadsheet#cell#Motion('l')
nnoremap <silent> <Plug>(table-mode-motion-left) :call tablemode#spreadsheet#cell#Motion('h')
nnoremap <silent> <Plug>(table-mode-motion-down) :call tablemode#spreadsheet#cell#Motion('j')
nnoremap <silent> <Plug>(table-mode-motion-up) :call tablemode#spreadsheet#cell#Motion('k')
nnoremap <silent> <Plug>(table-mode-realign) :call tablemode#table#Realign('.')
xnoremap <silent> <Plug>(table-mode-tableize-delimiter) :call tablemode#TableizeByDelimiter()
xnoremap <silent> <Plug>(table-mode-tableize) :Tableize
nnoremap <silent> <Plug>(table-mode-tableize) :Tableize
nnoremap <silent> <Plug>SurroundRepeat .
xnoremap <silent> <Plug>(signify-motion-outer-visual) :call sy#util#hunk_text_object(1)
onoremap <silent> <Plug>(signify-motion-outer-pending) :call sy#util#hunk_text_object(1)
xnoremap <silent> <Plug>(signify-motion-inner-visual) :call sy#util#hunk_text_object(0)
onoremap <silent> <Plug>(signify-motion-inner-pending) :call sy#util#hunk_text_object(0)
nnoremap <silent> <expr> <Plug>(signify-prev-hunk) &diff ? '[c' : ":\call sy#jump#prev_hunk(v:count1)\"
nnoremap <silent> <expr> <Plug>(signify-next-hunk) &diff ? ']c' : ":\call sy#jump#next_hunk(v:count1)\"
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cfile>"),0)
nnoremap <Plug>FireplaceSource :Source 
nmap <silent> <Plug>CommentaryUndo <Plug>Commentary<Plug>Commentary
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"
nnoremap <silent> <F9> :TagbarToggle
map <F5> :call RunCsound()
cmap  <Home>
cmap  <Left>
cnoremap  <Del>
cmap  <End>
cmap  <Right>
imap S <Plug>ISurround
imap s <Plug>Isurround
cnoremap  <BS>
cnoremap  D:<Up>
cmap  <Down>
cmap  <Up>
imap  <Plug>Isurround
cnoremap b b
cnoremap f e
cmap w!! w !sudo tee >/dev/null %
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set backspace=indent,start,eol
set backupdir=/tmp/jlucas/vim
set directory=/tmp/jlucas/vim
set fileencodings=
set helplang=en
set hidden
set ignorecase
set laststatus=2
set mouse=a
set nrformats=hex
set omnifunc=SlimvOmniComplete
set pastetoggle=<F3>
set printoptions=paper:letter
set ruler
set runtimepath=~/.vim,~/.vim/bundle/bufexplorer,~/.vim/bundle/clojure-static,~/.vim/bundle/color-edark,~/.vim/bundle/color-solarized,~/.vim/bundle/color-xterm16,~/.vim/bundle/colorsupport,~/.vim/bundle/commentary,~/.vim/bundle/csound,~/.vim/bundle/ctrlp,~/.vim/bundle/dispatch,~/.vim/bundle/fireplace,~/.vim/bundle/markdown,~/.vim/bundle/markdown-folding,~/.vim/bundle/nerdtree,~/.vim/bundle/netrw,~/.vim/bundle/scrollcolors,~/.vim/bundle/signify,~/.vim/bundle/simpylfold,~/.vim/bundle/slimv,~/.vim/bundle/surround,~/.vim/bundle/syntax-ini,~/.vim/bundle/table-mode,~/.vim/bundle/tagbar,~/.vim/bundle/vim-fugitive,~/.vim/bundle/vim-orgmode,~/.vim/bundle/vim-slime,~/.vim/bundle/zoomwin,~/.vim/bundle/slimv.bak,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim73,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/bundle/markdown-folding/after,~/.vim/after
set scrolloff=2
set shiftwidth=4
set shortmess=lnrxIT
set smartcase
set softtabstop=4
set splitright
set statusline=%t\ [%{strlen(&fenc)?&fenc:'none'},%{&ff}]%h\ %y\ %{fugitive#statusline()}%r\ %m%=%c,%l/%L\ %P
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tabstop=4
set tags=./tags;/
set wildcharm=<Tab>
set winminheight=0
set winminwidth=0
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd ~/.emacs.d
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +1 init.el
badd +0 user.el
badd +0 ~/.vimrc
args init.el user.el ~/.vimrc
edit init.el
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd _ | wincmd |
split
1wincmd k
wincmd w
set nosplitbelow
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe '2resize ' . ((&lines * 31 + 32) / 65)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
exe '3resize ' . ((&lines * 31 + 32) / 65)
exe 'vert 3resize ' . ((&columns * 119 + 119) / 239)
argglobal
let s:cpo_save=&cpo
set cpo&vim
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
inoremap <buffer> <silent> <S-Tab> =pumvisible() ? "\<C-P>" : "\<S-Tab>"
vnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
vnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()|silent! call repeat#set(",S")
vnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")|silent! call repeat#set(",W")
nnoremap <buffer> <silent> ,J :call PareditJoin()|silent! call repeat#set(",J")
nnoremap <buffer> <silent> ,O :call PareditSplit()|silent! call repeat#set(",O")
nnoremap <buffer> <silent> ,> :call PareditMoveRight()|silent! call repeat#set(",>")
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()|silent! call repeat#set(",\<")
nnoremap <buffer> <silent> ,I :call PareditRaise()|silent! call repeat#set(",I")
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
vnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')|silent! call repeat#set(",w\"")
vnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")|silent! call repeat#set(",w(")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)|silent! call repeat#set("D")
nnoremap <buffer> <silent> P :call PareditPut("P")|silent! call repeat#set("P")
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()|silent! call repeat#set("X")
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
vnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> dd :call PareditDeleteLines()|silent! call repeat#set("dd")
vnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut("p")|silent! call repeat#set("p")
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
vnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()|silent! call repeat#set("x")
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
inoremap <buffer> <silent> 	 =SlimvHandleTab()
inoremap <buffer> <silent>  =pumvisible() ?  "\<C-Y>" : SlimvHandleEnter()=SlimvArglistOnEnter()
inoremap <buffer> <silent> 0 :call SlimvCloseForm()
inoremap <buffer> <silent>    =SlimvArglist()
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=SlimvDescribe(v:beval_text)
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
set cursorline
setlocal cursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=2
setlocal imsearch=2
setlocal include=
setlocal includeexpr=
setlocal indentexpr=SlimvIndent(v:lnum)
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=hex
setlocal nonumber
setlocal numberwidth=4
setlocal omnifunc=SlimvOmniComplete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=4
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 31) / 63)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
edit user.el
let s:cpo_save=&cpo
set cpo&vim
inoremap <buffer> <expr> <Del> PareditDel()
inoremap <buffer> <expr> <BS> PareditBackspace(0)
inoremap <buffer> <silent> <S-Tab> =pumvisible() ? "\<C-P>" : "\<S-Tab>"
vnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',1)
nnoremap <buffer> <silent> ( :call PareditFindOpening('(',')',0)
vnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',1)
nnoremap <buffer> <silent> ) :call PareditFindClosing('(',')',0)
nnoremap <buffer> <silent> ,S :call PareditSplice()|silent! call repeat#set(",S")
vnoremap <buffer> <silent> ,W :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,W :call PareditWrap("(",")")|silent! call repeat#set(",W")
nnoremap <buffer> <silent> ,J :call PareditJoin()|silent! call repeat#set(",J")
nnoremap <buffer> <silent> ,O :call PareditSplit()|silent! call repeat#set(",O")
nnoremap <buffer> <silent> ,> :call PareditMoveRight()|silent! call repeat#set(",>")
nnoremap <buffer> <silent> ,< :call PareditMoveLeft()|silent! call repeat#set(",\<")
nnoremap <buffer> <silent> ,I :call PareditRaise()|silent! call repeat#set(",I")
nmap <buffer> <silent> ,<Down> d])%,S
nmap <buffer> <silent> ,<Up> d[(,S
vnoremap <buffer> <silent> ,w" :call PareditWrapSelection('"','"')
nnoremap <buffer> <silent> ,w" :call PareditWrap('"','"')|silent! call repeat#set(",w\"")
vnoremap <buffer> <silent> ,w( :call PareditWrapSelection("(",")")
nnoremap <buffer> <silent> ,w( :call PareditWrap("(",")")|silent! call repeat#set(",w(")
nnoremap <buffer> <silent> C v$:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> D v$:call PareditDelete(visualmode(),1)|silent! call repeat#set("D")
nnoremap <buffer> <silent> P :call PareditPut("P")|silent! call repeat#set("P")
nnoremap <buffer> <silent> S V:call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> X :call PareditEraseBck()|silent! call repeat#set("X")
nnoremap <buffer> <silent> [[ :call PareditFindDefunBck()
nnoremap <buffer> <silent> ]] :call PareditFindDefunFwd()
nnoremap <buffer> <silent> caw :call PareditChangeSpec('caw',1)
nnoremap <buffer> <silent> ciw :call PareditChangeSpec('ciw',1)
nnoremap <buffer> <silent> cb :call PareditChangeSpec('cb',0)
nnoremap <buffer> <silent> cW :set opfunc=PareditChangeg@E
nnoremap <buffer> <silent> cw :call PareditChangeSpec('cw',1)
nnoremap <buffer> <silent> cc :call PareditChangeLines()
vnoremap <buffer> <silent> c :call PareditChange(visualmode(),1)
nnoremap <buffer> <silent> c :set opfunc=PareditChangeg@
nnoremap <buffer> <silent> dd :call PareditDeleteLines()|silent! call repeat#set("dd")
vnoremap <buffer> <silent> d :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> d :call PareditSetDelete(v:count)g@
nnoremap <buffer> <silent> p :call PareditPut("p")|silent! call repeat#set("p")
nnoremap <buffer> <silent> s :call PareditEraseFwd()i
vnoremap <buffer> <silent> x :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> x :call PareditEraseFwd()|silent! call repeat#set("x")
vnoremap <buffer> <silent> <Del> :call PareditDelete(visualmode(),1)
nnoremap <buffer> <silent> <Del> :call PareditEraseFwd()
inoremap <buffer> <silent> 	 =SlimvHandleTab()
inoremap <buffer> <silent>  =pumvisible() ?  "\<C-Y>" : SlimvHandleEnter()=SlimvArglistOnEnter()
inoremap <buffer> <silent> 0 :call SlimvCloseForm()
inoremap <buffer> <silent>    =SlimvArglist()
inoremap <buffer> <expr> " PareditInsertQuotes()
inoremap <buffer> <expr> ( PareditInsertOpening('(',')')
inoremap <buffer> <silent> ) =(pumvisible() ? "\<C-Y>" : ""):let save_ve=&ve:set ve=all:call PareditInsertClosing('(',')'):let &ve=save_ve
let &cpo=s:cpo_save
unlet s:cpo_save
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=SlimvDescribe(v:beval_text)
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=:;;;,:;;,sr:#|,mb:|,ex:|#,:;
setlocal commentstring=/*%s*/
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
set cursorline
setlocal cursorline
setlocal define=^\\s*(def\\k*
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'lisp'
setlocal filetype=lisp
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=2
setlocal imsearch=2
setlocal include=
setlocal includeexpr=
setlocal indentexpr=SlimvIndent(v:lnum)
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e
setlocal noinfercase
setlocal iskeyword=38,42,43,45,47-58,60-62,64-90,97-122,_,+,-,*,/,%,<,=,>,:,$,?,!,@-@,94
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=hex
setlocal nonumber
setlocal numberwidth=4
setlocal omnifunc=SlimvOmniComplete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=4
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'lisp'
setlocal syntax=lisp
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=0
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 1 - ((0 * winheight(0) + 15) / 31)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1
normal! 0
wincmd w
argglobal
edit ~/.vimrc
vnoremap <buffer> <silent> [" :exe "normal! gv"|call search('\%(^\s*".*\n\)\%(^\s*"\)\@!', "bW")
nnoremap <buffer> <silent> [" :call search('\%(^\s*".*\n\)\%(^\s*"\)\@!', "bW")
vnoremap <buffer> <silent> [] m':exe "normal! gv"|call search('^\s*endf*\%[unction]\>', "bW")
nnoremap <buffer> <silent> [] m':call search('^\s*endf*\%[unction]\>', "bW")
vnoremap <buffer> <silent> [[ m':exe "normal! gv"|call search('^\s*fu\%[nction]\>', "bW")
nnoremap <buffer> <silent> [[ m':call search('^\s*fu\%[nction]\>', "bW")
vnoremap <buffer> <silent> ]" :exe "normal! gv"|call search('^\(\s*".*\n\)\@<!\(\s*"\)', "W")
nnoremap <buffer> <silent> ]" :call search('^\(\s*".*\n\)\@<!\(\s*"\)', "W")
vnoremap <buffer> <silent> ][ m':exe "normal! gv"|call search('^\s*endf*\%[unction]\>', "W")
nnoremap <buffer> <silent> ][ m':call search('^\s*endf*\%[unction]\>', "W")
vnoremap <buffer> <silent> ]] m':exe "normal! gv"|call search('^\s*fu\%[nction]\>', "W")
nnoremap <buffer> <silent> ]] m':call search('^\s*fu\%[nction]\>', "W")
setlocal keymap=
setlocal noarabic
setlocal autoindent
setlocal balloonexpr=
setlocal nobinary
setlocal bufhidden=
setlocal buflisted
setlocal buftype=
setlocal nocindent
setlocal cinkeys=0{,0},0),:,0#,!^F,o,O,e
setlocal cinoptions=
setlocal cinwords=if,else,while,do,for,switch
setlocal colorcolumn=
setlocal comments=sO:\"\ -,mO:\"\ \ ,eO:\"\",:\"
setlocal commentstring=\"%s
setlocal complete=.,w,b,u,t,i
setlocal concealcursor=
setlocal conceallevel=0
setlocal completefunc=
setlocal nocopyindent
setlocal cryptmethod=
setlocal nocursorbind
setlocal nocursorcolumn
set cursorline
setlocal cursorline
setlocal define=
setlocal dictionary=
setlocal nodiff
setlocal equalprg=
setlocal errorformat=
setlocal expandtab
if &filetype != 'vim'
setlocal filetype=vim
endif
setlocal foldcolumn=0
setlocal foldenable
setlocal foldexpr=0
setlocal foldignore=#
setlocal foldlevel=0
setlocal foldmarker={{{,}}}
setlocal foldmethod=manual
setlocal foldminlines=1
setlocal foldnestmax=20
setlocal foldtext=foldtext()
setlocal formatexpr=
setlocal formatoptions=croql
setlocal formatlistpat=^\\s*\\d\\+[\\]:.)}\\t\ ]\\s*
setlocal grepprg=
setlocal iminsert=2
setlocal imsearch=2
setlocal include=
setlocal includeexpr=
setlocal indentexpr=GetVimIndent()
setlocal indentkeys=0{,0},:,0#,!^F,o,O,e,=end,=else,=cat,=fina,=END,0\\
setlocal noinfercase
setlocal iskeyword=@,48-57,_,192-255,:,#
setlocal keywordprg=
setlocal nolinebreak
setlocal nolisp
setlocal nolist
setlocal makeprg=
setlocal matchpairs=(:),{:},[:]
setlocal modeline
setlocal modifiable
setlocal nrformats=hex
setlocal nonumber
setlocal numberwidth=4
setlocal omnifunc=SlimvOmniComplete
setlocal path=
setlocal nopreserveindent
setlocal nopreviewwindow
setlocal quoteescape=\\
setlocal noreadonly
setlocal norelativenumber
setlocal norightleft
setlocal rightleftcmd=search
setlocal noscrollbind
setlocal shiftwidth=4
setlocal noshortname
setlocal nosmartindent
setlocal softtabstop=4
setlocal nospell
setlocal spellcapcheck=[.?!]\\_[\\])'\"\	\ ]\\+
setlocal spellfile=
setlocal spelllang=en
setlocal statusline=
setlocal suffixesadd=
setlocal swapfile
setlocal synmaxcol=3000
if &syntax != 'vim'
setlocal syntax=vim
endif
setlocal tabstop=4
setlocal tags=
setlocal textwidth=78
setlocal thesaurus=
setlocal noundofile
setlocal nowinfixheight
setlocal nowinfixwidth
setlocal wrap
setlocal wrapmargin=0
silent! normal! zE
let s:l = 122 - ((14 * winheight(0) + 15) / 31)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
122
normal! 017l
wincmd w
3wincmd w
exe 'vert 1resize ' . ((&columns * 119 + 119) / 239)
exe '2resize ' . ((&lines * 31 + 32) / 65)
exe 'vert 2resize ' . ((&columns * 119 + 119) / 239)
exe '3resize ' . ((&lines * 31 + 32) / 65)
exe 'vert 3resize ' . ((&columns * 119 + 119) / 239)
tabnext 1
if exists('s:wipebuf')
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=lnrxIT
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
