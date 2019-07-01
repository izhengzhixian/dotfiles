" 当前配置分为简单版与插件版，当存在~/.vim/bundle/Vundle.vim时，会自动加载插件版
" 简单版只需要把本文件放在家目录下即可
" 下面是插件版运行方式：
" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" 然后进入vim，执行VundleInstall即可

" 以下是我们自己的配置
" 需要额外安装的主题，如果不是下面的几个，请自己加入plugin源
let my_plugin_colorscheme = "molokai"
"let my_plugin_colorscheme = "monokai"
"let my_plugin_colorscheme = "solarized"
" vim 自带的主题，当plugin主题不存在时使用
let my_self_colorscheme = "slate"
"let my_self_colorscheme = "desert"
" 插件模式开启，会自动检测vundle，不需要请设置为0
let my_plugin_mode = 1
" 插入模式用emacs的快捷键，不需要请设置为0
let my_emacs_hotkey = 1
" 新建文件默认保存编码，不需要可以为空，可以是gb18030 utf-8
let my_fileencoding = ""
" 保存时删除行尾空格，可在strip_traling_spaces设置删除时的文件类型
let save_delete_trailing_space = 1

" leader按键
let mapleader=","
"let mapleader="\<Space>"
nmap <Space> <leader>
set fileencodings=ucs-bom,utf-8,default,gb18030,latin1

function! ModifyFileencoding()
    let first_encoding = "ucs-bom"
    if exists("g:my_fileencoding") && g:my_fileencoding != ""
        let src_encoding = split(&fileencodings, ",")
        if len(src_encoding) > 0 && src_encoding[0] == first_encoding
            if g:my_fileencoding == first_encoding
                return
            endif
            let target_encoding = [first_encoding, g:my_fileencoding]
            let src_encoding = src_encoding[1:]
        else
            let target_encoding = [g:my_fileencoding]
        endif
        for enc in src_encoding
            if enc != g:my_fileencoding
                call add(target_encoding, enc)
            endif
        endfor
        let &fileencodings = join(target_encoding, ",")
    endif 
endfunction

call ModifyFileencoding()
 
let vundle_path = '~/.vim/bundle/Vundle.vim'
if exists("my_plugin_mode") && my_plugin_mode && !empty(glob(vundle_path))
    " 你在此设置运行时路径 
    set rtp+=~/.vim/bundle/Vundle.vim  
    filetype off                " 关闭文件检测，这是vundle这是必需的 

    " vundle初始化 
    call vundle#begin()  
     
    " 这应该始终是第一个 
    Plugin 'gmarik/Vundle.vim' 

    " 用户插件

    " 主题安装
    Plugin 'sickill/vim-monokai'
    Plugin 'altercation/vim-colors-solarized'
    Plugin 'tomasr/molokai'

    Plugin 'A.vim'
    "头/源文件切换命令
    ":A 头文件／源文件切换
    ":AS 分割窗后并切换头/源文件(切割为上下两个窗口)
    ":AV 垂直切割窗口后切换头/源文件(切割为左右两个窗口)
    ":AT 新建Vim标签式窗口后切换
    ":AN 在多个匹配文件间循环切换
    "将光标所在处单词作为文件名打开
    ":IH 切换至光标所在文件
    ":IHS 分割窗口后切换至光标所在文件(指将光标所在处单词作为文件名打开)
    ":IHV 垂直分割窗口后切换
    ":IHT 新建标签式窗口后切换
    ":IHN 在多个匹配文件间循环切换
    "快捷键操作
    "<Leader>ih 切换至光标所在文件*
    "<Leader>is 切换至光标所在处(单词所指)文件的配对文件(如光标所在处为foo.h，则切换至foo.c/foo.cpp...)
    "<Leader>ihn 在多个匹配文件间循环切换
    "*<Leader>指Vim所定义的映射(map)前缀，在Vim配置文件中可过变量'mapleader'进行设置，缺省为'\'。

    "" A插件
    "nmap <leader>aa :A
    "nmap <leader>as :AS
    "nmap <leader>av :AV
    "nmap <leader>at :AT
    "nmap <leader>hh :IHH
    "nmap <leader>hs :IHS
    "nmap <leader>hv :IHV
    "nmap <leader>ht :IHT
    "nmap <leader>hn :IHN


    Plugin 'visual-mark'
    map <silent><F3> <Plug>Vm_goto_prev_sign
    map <silent>mn <Plug>Vm_goto_next_sign
    map <silent>mp <Plug>Vm_goto_prev_sign


    Plugin 'L9'
    Plugin 'snipMate'
    "Plugin 'AutoComplPop'
    "let g:acp_completeOption = '.,w,b,u,t,i'
    "let g:acp_behaviorKeywordCommand = "\<C-x>\<C-o>"
    "let g:LookupFile_AlwaysAcceptFirst = 1 "回车打开第一个匹配项目


    Plugin 'fholgado/minibufexpl.vim'
    let g:miniBufExplorerMoreThanOne=1
    " MiniBufExpl Colors
    hi MBENormal               guifg=#808080 guibg=fg
    hi MBEChanged              guifg=#CD5907 guibg=fg
    hi MBEVisibleNormal        guifg=#5DC2D6 guibg=fg
    hi MBEVisibleChanged       guifg=#F1266F guibg=fg
    hi MBEVisibleActiveNormal  guifg=#A6DB29 guibg=fg
    hi MBEVisibleActiveChanged guifg=#F1266F guibg=fg
    nmap <silent><leader>bn :MBEbn<CR>
    nmap <silent><leader>bp :MBEbp<CR>
    nmap <silent><leader>bt :MBEToggle<CR>

    Plugin 'suan/vim-instant-markdown'

    " 快速注释
    Plugin 'scrooloose/nerdcommenter'
    "使用：
    "1、 \cc 注释当前行和选中行
    "2、 \cn 没有发现和\cc有区别
    "3、 \c<空格> 如果被选区域有部分被注释，则对被选区域执行取消注释操作，其它情况执行反转注释操作
    "4、 \cm 对被选区域用一对注释符进行注释，前面的注释对每一行都会添加注释
    "5、 \ci 执行反转注释操作，选中区域注释部分取消注释，非注释部分添加注释
    "6、 \cs 添加性感的注释，代码开头介绍部分通常使用该注释
    "7、 \cy 添加注释，并复制被添加注释的部分
    "8、 \c$ 注释当前光标到改行结尾的内容
    "9、 \cA 跳转到该行结尾添加注释，并进入编辑模式
    "10、\ca 转换注释的方式，比如： /**/和//
    "11、\cl \cb 左对齐和左右对其，左右对其主要针对/**/
    "12、\cu 取消注释

    "pip install twisted argparse service_identity
    "Plugin 'FredKSchott/CoVim'
    "let Covim_default_name=$USER
    "let Covim_default_port=12345


    Plugin 'scrooloose/nerdtree'
    let NERDTreeWinPos='left'
    map <silent><leader>wd :NERDTreeToggle<CR>
           "▶ 文件节点映射（File node mappings）
           "• 左键双击 or 回车 or o : 打开指定文件。
           "• go: 打开指定文件，将光标留在目录树中。
           "• t : 在新标签中打开文件。
           "• T : 在新标签中打开文件，保持鼠标焦点留在当前标签。
           "• 鼠标中键 or i : 在水平分屏窗口中打开指定文件。
           "• gi : 在水平分屏窗口中打开指定文件，将光标留在目录树中。
           "• s  : 在垂直分屏窗口中打开指定文件。
           "• gs : 在垂直分屏窗口中打开指定文件，将光标留在目录树中。


           "▶ 目录节点映射（Directory node mappings）
           "• 左键双击 or 回车 or o : 打开指定目录。
           "• O  : 递归打开指定目录。
           "• x  : 关闭当前节点的父节点。
           "• X  : 递归关闭当前节点的所有子节点。
           "• 鼠标中键 or e  : 浏览指定目录。


           "▶ 书签表映射（Bookmark table mappings）
           "• 左键双击 or 回车 or o : 打开指定书签。
           "• t    : 在新标签中打开书签。
           "• T    : 在新标签中打开书签，保持鼠标焦点留在当前标签。
           "• D    : 删除指定书签。


           "▶ 树形导航映射（Tree navigation mappings）
           "• p    : 跳转到根节点。
           "• P    : 跳转到当前节点的父节点。
           "• K    : 跳转到当前目录的第一个子节点。
           "• J    : 跳转到当前目录的最后一个子节点。
           "• Ctrl + K  : 跳转到当前节点的上一个兄弟节点。
           "• Ctrl + J  : 跳转到当前节点的下一个兄弟节点。


           "▶ 文件系统映射（Filesystem mappings）
           "• C : 将当前选择的目录做为树形目录的根节点，即切换当前根目录节点为选择的目录节点。
           "• u : 将当前视图中的树根节点上移一层目录，即拿当前树根目录的父目录做为新的根目录。
           "• U : 将当前视图中的树根节点上移一层目录，即拿当前树根目录的父目录做为新的根目录，并且保持原树目录状态不变。
           "• r : 递归刷新当前目录。
           "• R : 递归刷新当前节点。
           "• m :  显示菜单。
           "• cd: 将CWD切换到当前选择节点的目录。


           "▶ 树形过滤器映射（Tree filtering mappings）
           "• I : 是否显示隐藏文件开关。
           "• f : 是否启用文件过滤器开关。
           "• F : 是否显示文件开关。
           "• B : 是否显示书签表的开关。


           "▶ 树形过滤器映射（Tree filtering mappings）
           "• q : 关闭树形目录树窗口。
           "• A : 缩放树形目录树窗口。
           "• ? : 显示帮助文档的开关。


    " 显示ctags产生的符号的插件
    Plugin 'taglist.vim'
    let Tlist_Show_One_File = 1            " 不同时显示多个文件的tag，只显示当前文件的
    let Tlist_Exit_OnlyWindow = 1          " 如果taglist窗口是最后一个窗口，则退出vim
    let Tlist_Use_Right_Window = 1         " 在右侧窗口中显示taglist窗口
    let Tlist_File_Fold_Auto_Close=1       " 自动折叠当前非编辑文件的方法列表
    let Tlist_Auto_Open = 0
    let Tlist_Auto_Update = 1
    let Tlist_Hightlight_Tag_On_BufEnter = 1
    let Tlist_Enable_Fold_Column = 0
    let Tlist_Process_File_Always = 1
    let Tlist_Display_Prototype = 0
    let Tlist_Compact_Format = 1
    map <silent><leader>wt :Tlist<CR>


    " 窗口管理，主要用于管理nerdtree和taglist
    Plugin 'winmanager'
    let g:winManagerWindowLayout='FileExplorer|TagList'
    nmap <silent><leader>wm :WMToggle<CR>



    Plugin 'vim-airline/vim-airline'
    Plugin 'vim-airline/vim-airline-themes'
    set t_Co=256        " 在windows中用xshell连接打开vim可以显示色彩
    let g:airline_powerline_fonts = 1
    let g:airline_theme ='molokai'
    let g:airline_section_y = '%{&encoding}/%{&fileformat} [asc=%03.3b] [hex=%02.2B]'
    if !exists('g:airline_symbols') 
       let g:airline_symbols = {} 
    endif 
    "enable tabline
    let g:airline#extensions#tabline#enabled = 1
    " enable/disable displaying tabs, regardless of numbe
    let g:airline#extensions#tabline#show_tabs = 1
    " 设置tag编号的类型
    " 0(splits,default),1(tab number),2(splits and tab numbers)
    let g:airline#extensions#tabline#tab_nr_type = 1
    " 显示tabpage编号
    let g:airline#extensions#tabline#show_tab_nr = 1
    let g:airline#extensions#tabline#show_tab_type = 1
    " enable/disable displaying buffers with a single tab
    let g:airline#extensions#tabline#show_buffer = 1
    " enable/disable 右上角显示tabpage中所有的窗口名字
    let g:airline#extensions#tabline#show_splits = 0
    let g:airline#extensions#tabline#key_map_ignored_filetypes = 
                \['vimfilter', 'nerdtree', 'taglist']
    " show buffer number
    "let g:airline#extensions#tabline#buffer_nr_show = 1
    let g:airline#extensions#tabline#buffer_idx_format = {
            \ '0': '[0] ',
            \ '1': '[1] ',
            \ '2': '[2] ',
            \ '3': '[3] ',
            \ '4': '[4] ',
            \ '5': '[5] ',
            \ '6': '[6] ',
            \ '7': '[7] ',
            \ '8': '[8] ',
            \ '9': '[9] '
            \}



    "let g:airline#extensions#tabline#buffer_idx_mode = 1
    "nmap <silent><leader>1 <Plug>AirlineSelectTab1
    "nmap <silent><leader>2 <Plug>AirlineSelectTab2
    "nmap <silent><leader>3 <Plug>AirlineSelectTab3
    "nmap <silent><leader>4 <Plug>AirlineSelectTab4
    "nmap <silent><leader>5 <Plug>AirlineSelectTab5
    "nmap <silent><leader>6 <Plug>AirlineSelectTab6
    "nmap <silent><leader>7 <Plug>AirlineSelectTab7
    "nmap <silent><leader>8 <Plug>AirlineSelectTab8
    "nmap <silent><leader>9 <Plug>AirlineSelectTab9
    "nmap <silent><leader>- <Plug>AirlineSelectPrevTab
    "nmap <silent><leader>= <Plug>AirlineSelectNextTab

    let g:airline#extensions#quickfix#quickfix_text='Quickfix'
    let g:airline#extensions#quickfix#location_text='Location'
    let g:airline#extensions#bufferline#enabled = 1
    let g:airline#extensions#bufferline#overwrite_variables = 1




    " 该例子来自https://github.com/gmarik/Vundle.vim README 
    "Plugin 'tpope/vim-fugitive'  



    " 来自http://vim-scripts.org/vim/scripts.html的插件 
    "Plugin 'L9'  

    "未托管在GitHub上的Git插件 
    "Plugin 'git://git.wincent.com/command-t.git'  

    "本地机器上的git软件库（即编写自己的插件时） 
    "Plugin 'file:///home/gmarik/path/to/plugin'  

    " sparkup vim脚本在名为vim的该软件库子目录下。 
    " 传递路径，合理设置运行时路径。 
    "Plugin 'rstacruz/sparkup', {'rtp': 'vim/'} 

    " 与L9避免名称冲突 
    "Plugin 'user/L9', {'name': 'newL9'}  

    "每个插件都应该在这一行之前  


    call vundle#end()            " required 

    filetype plugin indent on   " 开启插件

endif

" 判断colorscheme是否存在
function! HasColorscheme(name)
    let pat = 'colors/'.a:name.'.vim'
    return !empty(globpath(&rtp, pat))
endfunction


" 以下是不同的color主题

if HasColorscheme(my_plugin_colorscheme)
    execute 'colorscheme '.my_plugin_colorscheme
    "set background=light
endif

if !exists('g:colors_name')
    execute 'colorscheme '.my_self_colorscheme
endif

" 以下是基本配置

"设置中文提示
"language messages zh_CN.utf-8 
"设置中文帮助
set helplang=cn
set nocompatible            " 关闭兼容模式
syntax on                   " 自动语法高亮
set gcr=a:block-blinkon0    " 禁止GUI光标闪烁,terminal无效
"set autoread                " 设置当文件被改动时自动载入，但是是在检查的时候才会不询问自动加载
set showtabline=2           " 总是显示标签栏
set tabpagemax=18           " VIM默认只能打开10个标签页，在配置文件可以修改这个限制
"set relativenumber          " 相对行号
set number                  " 显示行号
set cursorline              " 突出显示当前行
set ruler                   " 打开状态栏标尺
set shiftwidth=4            " 设定 << 和 >> 命令移动时的宽度为 4
set softtabstop=4           " 使得按退格键时可以一次删掉 4 个空格
set tabstop=4               " 设定 tab 长度为 4
set expandtab               " 用空格取代tab
set mouse=a                 " 打开鼠标更改窗口宽度的功能
set nobackup                " 覆盖文件时不备份
"set autochdir               " 自动切换当前目录为当前文件所在的目录
set backupcopy=yes          " 设置备份时的行为为覆盖
set ignorecase smartcase    " 搜索时忽略大小写，但在有一个或以上大写字母时仍保持对大小写敏感
"set nowrap                  " 禁止自动换行，默认开启
"set nowrapscan              " 禁止在搜索到文件两端时重新搜索
set incsearch               " 输入搜索内容时就显示搜索结果
set hlsearch                " 搜索时高亮显示被找到的文本
set noerrorbells            " 关闭错误信息响铃
set novisualbell            " 关闭使用可视响铃代替呼叫
set t_vb=                   " 置空错误铃声的终端代码
"set showmatch               " 插入括号时，短暂地跳转到匹配的对应括号
"set matchtime=2             " 短暂跳转到匹配括号的时间
set magic                   " 设置魔术
set hidden                  " 允许在有未保存的修改时切换缓冲区，此时的修改由 vim 负责保存
set guioptions-=T           " 隐藏工具栏
set guioptions-=m           " 隐藏菜单栏
set guioptions-=r           " 隐藏滚动条

"" 设置补全显示
set completeopt=longest,menu
"set completeopt=menuone,menu,longest,preview
"set completeopt=menu,longest,preview
"set tags+=~/.vim/tags/systags
"set tags+=~/.vim/tags/gtktags

"自动缩进
set autoindent
"类似C语言风格的缩进
set cindent
"智能缩进:每一行都和前一行有相同的缩进量,
"同时这种缩进形式能正确的识别出花括号,当遇到右花括号（}）,
"则取消缩进形式。此外还增加了识别C语言关键字的功能。
"如果一行是以#开头的(比如宏)，那么这种格式将会被特殊对待而不采用缩进格式
set smartindent             " 开启新行时使用智能自动缩进
set backspace=indent,eol,start
                            " 不设定在插入状态无法用退格键和 Delete 键删除回车符
set cmdheight=1             " 设定命令行的行数为 1
set laststatus=2            " 显示状态栏 (默认值为 1, 无法显示状态栏)
" 设置 laststatus = 0 ，不显式状态行
" 设置 laststatus = 1 ，仅当窗口多于一个时，显示状态行
" 设置 laststatus = 2 ，总是显式状态行

set statusline=\ %<%F[%1*%M%*%n%R%H]%=\ %y\ %0(%{&fileformat}\ %{&encoding}\ %c:%l/%L%)\ 
"set statusline=%F%m%r%h%w%=\ [ft=%Y]\ 
"\%{\"[fenc=\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\"+\":\"\").\"]\"}
"\[ff=%{&ff}]\ [asc=%03.3b]\ [hex=%02.2B]\ [pos=%04l,%04v][%p%%]\ [len=%L]
"set statusline=\ %<%F[%1*%M%*%n%R%H]%=\ %y\ %0(%{&fileformat}\ %{&encoding}\ 
"\\ [asc=%03.3b]\ [hex=%02.2B]\ %c:%l/%L%)\ 
" 设置状态行显示常用信息
" %F 完整文件路径名
" %m 当前缓冲被修改标记
" %m 当前缓冲只读标记
" %h 帮助缓冲标记
" %w 预览缓冲标记
" %Y 文件类型
" %b ASCII值
" %B 十六进制值
" %l 行数
" %v 列数
" %p 当前行数占总行数的的百分比
" %L 总行数
" %{...} 评估表达式的值，并用值代替
" %{"[fenc=".(&fenc==""?&enc:&fenc).((exists("+bomb") && &bomb)?"+":"")."]"} 显示文件编码
" %{&ff} 显示文件类型

set foldenable              " 开始折叠
set foldmethod=marker       " 默认折叠方法
"set foldmethod=syntax       " 设置语法折叠
"set foldcolumn=0            " 设置折叠区域的宽度
"setlocal foldlevel=0        " 设置折叠层数为
set foldclose=all           " 设置为自动关闭折叠                            
" 打开javascript折叠
let b:javascript_fold=1
" 打开javascript对dom、html和css的支持
let javascript_enable_domhtmlcss=1
" nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc' : 'zo')<CR>
                            " 用空格键来开关折叠
"set cscopequickfix=s-,c-,d-,i-,t-,e-


" emacs快捷键
if my_emacs_hotkey
    inoremap <C-f> <right>
    inoremap <C-b> <left>
    inoremap <C-p> <up>
    inoremap <C-n> <down>
    inoremap <C-a> <Esc>^i
    inoremap <C-e> <End>
    nnoremap <C-j> i<CR><Esc>

    cnoremap <C-f> <right>
    cnoremap <C-b> <left>
    cnoremap <C-p> <up>
    cnoremap <C-n> <down>
    cnoremap <C-a> <Home>
    cnoremap <C-e> <End>
endif

inoremap <silent>( ()<left>
inoremap <silent>{ {}<left>

" 设置快捷键将选中文本块复制至系统剪贴板
vnoremap <Leader>y "+y

" 设置快捷键将系统剪贴板内容粘贴至 vim
nnoremap <Leader>p "+p

" 定义快捷键关闭当前分割窗口
nnoremap <Leader>q :q<CR>

" 定义快捷键保存所有窗口内容并退出 vim
nnoremap <Leader>wQ :wqall<CR>
nnoremap <Leader>wq :wq<CR>

" 不做任何保存，直接退出 vim
nnoremap <Leader>Q :qa!<CR>

nnoremap <Leader>gm :wa<CR>:make<CR><CR>:cw<CR>
"nnoremap <leader>mm :make<CR>
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <Leader>ec :close<CR>
noremap <Leader>ew :w<CR>
nnoremap <leader>e; mqA;<ESC>`q

"一些不错的映射转换语法（如果在一个文件中混合了不同语言时有用）
nnoremap <leader>lx :set filetype=xhtml<CR>
nnoremap <leader>lc :set filetype=css<CR>
nnoremap <leader>lj :set filetype=javascript<CR>
nnoremap <leader>lp :set filetype=php<CR>
nnoremap <leader>lv :set filetype=vim<CR>
nnoremap <leader>lc :set filetype=c<CR>

" 选中状态下 Ctrl+c 复制
vmap <C-c> "+y

" Tab操作快捷方式!
noremap <silent><leader>tn :tabn<CR>
noremap <silent><leader>tp :tabp<CR>
noremap <silent><leader>tt :tabnew<CR>
noremap <silent><leader>tc :tabclose<CR>
noremap <silent><leader>1 :tabn 1<CR>
noremap <silent><leader>2 :tabn 2<CR>
noremap <silent><leader>3 :tabn 3<CR>
noremap <silent><leader>4 :tabn 4<CR>
noremap <silent><leader>5 :tabn 5<CR>
noremap <silent><leader>6 :tabn 6<CR>
noremap <silent><leader>7 :tabn 7<CR>
noremap <silent><leader>8 :tabn 8<CR>
noremap <silent><leader>9 :tabn 9<CR>
noremap <silent><leader>0 :tabn 10<CR>

" Buffers操作快捷方式!
noremap <silent><leader>bl :buffers<CR>
noremap <silent><leader>bn :bnext<CR>
noremap <silent><leader>pn :bprevious<CR>

nnoremap <silent> <leader>fe :Vexplore<CR>

" return OS type, eg: windows, or linux, mac, et.st..
function! MySys()
    if has("win16") || has("win32") || has("win64") || has("win95")
        return "windows"
    elseif has("osx")
        return "osx"
    elseif has("unix")
        return "linux"
    endif
endfunction

if has("autocmd")
    "记住最后一次编辑的位置
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif
endif

augroup checkFileType
    autocmd!
    autocmd FileType vim nnoremap <leader>mm :w<CR>:source %<CR>
    autocmd FileType make set noexpandtab   " 取消用空格取代tab
    autocmd FileType c nnoremap <leader>mm :w<CR>:!gcc %:p -o %:r && %:p:r<CR>
    autocmd FileType cpp nnoremap <leader>mm :w<CR>:!g++ %:p -o %:r && %:p:r<CR>
    autocmd FileType python nnoremap <leader>mm :w<CR>:!python %<CR>
augroup END

" 删除文件行尾空格
function! <SID>StripTrailingSpaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfunction

if exists("save_delete_trailing_space") && save_delete_trailing_space
    augroup strip_traling_spaces
        autocmd!
        autocmd FileType c,cpp,css,javascript,python autocmd BufWritePre <buffer> call <SID>StripTrailingSpaces() 
    augroup END
endif
