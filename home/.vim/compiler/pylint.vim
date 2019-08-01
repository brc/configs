let current_compiler = "pylint"

"if exists(":CompilerSet") != 2
"  command -nargs=* CompilerSet setlocal <args>
"endif
"
let s:rcflag = ''
if exists("b:gitroot")
    if filereadable(b:gitroot . '/pylintrc')
        let s:rcflag = '--rcfile=' . b:gitroot . '/pylintrc'
    endif
endif

execute 'setlocal makeprg=pylint\ ' . s:rcflag . '\ --output-format=text\ --msg-template=\"{path}:{line}:{column}:{C}:\ [{symbol}]\ {msg}\"\ --reports=no\ ' . expand('%')
setlocal errorformat=%A%f:%l:%c:%t:\ %m,%A%f:%l:\ %m,%A%f:(%l):\ %m,%-Z%p^%.%#,%-G%.%#
