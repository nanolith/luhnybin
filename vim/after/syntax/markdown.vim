unlet b:current_syntax

syntax include @Yacc syntax/yacc.vim
unlet b:current_syntax
syntax include @Cpp syntax/cpp.vim
unlet b:current_syntax
syntax region nowebCpp start=+^    <<.*>>=$+ end=+^    @$+ contains=@Cpp keepend

let b:current_syntax = "markdown"
