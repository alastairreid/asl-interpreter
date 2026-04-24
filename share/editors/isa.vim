" Vim syntax file
" Language: isa
" Maintainer:   Alastair Reid <alastair.reid@intel.com>
" Last Change:  Fri July 24 2025
" Filenames:    *.isa

" Comments:
" Make sure to create a file name .vim/ftdetect/isa.vim containing this line:
" au BufRead,BufNewFile *.isa set filetype=isa

if exists("b:current_syntax")
    finish
endif

syn keyword isaType        type array bitfield enumeration record exception
syn keyword isaType        Bit Bits Boolean Integer String
syn keyword isaType        Std::RAM Std::Slice
syn keyword isaExpr        and or xor not in as
syn keyword isaExpr        UNSPECIFIED as typeof
syn keyword isaStmt        assert return
syn keyword isaStmt        begin end
syn keyword isaStmt        case of when otherwise others endcase
syn keyword isaStmt        for to downto do endfor
syn keyword isaStmt        if then elsif else endif
syn keyword isaStmt        while repeat until
syn keyword isaStmt        try catch throw
syn keyword isaStmt        endif endtry endwhile

syn keyword isaDecl        function let var requires ensures implicit optimize foreign pure impure
syn keyword isaDecl        __builtin __operator1 __operator2
syn keyword isaDecl        module endmodule import export interface file use as with
syn keyword isaConstant    False True
syn keyword isaTodo        contained todo TODO

syn match   isaIdentifier  "\<[A-Za-z_][A-Za-z0-9_]*\>"
syn match   isaNumber      "\<\d\+\>"
syn match   isaNumber      "\<0[xXbd][0-9A-F_]\+\>"
syn match   isaDelimiter   "\[\|\]\|(\|)\|{\|}"
syn match   isaOperator    "!=\|==\|+\|-\|*\|/\|**\|!\|<\|<=\|>\|>="
syn match   isaAssign      ":="
syn match   isaReturns     "->"
syn match   isaMapsto      "=>"
syn match   isaCoCo        "::"

syn region  isaMultilineComment start="/\*" end="\*/" contains=isaTodo
syn region  isaTrailingComment  start="//" end="$" contains=isaTodo
syn region  isaString           start=/"/ skip=/\\./ end=/"/

hi def link isaMultilineComment comment
hi def link isaTrailingComment comment
hi def link isaIdentifier      Normal
hi def link isaDecl            Keyword
hi def link isaType            Type
hi def link isaReturns         Keyword
hi def link isaMapsto          Keyword
hi def link isaCoCo            Identifier
hi def link isaStmt            Conditional
hi def link isaAssign          Conditional
hi def link isaExpr            StorageClass
hi def link isaConstant        Constant
hi def link isaOperator        Special
hi def link isaDelimiter       Normal
hi def link isaNumber          Number
hi def link isaString          String
hi def link isaTodo            Todo

let b:current_syntax = "isa"
