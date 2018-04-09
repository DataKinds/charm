#include <string>

std::string prelude = R"~(

" OUTPUT FUNCTIONS " pop
" ================ " pop

p := dup pp
print := p newline
pn := [ print pop ] flip repeat i

" DEBUGGING " pop
" ========= " pop

" <stack depth> printstack " pop
_printstack_args       := " printstackref " flip setref
_printstack_correction := " printstackref " getref rotate
printstack             := _printstack_args [ print " printstackref " getref rotate ] " printstackref " getref repeat i _printstack_correction

" STACK MANIPULATION " pop
" ================== " pop

flip := 0 1 swap

swapnth := dup 1 + swap

" <stack index> copyfrom " pop
copyfrom := " copyfromref " flip setref 0 " copyfromref " getref swap dup 1 " copyfromref " getref 1 + swap

" <object> <stack index> pushto " pop
_pushto_args := 0 flip
_pushto_cond := dup 2 copyfrom -
_pushto_inc  := flip 1 + flip
_pushto      := [ _pushto_cond ] [ 1 copyfrom 2 + swapnth _pushto_inc _pushto ] [ pop pop ] ifthen
pushto       := _pushto_args _pushto

" <stack depth> " pop
rotate := pushto

" <object> <number of copies> stack " pop
stack := [ 1 - dup ] [ flip dup 0 2 swap stack ] [ pop ] ifthen

" ARITHMETIC " pop
" ========== " pop

succ := 1 +

" LIST MANIPULATION " pop
" ================= " pop

" [ list ] <from> <to> cut " pop
_cut_args     := [ ] 3 pushto
_cut_distance := dup 2 copyfrom -
_cut_iter     := 0 2 swap 1 copyfrom at 1 4 swap concat 0 3 swap 0 2 swap flip succ flip
_cut          := [ _cut_distance succ ] [ _cut_iter _cut ] [ pop pop flip ] ifthen
cut           := _cut_args _cut

" [ list ] <number> repeat " pop
_repeat_args := flip dup 0 2 swap 1 -
_repeat_iter := 0 2 swap dup 0 2 swap concat flip 2 0 swap 1 -
_repeat      :=  [ dup ] [ _repeat_iter _repeat ] [ pop flip pop ] ifthen
repeat       := _repeat_args _repeat

" [ list ] [ function ] map " pop
_map_args    := " mapfuncref " flip setref [ ]
_map_iter    := flip 1 split flip " mapfuncref " getref i 1 2 swap concat
_map_cond    := flip len 1 2 swap
_map_cleanup := flip pop
_map         := [ _map_cond ] [ _map_iter _map ] [ _map_cleanup ] ifthen
map          := _map_args _map_iter

)~";
