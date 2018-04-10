#include <string>
#include "Prelude.charm.h"

std::string prelude = R"~(

" OUTPUT FUNCTIONS " pop
" ================ " pop

put := dup p newline

" DEBUGGING " pop
" ========= " pop

" <stack length> clearstack " pop
_clearstack_args := " clearstackref " flip setref
_clearstack_cond := " clearstackref " getref 1 - dup " clearstackref " flip setref
_clearstack      := [ _clearstack_cond ] [ 0 _clearstack ] [ ] ifthen
clearstack       := _clearstack_args _clearstack

" <stack depth> printstack " pop
_printstack_args       := " printstackref " flip setref
_printstack_correction := " printstackref " getref rotate
printstack             := _printstack_args [ put " printstackref " getref rotate ] " printstackref " getref repeat i _printstack_correction

" pause " pop
pause := " Press enter to continue... " p getline pop

" [ <arguments> ] [ <code> ] <stack depth> stepthrough " pop
20000 " stepthroughstack " createstack

_stepthrough_pop_args          := " stepthroughdepthref " flip setref " stepthroughcoderef " flip setref  " stepthroughargsref " flip setref
_stepthrough_stack_init&switch := " stepthroughstack " switchstack clearstack " stepthroughargsref " getref i
_stepthrough_init              := _stepthrough_pop_args _stepthrough_stack_init&switch " Initial stack: " p newline _stepthrough_print_stack pause newline

_stepthrough_arg_depth := " stepthroughdepthref " getref
_stepthrough_arg_code  := " stepthroughcoderef " getref

_stepthrough_set_headref := " stepthroughheadref " flip setref
_stepthrough_get_headref := " stepthroughheadref " getref

_stepthrough_print_info  := " Running functions... " p newline _stepthrough_arg_code tostring p newline
_stepthrough_print_stack := _stepthrough_arg_depth printstack

_stepthrough_iter       := _stepthrough_print_info _stepthrough_set_headref " stepthroughstack " switchstack _stepthrough_get_headref i _stepthrough_print_stack 0 switchstack pause newline [ ]
_stepthrough_map        := 0 switchstack _stepthrough_arg_code [ _stepthrough_iter ] map 0 switchstack
stepthrough             := _stepthrough_init _stepthrough_map

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
map          := _map_args _map

)~";
