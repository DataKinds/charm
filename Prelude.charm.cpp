#include <string>
#include "Prelude.charm.h"

const std::string prelude = R"~(

" OUTPUT FUNCTIONS " pop
" ================ " pop

put :: any ->
put := dup p newline

" DEBUGGING " pop
" ========= " pop

" <stack length> clearstack " pop
clearstack :: int ->
_clearstack_args := " clearstackref " flip setref
_clearstack_cond := " clearstackref " getref 1 - dup " clearstackref " flip setref
_clearstack      := [ _clearstack_cond ] [ 0 _clearstack ] [ ] ifthen
clearstack       := _clearstack_args _clearstack

" <stack depth> printstack " pop
printstack :: int ->
_printstack_args       := " printstackref " flip setref
_printstack_correction := " printstackref " getref rotate
printstack             := _printstack_args [ put " printstackref " getref rotate ] " printstackref " getref repeat i _printstack_correction

" pause " pop
pause :: ->
pause := " Press return to continue... " pstring getline pop

" [ <arguments> ] [ <code> ] <stack depth> stepthrough " pop
stepthrough :: list list int ->
" stepthroughstack " createstack

_stepthrough_pop_args          := " stepthroughdepthref " flip setref   inline " stepthroughcoderef " flip setref   " stepthroughargsref " flip setref
_stepthrough_stack_init&switch := " stepthroughstack " switchstack clearstack " stepthroughargsref " getref i
_stepthrough_init_cursor       := " stepthroughcursor " 0 setref
_stepthrough_init              := _stepthrough_pop_args _stepthrough_init_cursor _stepthrough_stack_init&switch " Initial stack: " pstring newline _stepthrough_print_stack pause newline

_stepthrough_arg_depth := " stepthroughdepthref " getref
_stepthrough_arg_code  := " stepthroughcoderef " getref

_stepthrough_set_headref := " stepthroughheadref " flip setref
_stepthrough_get_headref := " stepthroughheadref " getref

_stepthrough_func_string_len     := tostring len 4 -
_stepthrough_move_cursor_int     := " stepthroughcursor " getref + " stepthroughcursor " flip setref
_stepthrough_move_cursor_len     := _stepthrough_func_string_len 1 + _stepthrough_move_cursor_int pop
_stepthrough_move_cursor_headref := _stepthrough_get_headref _stepthrough_move_cursor_len

_stepthrough_print_func   := tostring 2 flip len 2 - 1 2 swap substring flip pop pstring
_stepthrough_print_stack  := " Stack: " pstring newline _stepthrough_arg_depth printstack
_stepthrough_print_cursor := " " [ space concat ] " stepthroughcursor " getref repeat i " ^ " concat pstring newline
_stepthrough_print_info   := " Running functions... " pstring newline _stepthrough_arg_code _stepthrough_print_func newline

_stepthrough_iter       := _stepthrough_print_info _stepthrough_print_cursor _stepthrough_set_headref " stepthroughstack " switchstack _stepthrough_get_headref i _stepthrough_print_stack _stepthrough_move_cursor_headref 0 switchstack pause newline [ ]
_stepthrough_map        := 0 switchstack _stepthrough_arg_code [ _stepthrough_iter ] map 0 switchstack
stepthrough             := _stepthrough_init _stepthrough_map

" STACK MANIPULATION " pop
" ================== " pop

flip :: any any -> any any
flip := 0 1 swap

swapnth :: int ->
swapnth := dup 1 + swap

" <stack index> copyfrom " pop
copyfrom :: int ->
copyfrom := " copyfromref " flip setref 0 " copyfromref " getref swap dup 1 " copyfromref " getref 1 + swap

" <object> <stack index> pushto " pop
pushto :: any int ->
_pushto_args := 0 flip
_pushto_cond := dup 2 copyfrom -
_pushto_inc  := flip 1 + flip
_pushto      := [ _pushto_cond ] [ 1 copyfrom 2 + swapnth _pushto_inc _pushto ] [ pop pop ] ifthen
pushto       := _pushto_args _pushto

" <stack depth> rotate " pop
rotate :: int ->
rotate := pushto

" <object> <number of copies> stack " pop
stack :: any int ->
stack := [ 1 - dup ] [ flip dup 0 2 swap stack ] [ pop ] ifthen

" ARITHMETIC " pop
" ========== " pop

succ :: int -> int
succ := 1 +

" STRING MANIPULATION " pop
" =================== " pop

" space " pop
space :: ->
space := 32 char

" \" string \" <from> <to> substring " pop
substring :: string int int -> string
_substring_args     := " " 3 pushto
_substring_distance := dup 2 copyfrom - 1 -
_substring_iter     := 0 2 swap 1 copyfrom at 1 4 swap concat 0 3 swap 0 2 swap flip succ flip
_substring          := [ _substring_distance succ ] [ _substring_iter _substring ] [ pop pop flip ] ifthen
substring           := _substring_args _substring

" LIST MANIPULATION " pop
" ================= " pop

" [ list ] <from> <to> cut " pop
cut :: list int int -> list
_cut_args     := [ ] 3 pushto
_cut_distance := dup 2 copyfrom - 1 -
_cut_iter     := 0 2 swap 1 copyfrom at 1 4 swap concat 0 3 swap 0 2 swap flip succ flip
_cut          := [ _cut_distance succ ] [ _cut_iter _cut ] [ pop pop flip ] ifthen
cut           := _cut_args _cut

" [ list ] <number> repeat " pop
repeat :: list/string int -> list
_repeat_args := flip type " repeattyperef " flip setref dup 0 2 swap 1 -
_repeat_iter := 0 2 swap dup 0 2 swap concat flip 2 0 swap 1 -
_repeat_zero := [ " repeattyperef " getref " LIST_FUNCTION " eq ] [ [ ] ] [ " " ] ifthen
_repeat      := [ dup ] [ _repeat_iter _repeat ] [ pop flip pop ] ifthen
repeat       := _repeat_args [ dup ] [ _repeat ] [ pop pop pop _repeat_zero ] ifthen

" [ list ] [ function ] map " pop
map :: list list -> list
_map_args    := " mapfuncref " flip setref [ ]
_map_iter    := flip 1 split flip " mapfuncref " getref i 1 2 swap concat
_map_cond    := flip len 1 2 swap
_map_cleanup := flip pop
_map         := [ _map_cond ] [ _map_iter _map ] [ _map_cleanup ] ifthen
map          := _map_args _map

" [ list ] [ function ] for " pop
for :: list list ->
revfor :: list list ->
_for_args := " for_function " flip setref " for_iterable " flip setref
_for_body := " for_iterable " getref len flip pop [ " for_iterable " getref " for_index " getref at flip pop " for_item " flip setref " for_function " getref i " for_index " getref [ " for_rev " getref 1 eq ] [ 1 - ] [ 1 + ] ifthen " for_index " flip setref ] flip repeat i
for_item := " for_item " getref
for := _for_args " for_index " 0 setref _for_body
revfor := _for_args " for_index " " for_iterable " len flip pop 1 - setref " for_rev " 1 setref _for_body

" [ list ] reverse " pop
reverse :: list -> list
reverse := [ ] flip [ for_item concat ] revfor

" [ list ] <list index> delitem " pop
delitem :: list int -> list
delitem := 1 + split flip len 1 - split pop flip concat

" [ condition ] [ code ] while " pop
while :: list ->
_loopwhile := [ " condition " getref i ] [ " block " getref i _loopwhile ] [ ] ifthen
while := [ " condition " " block " ] setrefs _loopwhile

fromcharlist :: list -> string
fromcharlist := [ len ] [ " " flip [ for_item i concat ] for ] [ pop ] ifthen

" NAMED REF MANIPULATION " pop
" ====================== " pop

" <ref name> [ function ] modref " pop
modref :: any list ->
_modref_args := " modref_function " flip setref " modref_reference " flip setref
modref := _modref_args " modref_reference " getref getref " modref_function " getref i " modref_reference " getref flip setref

" [ list ] setrefs " pop
setrefs :: list ->
setrefs := [ for_item i flip setref ] revfor

)~";
