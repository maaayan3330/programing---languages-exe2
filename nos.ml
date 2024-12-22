[@@@ocaml.warning "-8"];;

open Semantics
open Ast


let rec nos c = 
    match c with
    |(Ass(v,e1),s)-> update v e1 s
    |(Skip,s) -> s
    |(Comp(stm1,stm2),s)-> nos( stm2 , nos(stm1,s) )
    |(If(e1, stm1, stm2), s)-> if ((solve_b e1 s) = "tt") then nos(stm1,s) else nos(stm2, s) 
    |(While(e1,stm1), s) -> if ((solve_b e1 s) = "tt") then nos (While(e1, stm1), nos (stm1, s)) else s
    |(Repeat(stm1,e1),s) -> if ((solve_b e1 s) = "tt") then nos (stm1,s) else nos (Repeat (stm1,e1), nos (stm1,s)) ;;



(* tests *) 

print_string "x = ";;
print_int (let new_state = nos (Ast.test1, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test2, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test3, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "x = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "x");;
print_endline "";;

print_string "y = ";;
print_int (let new_state = nos (Ast.test4, Semantics.s1) in new_state "y");;
print_endline "";;

print_string "a = ";;
print_int (let new_state = nos (Ast.test5, Semantics.default_state) in new_state "a");;
print_endline "";;

print_string "b = ";;
print_int (let new_state = nos (Ast.test5, Semantics.default_state) in new_state "b");;
print_endline "";;

print_string "c = ";;
print_int (let new_state = nos (Ast.test5, Semantics.default_state) in new_state "c");;
print_endline "";;



print_string "x = ";;
print_int (let new_state = nos (test6, Semantics.s0) in new_state "x");;
print_endline "";;

print_string "y = ";;
print_int (let new_state = nos (test6, Semantics.s0) in new_state "y");;
print_endline "";;
