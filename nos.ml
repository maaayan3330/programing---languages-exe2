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



