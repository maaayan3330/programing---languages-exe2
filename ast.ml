type var = string;;

(* of (exp) mean that the constactor ask for a value that is from this (exp) *)

(* first constactor - aexp : aritmatic exp*)
type aexp = Num of int 
 | Var of var 
 | Add of aexp * aexp 
 | Mult of aexp * aexp 
 | Sub of aexp * aexp
 | Shr of aexp * aexp
 | Shl of aexp * aexp;;

(* seconed constactor - bexp : boolean exp*)
type bexp = True 
 | False 
 | Aeq of aexp * aexp   (*aritmetic comper *)
 | Beq of bexp * bexp   (*bool comper*)
 | Gte of aexp * aexp   (*x1>=x2 aritmetic*)
 | Neg of bexp
 | And of bexp * bexp;;

type stm = Ass of var * aexp 
 | Skip 
 | Comp of stm * stm     
 | If of bexp * stm * stm 
 | While of bexp * stm
 | Repeat of stm * bexp;;

type state = var -> int ;;


(* Add test5 *)
let test5 = 
  Comp (
    Ass ("a", Num 84),
    Comp (
      Ass ("b", Num 22),
      Comp (
        Ass ("c", Num 0),
        While (
          Neg (Aeq (Var "b", Num 0)),
          Comp (
            Ass ("a", Shl (Var "a", Num 1)),  (* a := a bit-shift-left 1 *)
            Ass ("b", Shr (Var "b", Num 1))   (* b := b bit-shift-right 1 *)
          )
        )
      )
    )
  );;

