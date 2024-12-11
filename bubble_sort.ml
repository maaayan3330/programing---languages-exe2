open Ast_sol
open Nos_sol
open Semantics_sol

let generate_numbers n =
  let rec aux i stm =
    if i > n then stm
    else
      let var_name = "x" ^ string_of_int i in
      aux (i + 1) (Comp(stm, Ass(var_name, Num i)))
  in
  aux 1 Skip;;

(* Bubble sort program generator for n variables x1, x2, ..., xn *)
let run_bubble_sort n =
  let rec outer_loop i =
    if i > n then Skip
    else
      let rec inner_loop j =
        if j >= n - i + 1 then Skip
        else
          Comp (
            If (
              Gte (Var ("x" ^ string_of_int j), Var ("x" ^ string_of_int (j + 1))),
              (* Swap xi and xi+1 *)
              Comp (
                Ass ("tmp", Var ("x" ^ string_of_int j)),
                Comp (
                  Ass ("x" ^ string_of_int j, Var ("x" ^ string_of_int (j + 1))),
                  Ass ("x" ^ string_of_int (j + 1), Var "tmp")
                )
              ),
              Skip
            ),
            inner_loop (j + 1)
          )
      in
      Comp (inner_loop 1, outer_loop (i + 1))
  in
  outer_loop 1;;
