open Ast
open Nos
open Semantics


let initial_numbers n =
  let rec aux i stm =
    if i > n then stm
    else
      let var_name = "x" ^ string_of_int i in
      aux (i + 1) (Comp(Ass(var_name, Num (n - i + 1)), stm))
  in
  aux 1 Skip;;


  let swap first second= 
        Comp (Ass ("tmp", Var (first)),
        Comp (Ass (first, Var (second)),
        Ass (second, Var "tmp"))
        )


let bubble_sort_logic n=
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
                swap ("x" ^ string_of_int j) ("x" ^ string_of_int (j + 1)),
                Skip
                ),
                inner_loop (j + 1)
            )
        in
        Comp (inner_loop 1, outer_loop (i + 1))
    in
    outer_loop 1;;



(* Print variables in the current state *)
let print_state state n =
  List.iter (fun var ->
    Printf.printf "%s = %d\n" var (state var)
  ) (List.init n (fun i -> "x" ^ string_of_int (i + 1)))
;;



(* Main function *)
let run_bubble_sort n =
  (* Step 1: Execute the initial assignments *)
  let s1 = nos (initial_numbers n, default_state) in
  print_state s1 n;

  (* Step 2: Execute Bubble Sort *)
  let s2 = nos (bubble_sort_logic n, s1) in
  print_state s2 n
;;

(* Run the Bubble Sort for n = 5 *)
run_bubble_sort 5;;
