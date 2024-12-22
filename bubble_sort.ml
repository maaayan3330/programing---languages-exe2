open Ast
open Nos
open Semantics

(*PAY ATTENION!! in the run_bubble_sort you ask to be able to chose n in an esey order to check
so you need to enter the vars and there value manually accordingly to the n you choose*)



(*swap function*)
let swap first second= 
      Comp (Ass ("tmp", Var (first)),
      Comp (Ass (first, Var (second)),
      Ass (second, Var "tmp"))
      )

(*the bubble_sort_logic function*)
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



(* print function *)
let print_state state n =
  List.iter (fun var ->
    Printf.printf "%s = %d\n" var (state var)
  ) (List.init n (fun i -> "x" ^ string_of_int (i + 1)))
;;





let run_bubble_sort n =

  (* init the vars by the n you choose ,you may need to add more or less vars tp the state s1 *)
  let s1  =
    let s = Semantics.default_state in
    let s = Semantics.update "x1" (Num 3) s in  
    let s = Semantics.update "x2" (Num 5) s in
    let s = Semantics.update "x3" (Num 1) s in
    let s = Semantics.update "x4" (Num (-2)) s in
    let s = Semantics.update "x5" (Num 100) s in
    let s = Semantics.update "x6" (Num 15) s in
    let s = Semantics.update "x7" (Num 80) s in
    let s = Semantics.update "x8" (Num 47) s in
    let s = Semantics.update "x9" (Num (-10)) s in
    let s = Semantics.update "x10" (Num 0) s in
    s
  in
  
  Printf.printf "Before sorting\n";
  print_state s1 n;

  (* Step 2: Execute Bubble Sort *)
  let s2 = nos (bubble_sort_logic n, s1) in
 
  Printf.printf "After sorting\n";
  print_state s2 n
;;

(* Run the Bubble Sort for n = 10 change the n in line 81 for diffrent n *)
run_bubble_sort 10;;

