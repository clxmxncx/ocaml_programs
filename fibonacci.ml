
(* http://programmer-avec-ocaml.lri.fr/code/p84.ml.html *)
(* http://programmer-avec-ocaml.lri.fr/code/p85.ml.html *)

(* timer added *)

open Unix

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let exec_time = Unix.gettimeofday () -. t in 
  (res, exec_time)

let memo = Hashtbl.create 17

let rec fib n =
  if n <= 1 then n else fib (n - 2) + fib (n - 1)

(* http://programmer-avec-ocaml.lri.fr/code/p84.ml.html *)
let rec fib_memo n =
  try
    Hashtbl.find memo n
  with Not_found ->
    let fn =
      if n <= 1 then n else fib_memo (n-2) + fib_memo (n-1) in
        Hashtbl.add memo n fn;
        fn

(* http://programmer-avec-ocaml.lri.fr/code/p85.ml.html *)
let fib_dp n =
  if n = 0 then 0 else
  (* fib_dp est non récursive,
   le tableau peut être alloué à l’intérieur de la fonction : *)
  let f = Array.make (n+1) 0 in
  f.(1) <- 1;
  for i = 2 to n do f.(i) <- f.(i-2) + f.(i-1) done;
  f.(n)

(* http://programmer-avec-ocaml.lri.fr/solutions/ex11_1.ml.html *)
(* Calcul de fib(n) itérativement, en conservant seulement les deux
   dernières valeurs *)
let fib_ex_11_1 n =
  let rec fib_aux a b i = (* invariant : a = fib(i) et b = fib(i+1) *)
    if i = n then a else fib_aux b (a + b) (i + 1)
  in
  fib_aux 0 1 0
(* note : on va un cran plus loin que nécessaire, ce qu'on peut éviter
   en traitant à part le cas n = 0 *)

let () = 
  let n = int_of_string Sys.argv.(1) in
  let (res, exec_time) = time (fun () -> fib n) in 
  let (res_memo, exec_time_memo) = time (fun () -> fib_memo n) in
  let (res_dp, exec_time_dp) = time (fun () -> fib_dp n) in 
  let (res_ex_11_1, exec_time_ex_11_1) = time (fun () -> fib_ex_11_1 n) in 
    Printf.printf "\n";
    Printf.printf "> result (naive function): \t\t\t%d\n" res;
    Printf.printf "> execution time (naive function): \t\t%f\n" exec_time;
    Printf.printf "> result with memoization: \t\t\t%d\n" res_memo;
    Printf.printf "> execution time with memoization: \t\t%f\n" exec_time_memo;
    Printf.printf "> result with dynamic programming: \t\t%d\n" res_dp;
    Printf.printf "> execution time with dynamic programming: \t%f\n" exec_time_dp;
    Printf.printf "> result ex 11.1: \t\t\t\t%d\n" res_ex_11_1;
    Printf.printf "> execution time ex 11.1: \t\t\t%f\n" exec_time_ex_11_1;
    Printf.printf "\n"
    
(* results *)
(*  

$ ocamlopt unix.cmxa fibonacci.ml -o fibonacci_opt && ./fibonacci_opt 40

> result (naive function): 			102334155
> execution time (naive function): 		0.592695
> result with memoization: 			102334155
> execution time with memoization: 		0.000014
> result with dynamic programming: 		102334155
> execution time with dynamic programming: 	0.000003
> result ex 11.1: 				102334155
> execution time ex 11.1: 			0.000001


$ ocamlopt unix.cmxa fibonacci.ml -o fibonacci_opt && ./fibonacci_opt 50

> result (naive function): 			12586269025
> execution time (naive function): 		72.142781
> result with memoization: 			12586269025
> execution time with memoization: 		0.000014
> result with dynamic programming: 		12586269025
> execution time with dynamic programming: 	0.000000
> result ex 11.1: 				12586269025
> execution time ex 11.1: 			0.000001

*)

(* tests *)
let () =
  assert (fib 0 = 0);
  assert (fib 1 = 1);
  assert (fib 10 = 55);
  assert (fib 14 = 377);
  assert (fib_memo 0 = 0);
  assert (fib_memo 1 = 1);
  assert (fib_memo 10 = 55);
  assert (fib_memo 14 = 377);
  assert (fib_dp 0 = 0);
  assert (fib_dp 1 = 1);
  assert (fib_dp 10 = 55);
  assert (fib_dp 14 = 377);
  assert (fib_ex_11_1 0 = 0);
  assert (fib_ex_11_1 1 = 1);
  assert (fib_ex_11_1 10 = 55);
  assert (fib_ex_11_1 14 = 377)







