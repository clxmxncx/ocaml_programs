(* http://programmer-avec-ocaml.lri.fr/ *)

(* Programme 16 page 112
   Le problème des $N$ reines *)

(* timer ajouté:

➜  ocamlc -o queens unix.cma queens.ml
➜  ./queens 14
execution time: 528.081826
365596

➜  ocamlopt unix.cmxa queens.ml -o queens_opt
➜  ./queens_opt 14
execution time: 62.912940
365596

https://caml.inria.fr/pub/docs/manual-ocaml/libunix.html

Dans le livre:
"On peut ainsi dénombrer les 365 596 solutions du problème des 14 reines en moins
d’une minute."
*)

open Unix

let time f x y z =
  let start = (times ()).tms_utime in
  let res = f x y z in
  let stop = (times ()).tms_utime in
  let () = Printf.printf "execution time: %f\n" (stop -. start) in
  res


module S = Set.Make(struct type t = int let compare = compare end)

let map f s = S.fold (fun x s -> S.add (f x) s) s S.empty

let rec upto n = if n < 0 then S.empty else S.add n (upto (n-1))

let rec count cols d1 d2 =
(* cols: colonnes restant à considérer *)
(* colonnes à ne pas considérer: *)
(*    d1: colonnes correspondant à diagonales ascendantes *)
(*    d2: colonnes correspondant à diagonales descendantes *)
  if S.is_empty cols then
    1
  else
    S.fold
      (fun c res ->
        let d1 = map succ (S.add c d1) in
        let d2 = map pred (S.add c d2) in
        res + count (S.remove c cols) d1 d2)
      (S.diff (S.diff cols d1) d2)
      0

let () =
  let n = int_of_string Sys.argv.(1) in
  let res = time count (upto (n - 1)) S.empty S.empty in
  Format.printf "%d@." res
