(*EXERCICE F page 74*)
(*Livre Option Informatique MPSI/ MP-MP* N.Carré, R. Mansuy*)



type symb = N of int | Op of (int -> int -> int)
and expr = symb list

(*exemple  : [N 3; N 5; N 4; Op ( + ); Op ( * ); N 2; Op ( - )]*)
(*résultat = 25*)


let postfixe l =
  let rec aux p l = match p, l with
    | [x], []                  -> x
    | x :: y :: q1, Op f :: q2 -> aux (f y x :: q1) q2
    | _, N x :: q              -> aux (x::p) q
  in aux [] l



let () =
  let x = postfixe [N 3; N 5; N 4; Op ( + ); Op ( * ); N 2; Op ( - )] in
  print_int x;
  print_newline ()
