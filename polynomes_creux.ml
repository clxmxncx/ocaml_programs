
(*EXERCICE E page 73 74*)
(*Livre Option Informatique MPSI/ MP-MP* N.Carré, R. Mansuy*)

type monome = int * int
type polynome = monome list


(*exemple de polynome : [(0;7);(3;-4);(1789;2)]*)


let newline () = print_newline();
                 print_newline()

let rec print_polynome p = match p with
  | []                -> ()
  | (d1,c1) :: q1     -> print_int c1; Printf.printf "X^";
                         print_int d1; Printf.printf "  "; print_polynome q1



let rec add_p p1 p2 = match p1, p2 with
  | _, [] -> p1
  | [], _ -> p2
  | (d1, c1) :: q1, (d2, c2) :: q2 when d1 = d2
          -> let c = c1 + c2 in
                if c = 0 then add_p q1 q2
                else (d1, c1 + c2) :: add_p q1 q2
  | (d1, c1) :: q1, (d2, c2) :: q2 when d1 < d2
          -> (d1, c1) :: add_p q1 p2
  | (d1, c1) :: q1, (d2, c2) :: q2
          -> (d2, c2) :: add_p p1 q2


(* let addp = add_p [(0,7);(3,-4);(1789,2)] [(0,-7);(2,-4);(3,2)] *)

let rec prod_mp (d,c) p = match (d,c), p with
  | _, []                   -> failwith "(prod_mp) polynome vide"
  | (d,c), [(d1,c1)]        -> [(d+d1,c * c1)]
  | (d,c), (d1,c1) :: q1    -> (d+d1,c * c1) :: prod_mp (d,c) q1

(* let prodmp = prod_mp [(2,3] [(0,7);(3,-4);(1789,2)] *)

let rec prod_p p1 p2 = match p1 with
  | []              -> failwith "(prod_p) polynôme vide"
  | [(d1,c1)]       -> prod_mp (d1,c1) p2
  | (d1,c1) :: q1   -> add_p (prod_mp (d1,c1) p2) (prod_p q1 p2)

(* let prodp = prod_p [(0,-7);(2,-4);(3,2)] [(0,7);(3,-4);(1789,2)] *)


let rec deriv_p p = match p with
  | []            -> []
  | (0,c1) :: q1  -> deriv_p q1
  | (d1,c1) :: q1 -> (d1-1,d1*c1) :: deriv_p q1

(* let pprime = deriv_p [(0,7);(3,-4);(1789,2)] *)



let polynome1 = [(0,7);(3,-4);(1789,2)]
let polynome2 = [(0,-7);(2,-4);(3,2)]
let monome = (2,3)

let () =
  Printf.printf "-> polynome 1 : ";
  print_polynome polynome1;

  newline ();

  Printf.printf "-> polynome 2 : ";
  print_polynome polynome2;

  newline ();

  Printf.printf "-> monome : ";
  print_polynome [monome];

  newline ();

  Printf.printf "-> Addition des deux polynomes : ";
  print_polynome (add_p polynome1 polynome2);

  newline ();

  Printf.printf "-> Produit du polynome 1 et du monome : ";
  print_polynome (prod_mp monome polynome1);

  newline ();

  Printf.printf "-> Produit du polynome 1 et du polynome 2 : ";
  print_polynome (prod_p polynome1 polynome2);

  newline ();

  Printf.printf "-> dérivée du polynome 1 : ";
  print_polynome (deriv_p polynome1);

  newline ();

  Printf.printf "-> dérivée du polynome 2 : ";
  print_polynome (deriv_p polynome2);

  newline ();

  Printf.printf "-> dérivée du monome : ";
  print_polynome (deriv_p [monome])
