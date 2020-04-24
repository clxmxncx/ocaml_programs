(*PROBLEME 2 PAGE 253 : Arbres gauchers*)
(*Livre Option Informatique MPSI/ MP-MP* N.Carré, R. Mansuy*)

(*

Traitement des exercices et comparaison effective des temps d'exécution du tri standard de OCaml et
du tri basé sur la structure d'arbre gaucher.

*)

(*

  Compilation et exécution du programme :

  $ ocamlc -o arbres_gauchers.native unix.cma arbres_gauchers.ml
  $ ./arbres_gauchers.native

*)

open Unix

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let exec_time = Unix.gettimeofday () -. t in
  (res, exec_time)

let rec print_liste l = match l with
  | []        -> ()
  | x :: q    -> print_int x; Printf.printf " ; "; print_liste q

let rec insere_element x l = match l with
  | [] -> [x]
  | y :: q -> y :: (insere_element x q)

let generateur_liste n =
  let l = ref [] in
  for i = 0 to 100 do
    l := insere_element (Random.int n) !l
  done;
  !l

let generateur_liste_de_listes nb taille =
  let ll = ref [] in
  for i = 0 to nb do
    ll := insere_element (generateur_liste taille) !ll
  done;
  !ll

type 'a arbre = Vide | N of 'a * 'a arbre * 'a arbre

let rec rang a = match a with
  | Vide -> 0
  | N(_,_,d) -> (rang d) + 1

let rec gaucher a = match a with
  | Vide            -> true
  | N(_,Vide, Vide) -> true
  | N(_,Vide,_)     -> false
  | N(e1,g,Vide)     -> let N(e2,_,_) = g in e1 >= e2 && gaucher g
  | N(e1,g,d)        -> let N(e2,_,_) = g in let N(e3,_,_) = d in
                       (e2 <= e1) && (e3 <= e1) && (gaucher g) && (gaucher d) && (rang d <= rang g)


type 'a arbre2 = Vide | M of int * 'a * 'a arbre2 * 'a arbre2

let rang2 a = match a with
  | Vide       -> 0
  | M(r,_,_,_) -> r

let rec fusion a b = match a, b with
  | _, Vide                               -> a
  | Vide, _                               -> b
  | M(_,ea,_,_), M(_,eb,_,_) when eb > ea -> fusion b a
  | M(_,ea,ga,da), _                      -> let c = fusion da b in
                                             if (rang2 ga) >= (rang2 c) then
                                               M((rang2 c) + 1, ea, ga, c)
                                             else
                                               M((rang2 ga) + 1, ea, c, ga)

let insertion n a = fusion a (M(1,n,Vide,Vide))


let extraire_max a = match a with
  | Vide       -> failwith " l'arbre est vide"
  | M(r,e,g,d) -> e, fusion g d


let tri_gaucher l =
  let rec liste_to_gaucher l = match l with
    | []     -> Vide
    | x :: q -> insertion x (liste_to_gaucher q) in
  let rec tri acc a = match a with
    | Vide -> acc
    | _    -> let (x, b) = extraire_max a in tri (x :: acc) b in
  tri [] (liste_to_gaucher l)


(* let t = M (2, 8, M(2,4, M(1,1, Vide, Vide), M(1,3, Vide, Vide)), M(1,7, M(1,5, Vide, Vide), Vide))
let t2 = M (2,10, M(2,5, M(1,2, Vide, Vide), M(1,4, Vide, Vide)), M(1,7, M(1,6, Vide, Vide), Vide)) *)


let temps_moyen tri ll =
  let rec aux ll = match ll with
    | [] -> ()
    | l :: q -> let _ = tri l in aux q
  in
  let (res, exec_time) = time (fun () -> aux ll) in
  exec_time /. (float_of_int (List.length ll))


let tri_gaucher_moyen ll = temps_moyen tri_gaucher ll

let tri_standard_moyen ll = temps_moyen (List.sort compare) ll




let () =

  let l = generateur_liste 100 in
  let (res, exec_time) = time (fun () -> tri_gaucher l) in
  let (res2,exec_time2) = time (fun () -> List.sort compare l) in

  Printf.printf " -> La liste initiale est : ";
  print_liste l ;
  print_newline ();
  print_newline ();
  Printf.printf " (tri_gaucher) : La liste triée est :";
  print_liste res;
  print_newline ();
  print_newline ();
  Printf.printf " (List.sort) : La liste triée est :";
  print_liste res2;
  print_newline ();
  print_newline ();
  Printf.printf " --> COMPARAISON TEMPS D'EXECUTION DE LA FONCTION TRI_GAUCHER ET FONCTION DE TRI DU MODULE LIST
  sur 100 listes";
  print_newline ();
  print_newline ();

  let ll = generateur_liste_de_listes 100 100 in
  let t = tri_gaucher_moyen ll in
  Printf.printf "(tri_gaucher_moyen) ";
  print_float t;
  print_newline ();
  let t2 = tri_standard_moyen ll in
  Printf.printf "(List.sort) ";
  print_float t2;
  print_newline ()
