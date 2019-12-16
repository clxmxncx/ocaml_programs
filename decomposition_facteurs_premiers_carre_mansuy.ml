
(* Option Informatique Carré Mansuy : exercice B page 22*)
(* solution proposée, légèrement différente de celle du corrigé *)


(* ENONCE : si n est un facteur premier, écrire une suite d'instructions qui affiche la décomposition de n
en produit de facteurs premiers.*)


let n = ref 3500 in

for d = 2 to int_of_float (sqrt (float_of_int !n)) do  
    let est_diviseur = ref false in     
        if !n mod d = 0 then est_diviseur := true;
    let c = ref 0 in
    while !n mod d = 0 do
        c := !c + 1;
        n := !n/d
    done;
    if !est_diviseur = true then
         begin 
            print_int d;
            print_string "^";
            print_int !c;
            print_string " "
        end
done;
print_string "\n"

