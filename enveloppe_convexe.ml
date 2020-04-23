(*PROBLEME 1 PAGE 146*)
(*Livre Option Informatique MPSI/ MP-MP* N.Carré, R. Mansuy*)


(*
COMPILATION ET EXECUTION DU PROGRAMME :    

$ ocamlc -o enveloppe_convexe.native graphics.cma enveloppe_convexe.ml
$ ./enveloppe_convexe.native                                          

*)

type point = int * int and nuage = point array

(* question 2 *)

(* Le tableau e est un tableau de coordonnées où les couples sont rangés par ordre
croissant d'abscisses*)
let pointinit e =
  let n = Array.length e in
  let k = ref 0 in
  for i = 1 to n - 1 do
    if snd e.(i) < snd e.(!k) then
        k := i
  done;
  !k

(*question 3*)

let orientation e i j k =
  let (xi, yi) = e.(i) and (xj, yj) = e.(j) and (xk, yk) = e.(k) in
  let det = (xj - xi) * (yk - yi) - (yj - yi) * (xk - xi) in
  if det > 0 then 1
  else
    begin
      if det < 0 then -1
      else 0
    end


(*question 5*)

let pointmax e i =
  let k = ref (if i = 0 then 1 else 0) in
  for j = 0 to Array.length e-1 do
    if i <> j && orientation e i j !k > 0 then
      k:= j
  done;
  !k


(*question 6*)

let jarvis e =
  let rec aux i0 bord = match pointmax e (List.hd bord) with
    | i when i = i0 -> bord
    | i             -> aux i0 (i :: bord)
  in
  let i0 = pointinit e in
  aux i0 [i0]


(* OUTILS *)
let wait_for_key_pressed ?(pos=(0,0)) ?(txt="(press any key)") () =
  (* https://www.oreilly.com/library/view/x-window-system/9780937175149/Chapter05.html *)
  let font = "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1" in
  Graphics.set_font font;
  Graphics.set_color Graphics.blue;
  Graphics.moveto (fst pos) (snd pos);
  Graphics.draw_string txt;
  Graphics.synchronize ();
  Graphics.read_key ()

let initialize_graphics ~title ~size =
  Printf.printf "--- %s\n" title;
  Graphics.open_graph size;
  Graphics.set_window_title title


(*AFFICHAGE*)

let offset = 100
let scale = 30

(* cercle de centre x,y et de rayon r *)

let cercle x y r c scale =
  Graphics.set_color c;
  Graphics.fill_circle (offset + x*scale) (offset + y*scale) r

let tracer_nuage tab =
  for i = 0 to Array.length tab - 1 do
    let x = fst tab.(i) and y = snd tab.(i) in
    cercle x y 4 Graphics.magenta scale;
    (*Numérotation des points : *)
    let x2 = offset + x * scale and y2 = offset + scale * y + 10 in
    Graphics.moveto x2 y2;
    Graphics.set_color Graphics.blue ;
    Graphics.set_font "-*-fixed-medium-r-semicondensed--18-*-*-*-*-*-iso8859-1";
    Graphics.draw_string (string_of_int (i));
  done

let tracer_bord e bord =
  Graphics.set_color (Graphics.rgb 150 0 255);
  Graphics.set_line_width 2;
  let j = List.hd bord in
  Graphics.moveto (offset + (fst e.(j))*scale) (offset + (snd e.(j)*scale));
  let i = ref 0 in
  let rec aux k bord = match k, bord with
    | k, []     -> i := k
    | k, bord   ->
            let m = List.hd bord in
            let tail_bord = List.tl bord in
              begin
                Graphics.lineto (offset + (fst e.(m))*scale) (offset + (snd e.(m))*scale);
                aux m tail_bord
            end
  in
  aux j (List.tl bord);
  Graphics.lineto (offset + fst e.(j)*scale) (offset + snd e.(j)*scale)



let demo_tracer_enveloppe_convexe e =
  initialize_graphics ~title:"nuage" ~size:" 600x600";
  tracer_nuage e;
  let bord = jarvis e in
  tracer_bord e bord;
  ignore (wait_for_key_pressed ())


let () =
  let e = [|
    (0,0); (1,4); (1,8);
    (4,1); (4,4); (5,9);
    (5,6); (7,-1); (7,2);
    (8,5); (11,6); (13,1)
  |]
  in
  demo_tracer_enveloppe_convexe e
