
(* ex 2.3 solution fournie par

http://programmer-avec-ocaml.lri.fr/solutions/ex2_3.ml.html

Retouches coloration
--------------------------------------------------------

Dessin de l'ensemble de Mandelbrot, avec des couleurs.

2.3 On peut embellir le dessin de l’ensemble de Mandelbrot en donnant aux
points n’appartenant pas à l’ensemble une couleur qui dépend de la première valeur
de i pour laquelle x 2 i + y i 2 > 4. On pourra par exemple choisir la couleur par
une interpolation linéaire entre deux couleurs prédéfinies en utilisant la fonction
Graphics.rgb (voir section 2.10 Un casse-briques sans briques) et une simple règle
de trois.

http://programmer-avec-ocaml.lri.fr/solutions/ex2_3.ml.html
*)

open Graphics

let width = 800
let height = 800
let k = 100

(* On choisit ici de donner une couleur qui est une nuance de gris,
   avec un gris d'autant plus foncé que le nombre d'itérations i est petit.
   Cela donne un joli contraste avec l'ensemble lui-même, qui est noir.

   Bien entendu, on peut aussi mettre de la couleur. Il suffit pour cela
   de remplacer "rgb f f f" par autre chose. On peut aussi choisir une
   formule plus subtile qu'une règle de trois. *)

(* let color i =
  if i = k then black
  else let f = truncate (255. *. float i /. float k) in rgb f f f *)

let color i =
  if i = k then white
  else let f = truncate (255. *. (float k -. float i) /. float k) in rgb f f f

(* let color i =
  if i = k then white
  else let f = truncate (255. *. float i /. float k) in rgb f f f *)



let norm2 x y = x *. x +. y *. y

let mandelbrot a b =
  let rec mandel_rec x y i =
    if i = k || norm2 x y > 4. then
      color i
    else
      let x' = x *. x -. y *. y +. a in
      let y' = 2. *. x *. y +. b in
      mandel_rec x' y' (i + 1)
  in
  mandel_rec 0. 0. 0

let draw () =
  for w = 0 to width - 1 do
    for h = 0 to height - 1 do
      let a = 4. *. float w /. float width -. 2. in
      let b = 4. *. float h /. float height -. 2. in
      set_color (mandelbrot a b);
      plot w h
    done
  done

let () =
  let dim = Printf.sprintf " %dx%d" width height in
  open_graph dim;
  draw ();
  ignore (read_key ())
