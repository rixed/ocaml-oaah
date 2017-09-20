(* Some modules to play with *)

module Kcol = Algen_impl.IntField (struct let v = 8 end)
module Color = Oaah_color.Make (Kcol)
module Img = Oaah_image.Make (Color)

module K = Algen_impl.IntField (struct let v = 8 end)
module Vector = Algen_vector.Make (K) (struct let v = 2 end)
module Point = Geom_shapes.Point (Vector)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Algo = Geom_algo.Algorithms (Poly) (Path)

let display t =
  Img.open_graph t ;
  Img.draw t ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

let white = [| Kcol.one ; Kcol.one ; Kcol.one |]
let black = Color.zero

let various_opacity () =
  let image = Img.make ~default:white 4 3 in
  let put_pixel = Img.poke image black 1. in
  put_pixel 2 1 1. ;
  put_pixel 1 2 0.5 ;
  put_pixel 3 2 0.75 ;
  put_pixel 0 1 0. ;
  display image

let poly1 = List.fold_left Poly.insert_after Poly.empty
  [ [| K.of_int 200 ; K.of_int  50 |] ;
    [| K.of_int 250 ; K.of_int 150 |] ;
    [| K.of_int  50 ; K.of_int 100 |] ]

let polygon () =
  let image = Img.make ~default:white 300 200 in
  Algo.rasterize (fun x1 x2 y a ->
    Img.poke_scanline image black 1. x1 x2 y (K.to_float a)) [ poly1 ] ;
  display image

let hole () =
  let image = Img.make ~default:white 300 200 in
  let center = [| K.of_int 150 ; K.of_int 100 |] in
  let poly2 = Poly.scale ~center (K.of_float 0.6) poly1 in
  let poly2 = Algo.reverse_single poly2 in
  Algo.rasterize (fun x1 x2 y a ->
    Img.poke_scanline image black 1. x1 x2 y (K.to_float a)) [ poly1 ; poly2 ] ;
  display image

let () =
  various_opacity () ;
  polygon () ;
  hole ()
