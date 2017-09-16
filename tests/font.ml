(* Some modules to play with *)

module Kcol = Algen_impl.FloatField
module Color = Oaah_color.Make (Kcol)
module Img = Oaah_image.Make (Color)

module K = Algen_impl.IntField (struct let v = 8 end)
module Vector = Algen_vector.Make (K) (struct let v = 2 end)
module Point = Geom_shapes.Point (Vector)
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Algo = Geom_algo.Algorithms (Poly) (Path)
module Glyph = Text_impl.Glyph (Poly) (Path)
module Word = Text_impl.Word (Glyph)

let display t =
  Img.open_graph t ;
  Img.draw t ;
  ignore (Graphics.(wait_next_event [Button_down; Key_pressed])) ;
  Graphics.close_graph ()

let white = [| Kcol.one ; Kcol.one ; Kcol.one |]
let black = Color.zero
let width, height = 400, 400

let poke_segment image color x_start x_stop y alpha =
  let a = K.to_float alpha in
  for x = x_start to x_stop do
    Img.poke image color x (height - y - 1) a
  done

let word () =
  let image = Img.make ~default:white width height in
  let word = Word.make "Hello World!" in
  let ppolys = Word.to_polys word (K.of_float 0.15) in
  let bbox_diag = Point.Bbox.diagonal (Word.bbox word) in
  let min_scale = K.of_float 0.05 in
  let next_scale scale =
    let ratio = K.of_float 0.8 in
    K.mul ratio scale in
  let next_y scale y =
    let margin = K.of_int 3 in
    let y = K.add y (K.mul bbox_diag.(1) scale) in
    K.add margin y in
  let rec at_scale y scale =
    let draw_polys (pos, polys) =
      let trans = Point.add [| K.zero ; y |] (Point.mul scale pos) in
      let polys = Algo.translate_poly
        (Algo.scale_poly polys Point.zero scale)
        trans in
      Algo.rasterize polys (poke_segment image black) in
    if K.compare scale min_scale > 0 then begin
      List.iter draw_polys ppolys ;
      at_scale (next_y scale y) (next_scale scale)
    end in
  at_scale K.zero (K.of_float 3.) ;
  display image

let () =
  word ()

