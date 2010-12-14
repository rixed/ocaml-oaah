(* Some modules to play with *)

module K = Algen_impl.IntField (struct let v = 0 end)
module Color = Algen_vector.Make (K) (struct let v = 3 end)
module Img = Oaah_image.Make (Color)

let white = [| 1 ; 1 ; 1 |]
let black = [| 0 ; 0 ; 0 |]

let show_white () =
	let image = Img.make ~default:white 300 200 in
	Img.show image

let various_opacity () =
	let image = Img.make ~default:white 4 3 in
	let put_pixel = Img.poke image black in
	put_pixel 2 1 1. ;
	put_pixel 1 2 0.5 ;
	put_pixel 3 2 0.75 ;
	put_pixel 0 1 0. ;
	Img.show image

let () =
	show_white () ;
	various_opacity ()

