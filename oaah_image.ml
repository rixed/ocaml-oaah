open Bricabrac
open Oaah

module Make_RAW (Color : COLOR) =
struct
	module Color = Color
	type t = {
		width  : int ;
		height : int ;
		(* The bitmap itself is an array of X*Y colors *)
		image  : Color.t array }
	
	let make ?(default=Color.zero) width height =
		{ width  = width ;
		  height = height ;
		  image  = Array.make (width*height) default }
	
	let max_byte = Color.K.of_int 255
	let byte_of_comp f =
		Color.K.to_int (Color.K.mul max_byte f)
	(* [byte_of_comp x] returns the byte value of a color component *)

	let poke t c x y a =
		if x >= 0 && x < t.width &&
		   y >= 0 && x < t.height &&
		   a > 0. then
			let a = Color.K.of_float a in
			let a' = Color.K.sub Color.K.one a in
			let o = y * t.width + x in
			let combine_comp old_comp new_comp =
				Color.K.add
					(Color.K.mul a' old_comp)
					(Color.K.mul a  new_comp) in
			let new_col = array_zip combine_comp t.image.(o) c in
			t.image.(o) <- new_col

	let save t ?(format=PNM) filename =
		let ochan = open_out_bin filename in
		let write_color c =
			for i = 0 to 2 do
				output_byte ochan (byte_of_comp c.(i))
			done in
		begin match format with
		| PNM ->
			Printf.fprintf ochan "P6\n%d %d\n255\n" t.width t.height ;
			Array.iter write_color t.image
		end ;
		close_out ochan

	let img_viewer =
		try Sys.getenv "IMG_VIEWER"
		with Not_found -> "display"

	let show t =
		let tempname = Filename.temp_file "oaah" ".pnm" in
		save t tempname ;
		let cmd = Printf.sprintf "%s %s" img_viewer tempname in
		ignore (Sys.command cmd) ;
		Sys.remove tempname

end

module Make (Color : COLOR) : IMAGE with module Color = Color = Make_RAW (Color)