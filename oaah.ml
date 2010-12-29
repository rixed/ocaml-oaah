(* Small lib to generate good looking anti aliased bitmap pictures.
 * Designed to be ued in combination with the geom library.
 * Both draw inspiration from the excelent cl-vector library. *)

type file_format = PNM

(*
type color_spec =
	{ r_mask = int32 ; g_mask = int32 ; b_mask = int32 }

let format_rgb16 =
	{ r_mask = 0b11111_000000_00000L ;
	  g_mask = 0b00000_111111_00000L ;
	  b_mask = 0b00000_000000_11111L }

let format_rgb =
	{ r_mask = 0xFF_00_00_00L ;
	  g_mask = 0x00_FF_00_00L ;
	  b_mask = 0x00_00_FF_00L }

let format_abgr =
	{ r_mask = 0x00_00_00_FFL ;
	  g_mask = 0x00_00_FF_00L ;
	  b_mask = 0x00_FF_00_00L ;
	  a_mask = 0xFF_00_00_00L }
*)

(* Any vector space of at least 3 dimention is usable for colors, with (0,0,0) meaning black
 * and (1,1,1) white *)
module type COLOR =
sig
	include Algen_intf.VECTOR
end

module type IMAGE =
sig
	module Color : COLOR	(* What color format to use *)
	type t

	val make  : ?default:Color.t -> int -> int -> t
	val show  : t -> unit
	(* [show img] will display [img] using the external viewer specified with the IMG_VIEWER
	 * environment variable. *)
	val save  : t -> ?format:file_format -> string -> unit
	val poke  : t -> Color.t -> int -> int -> float -> unit
	(* [poke img color x y alpha] draws this color at the given pixel location.
	 * Note that (x, y) = (0, 0) corresponds to the upper left corner.
	 * Alpha is supposed to be between 0 (transparent) to 1 (opaque). *)
end
