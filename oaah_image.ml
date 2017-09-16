open Oaah

module Make_RAW (Color : COLOR) =
struct
  module Color = Color
  type t = {
    width  : int ;
    height : int ;
    image  : Color.t array (** The bitmap itself is an array of X*Y colors *)
  }

  let make ?(default=Color.zero) width height =
    { width  = width ;
      height = height ;
      image  = Array.make (width*height) default }

  let max_byte = Color.K.of_int 255
  let byte_of_comp f =
    Color.K.to_int (Color.K.mul max_byte f)
  (** [byte_of_comp x] returns the byte value of a color component *)

  let poke t c x y a =
    if x >= 0 && x < t.width &&
       y >= 0 && y < t.height &&
       a > 0. then
      let a = Color.K.of_float a in
      let a = if Color.K.compare a Color.K.one > 0 then Color.K.one else a in
      let a'= Color.K.sub Color.K.one a in
      let o = y * t.width + x in
      let combine_comp old_comp new_comp =
        Color.K.add
          (Color.K.mul a' old_comp)
          (Color.K.mul a  new_comp) in
      t.image.(o) <- Array.mapi (fun i p ->
        combine_comp p c.(i)) t.image.(o)

  let poke_scanline t c x1 x2 y a =
    for x = x1 to x2 do
      poke t c x y a
    done

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

  let open_graph t =
    let x11_size = Printf.sprintf " %dx%d" t.width t.height in
    Graphics.open_graph x11_size

  let draw t =
    let open Graphics in
    for x = 0 to t.width-1 do
      for y = 0 to t.height-1 do
        let c = t.image.(y * t.width + x) in
        let color = rgb (byte_of_comp c.(0))
                        (byte_of_comp c.(1))
                        (byte_of_comp c.(2)) in
        set_color color ;
        plot x (t.height - 1 - y)
      done
    done

end

module Make (Color : COLOR) : IMAGE with module Color = Color = Make_RAW (Color)
