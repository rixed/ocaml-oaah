module Make (K : Algen_intf.FIELD) : Oaah.COLOR with module K = K =
struct
  include Algen_vector.Make (K) (struct let v = 3 end)

  let white = [| K.one ; K.one ; K.one |]
  let black = [| K.zero ; K.zero ; K.zero |]
end
