signature BASE_IMAGE = sig
	type colour
	type image

	val createFunc : (int * int) -> (int * int -> colour) -> image
	val createBlank : (int * int) -> colour -> image

	val getWidth : image -> int
	val getHeight : image -> int

	val getPixel : image -> int * int -> colour

	val drawPixel : image -> colour -> (int * int) -> unit
	val drawLine : image -> colour -> (int * int) -> (int * int) -> unit
	val drawAll : image -> (int * int -> colour) -> unit

	val insert : image -> int * int -> image -> unit
end