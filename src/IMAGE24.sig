signature IMAGE24 = sig
	include IMAGE
	val Black : colour
	val White : colour
	val Red : colour
	val Green : colour
	val Blue : colour
	val Yellow : colour
	val Magenta : colour
	val Cyan : colour

	val colourFromBytes : Word8Vector.vector -> colour
	val colourFromInts : int * int * int -> colour

	val toImage8 : image -> Image8.image
	val toImage1 : image -> Image1.image

	val fromImage8 : Image8.image -> image
	val fromImage1 : Image1.image -> image
end