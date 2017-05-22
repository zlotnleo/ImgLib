signature IMAGE8 = sig
	include IMAGE
	val Black : colour
	val White : colour
	
	val colourFromWord8 : Word8.word -> colour
	val colourFromInt : int -> colour
	val colourToWord8 : colour -> Word8.word
	val colourToInt : colour -> int

	val toImage1 : image -> Image1.image
	val fromImage1 : Image1.image -> image
end