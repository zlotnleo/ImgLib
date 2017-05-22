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

structure Image8 :> IMAGE8 = struct
	structure T = ImageFn(type colour = Word8.word)
	open T

	exception InvalidColour
	val Black = (0wx00 : colour)
	val White = (0wxFF : colour)

	fun colourFromWord8 w = w
	fun colourFromInt i = if i >= 0 andalso i <= 255 then Word8.fromInt i else raise InvalidColour
	fun colourToWord8 c = c
	fun colourToInt c = Word8.toInt c

	fun toTextPPM (img as {width, height, data}) =
	let
		fun printRow i oc row =
			if i = width then ()
			else (
				TextIO.output(oc, (StringCvt.padLeft #" " 4 o (Word8.fmt StringCvt.DEC) o Array.sub) (row, i));
				printRow (i + 1) oc row
			)
		val header = "P2\n" ^ Int.toString width ^ " " ^ Int.toString height ^ "\n255\n"
	in
		textOut (printRow 0) header img
	end

	fun toBinPPM (img as {width, height, data}) =
	let
		fun printRow oc row = BinIO.output(oc, Word8Vector.tabulate(Array.length row, fn i => Array.sub(row, i)))
		val header = "P5 " ^ Int.toString width ^ " " ^ Int.toString height ^ " 255\n"
	in
		binOut printRow header img
	end

	fun toImage1 img =
		Image1.createFunc
			(getWidth img, getHeight img)
			(fn coord =>
				(fn x => if Word8.>= (x, 0wx80) then Image1.Black else Image1.White)
				(getPixel img coord)
			)

	fun fromImage1 img =
		createFunc
			(Image1.getWidth img, Image1.getHeight img)
			(fn coord =>
				(fn b => if Image1.isWhite b then 0wxFF else 0wx00)
				(Image1.getPixel img coord)
			)
end