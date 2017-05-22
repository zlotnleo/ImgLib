structure Image24 :> IMAGE24 = struct
	structure T = ImageFn(type colour = Word8.word * Word8.word * Word8.word)
	open T

	exception InvalidColour
	val Black   = ((0wx00, 0wx00, 0wx00) : colour)
	val White   = ((0wxFF, 0wxFF, 0wxFF) : colour)
	val Red     = ((0wxFF, 0wx00, 0wx00) : colour)
	val Green   = ((0wx00, 0wxFF, 0wx00) : colour)
	val Blue    = ((0wx00, 0wx00, 0wxFF) : colour)
	val Yellow  = ((0wxFF, 0wxFF, 0wx00) : colour)
	val Magenta = ((0wxFF, 0wx00, 0wxFF) : colour)
	val Cyan    = ((0wx00, 0wxFF, 0wxFF) : colour)

	fun colourFromBytes v =
		if Word8Vector.length v = 3 then
			(
				Word8Vector.sub (v, 0),
				Word8Vector.sub (v, 1),
				Word8Vector.sub (v, 2)
			)
		else raise InvalidColour

	fun colourFromInts (r, g, b) =
		(
			if r >= 0 andalso r <= 255 then Word8.fromInt r else raise InvalidColour,
			if g >= 0 andalso g <= 255 then Word8.fromInt g else raise InvalidColour,
			if b >= 0 andalso b <= 255 then Word8.fromInt b else raise InvalidColour
		)

	fun toTextPPM (img as {width, height, data}) =
	let
		fun colourToStr (R, G, B) = String.concat (map (StringCvt.padLeft #" " 4 o Word8.toString) [R, G, B])
		fun printRow i oc row =
			if i = width then ()
			else (
				TextIO.output(oc, colourToStr (Array.sub(row, i)));
				printRow (i + 1) oc row
			)
		val header = "P3\n" ^ Int.toString width ^ " " ^ Int.toString height ^ "\n255\n"
	in
		textOut (printRow 0) header img
	end

	fun toBinPPM (img as {width, height, data}) =
	let
		fun printRow oc row = BinIO.output(
			oc,
			(Word8Vector.concat
				o (map (fn (R, G, B) => Word8Vector.fromList [R, G, B]))
				o List.tabulate)
					(Array.length row, fn i => (Array.sub (row, i)))
		)
		val header = "P6 " ^ Int.toString width ^ " " ^ Int.toString height ^ " 255\n"
	in
		binOut printRow header img
	end

	fun toImage8 img =
		Image8.createFunc
			(getWidth img, getHeight img)
			(fn coord =>
				(fn (r, g, b) => Image8.colourFromInt ((Word8.toInt r + Word8.toInt g + Word8.toInt b) div 3))
				(getPixel img coord)
			)

	val toImage1 = Image8.toImage1 o toImage8

	fun fromImage8 img =
		createFunc
			(Image8.getWidth img, Image8.getHeight img)
			(fn coord =>
				(fn x => (Image8.colourToWord8 x, Image8.colourToWord8 x, Image8.colourToWord8 x))
				(Image8.getPixel img coord)
			)

	val fromImage1 = fromImage8 o Image8.fromImage1
end