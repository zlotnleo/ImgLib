signature IMAGE1 = sig
	include IMAGE
	val Black : colour
	val White : colour
	val isWhite : colour -> bool
	val isBlack : colour -> bool
end

structure Image1 :> IMAGE1 = struct
	structure T = ImageFn(type colour = bool)
	open T

	exception InvalidColour
	val Black = false
	val White = true

	fun isWhite b = b
	fun isBlack b = not b

	fun toTextPPM (img as {width, height, data}) =
	let
		fun printRow i oc row =
			if i = width then ()
			else (
				TextIO.output(oc, if Array.sub(row, i) then " 1" else " 0");
				printRow (i + 1) oc row
			)
		val header = "P1\n" ^ Int.toString width ^ " " ^ Int.toString height ^ "\n"
	in
		textOut (printRow 0) header img
	end

	fun toBinPPM (img as {width, height, data}) =
	let
		fun boolListToWord8 l =
		let
			fun helper [] 0 acc = acc
			|	helper _ 0 _ = raise Overflow
			|	helper [] n acc = helper [] (n - 1) (Word8.<< (acc, 0wx1))
			|	helper (x::xs) n acc = helper xs (n - 1) (Word8.orb (Word8.<< (acc, 0wx1), if x then 0wx1 else 0wx0))
		in
			helper l 8 0wx0
		end

		fun softTake _ 0 = []
		|	softTake [] _ = []
		|	softTake (x::xs) n = x :: (softTake xs (n - 1))

		fun softDrop xs 0 = xs
		|	softDrop [] _ = []
		|	softDrop (x::xs) n = softDrop xs (n - 1)

		fun toWord8List [] acc = List.rev acc
		|	toWord8List l  acc = toWord8List (softDrop l 8) ((boolListToWord8 (softTake l 8)) :: acc) 

		fun printRow oc row = BinIO.output(oc, Word8Vector.fromList (toWord8List (Array.foldr (op::) [] row) []))

		val header = "P4 " ^ Int.toString width ^ " " ^ Int.toString height ^ "\n"
	in
		binOut printRow header img
	end
end