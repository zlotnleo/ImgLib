fun textOut printRow header {width, height, data} filename =
let
	val oc = TextIO.openOut filename
	fun out i = 
		if i = height then ()
		else (
			printRow oc (Array.sub (data, i));
			TextIO.output(oc, "\n");
			out (i + 1)
		)
	in
		TextIO.output(oc, header);
		out 0;
		TextIO.closeOut oc
	end

fun binOut printRow header {width, height, data} filename =
let
	val oc = BinIO.openOut filename
	fun out i = 
		if i = height then ()
		else (
			printRow oc (Array.sub (data, i));
			out (i + 1)
		)
in
	BinIO.output(oc, Byte.stringToBytes header);
	out 0;
	BinIO.closeOut oc
end