signature IMAGE = sig
	include BASE_IMAGE
	exception InvalidColour
	val toBinPPM : image -> string -> unit
	val toTextPPM : image -> string -> unit
end