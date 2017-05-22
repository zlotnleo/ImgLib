functor ImageFn(type colour) : BASE_IMAGE = struct
	type colour = colour
	type image = {width : int, height : int, data : colour array array}

	fun createFunc (w, h) f = {
		width = w,
		height = h,
		data = Array.tabulate(
			h,
			fn y => Array.tabulate(
				w, fn x => f (x, y)
			)
		)
	}
	fun createBlank (w, h) clr = createFunc (w, h) (fn _ => clr)

	fun getHeight {height, width, data} = height
	fun getWidth {height = _, width, data} = width

	fun getPixel ({width = _, height = _, data = data} : image) (x, y) = Array.sub (Array.sub (data, y), x)

	fun drawPixel {width, height, data} clr (x, y) = Array.update (Array.sub (data, y), x, clr)

	fun drawLine img clr (x0, y0) (x1, y1) =
	let
		val dx = Int.abs(x1 - x0)
		val dy = Int.abs(y1 - y0)
		val sx = if x0 < x1 then 1 else ~1;
		val sy = if y0 < y1 then 1 else ~1;

		fun helper x y err = (
			drawPixel img clr (x, y);
			if (x = x1) andalso (y = y1) then ()
			else
				if 2 * err > ~dy then
					if 2 * err < dx then helper (x + sx) (y + sy) (err - dy + dx)
					else                 helper (x + sx) y (err - dy)
				else
					if 2 * err < dx then helper x (y + sy) (err + dx)
					else                 ()
		)
	in
		helper x0 y0 (dx - dy)
	end

	fun drawAll {width, height, data} f = Array.appi (fn (y, row) => (Array.modifyi (fn (x, _) => f(x, y)) row)) data

	fun insert img (x0, y0) (img2 as {width, height, data = _}) =
	drawAll img
	(fn (x, y) =>
		if x < x0 orelse y < y0 orelse x >= x0 + width orelse y >= y0 + height then
			getPixel img (x, y)
		else
			getPixel img2 (x - x0, y - y0)
	)
end