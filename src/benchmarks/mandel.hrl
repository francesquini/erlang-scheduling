-define(Mult(X0, X1, Y0, Y1), {X0 * Y0 - X1 * Y1,  X0 * Y1 + Y0 * X1}).
-define(Sum(X0, X1, Y0, Y1), {X0 + Y0,  X1 + Y1}).
-define(Norma(Z0, Z1), Z0 * Z0 + Z1 * Z1). 

-record(
   mandel,
   {
		size,
		delta,
		max_complex,
		max_iter,
		max_colors,
		pallete,
		bailout2	
	}
).