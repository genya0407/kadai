package main

func Solve(A [N][N]float64, b [N]float64) [N]float64 {
	L, U := LuDecomp(A)
	y := Forward(L, b)
	x := Backward(U, y)
	return x
}
