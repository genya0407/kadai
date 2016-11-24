package main

func Solve(A TargetMatrix, b [N]float64) [N]float64 {
	L, U := A.Decomp()
	y := Forward(L, b)
	x := Backward(U, y)
	return x
}
