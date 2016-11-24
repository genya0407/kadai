package main

// Ax = b
// という形の方程式の解（x）を求める
// x := Solve(A, b)
func Solve(A TargetMatrix, b [N]float64) [N]float64 {
	L, U := A.Decomp()
	y := Forward(L, b)
	x := Backward(U, y)
	return x
}
