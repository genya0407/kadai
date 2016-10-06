package main

import "./matmul"

const N = matmul.N

func main() {
	A := [N][N]float64{
		{1, 4, 7},
		{2, 5, 8},
		{3, 6, 9},
	}
	b1 := [N]float64{1, 2, 3}
	b2 := [N]float64{4, 5, 6}
	b3 := [N]float64{7, 8, 9}

	matmul.MatVec(A, b1)
	matmul.MatVec(A, b2)
	matmul.MatVec(A, b3)
}
