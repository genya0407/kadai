package main

import "fmt"
import "./forward"
import "./backward"
import "./lu"

const N = 3

func main() {
	A := [N][N]float64{
		{3, 1, -3},
		{-3, -3, 2},
		{-6, -8, 5},
	}
	b := [N]float64{9, -1, 2}

	L, U := lu.LuDecomp(A)
	y := forward.Forward(L, b)
	x := backward.Backward(U, y)
	fmt.Println("x:")
	PrintVector(x)
}

func PrintVector(vector [N]float64) {
	for i := 0; i <= N-1; i++ {
		fmt.Printf("%3.3f\n", vector[i])
	}
}
