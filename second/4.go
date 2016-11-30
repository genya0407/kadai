package main

import "fmt"

const N = 3

func main() {
	A := TargetMatrix{
		{3, 1, -3},
		{-3, -3, 2},
		{-6, -8, 5},
	}
	L, U := A.Decomp()
	fmt.Println("L")
	PrintMatrix(L)
	fmt.Println("U")
	PrintMatrix(U)
}
