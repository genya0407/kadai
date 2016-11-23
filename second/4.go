package main

import "fmt"

const N = 3

func main() {
	A := [N][N]float64{
		{3, 1, -3},
		{-3, -3, 2},
		{-6, -8, 5},
	}
	L, U := LuDecomp(A)
	fmt.Println("L")
	PrintMatrix(L)
	fmt.Println("U")
	PrintMatrix(U)
}

func PrintMatrix(matrix [N][N]float64) {
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			fmt.Printf("%3.3f\t", matrix[i][j])
		}
		fmt.Printf("\n")
	}
}
