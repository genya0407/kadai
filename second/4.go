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

// 行列を見やすく表示する
func PrintMatrix(matrix [N][N]float64) {
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			fmt.Printf("%3.3f\t", matrix[i][j])
		}
		fmt.Printf("\n")
	}
}
