package main

import "fmt"

// 行列を見やすく表示する
func PrintMatrix(matrix [N][N]float64) {
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			fmt.Printf("%3.3f\t", matrix[i][j])
		}
		fmt.Printf("\n")
	}
}

// ベクトルを見やすく表示する
func PrintVector(vector [N]float64) {
	for i := 0; i <= N-1; i++ {
		fmt.Printf("%3.3f\n", vector[i])
	}
}
