package main

import "math"
import "fmt"

func main() {
	var H [N][N]float64
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			H[i][j] = math.Pow(0.25, math.Abs(float64(i-j)))
		}
	}

	I1 := MatMul(H, Inverse(H))
	I2 := MatMul(Inverse(H), H)
	fmt.Println("I1")
	PrintMatrix(I1)
	fmt.Println("I2")
	PrintMatrix(I2)
}

func MatMul(A [N][N]float64, B [N][N]float64) [N][N]float64 {
	var retMatrix [N][N]float64
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			for k := 0; k <= N-1; k++ {
				retMatrix[i][j] += A[i][k] * B[k][j]
			}
		}
	}
	return retMatrix
}
