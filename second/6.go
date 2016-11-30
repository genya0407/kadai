package main

import "math"

func main() {
	var H [N][N]float64
	var b [N]float64

	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			H[i][j] = math.Pow(0.25, math.Abs(float64(i-j)))
		}
	}
	for i := 0; i <= N-1; i++ {
		b[i] = 5.0 - math.Pow(4.0, float64(i-N+1)) - math.Pow(4.0, float64(-i))
	}

	x := Solve(H, b)
	PrintVector(x)
}
