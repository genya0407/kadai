package main

const N = 3

func main() {
	A := [N][N]float64{
		{3, 1, -3},
		{-3, -3, 2},
		{-6, -8, 5},
	}
	b := [N]float64{9, -1, 2}
	x := Solve(A, b)

	PrintVector(x)
}
