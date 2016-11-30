package main

const N = 3

func main() {
	U := [N][N]float64{
		{3, 1, -3},
		{0, -2, -1},
		{0, 0, 2},
	}
	c := [N]float64{9, 8, -4}
	x := Backward(U, c)
	PrintVector(x)
}
