package main

// 後退代入によって、方程式の解を求める
// x := Backward(U, c)
func Backward(U [N][N]float64, c [N]float64) [N]float64 {
	var x [N]float64
	for i := N - 1; 0 <= i; i-- {
		sum := 0.0
		for k := i + 1; k <= N-1; k++ {
			sum += U[i][k] * x[k]
		}
		x[i] = (c[i] - sum) / U[i][i]
	}
	return x
}
