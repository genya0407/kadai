package main

// 前進代入によって、方程式の解を求める
// y := Forward(L, b)
func Forward(L [N][N]float64, b [N]float64) [N]float64 {
	var y [N]float64
	for i := 0; i < N; i++ {
		sum := 0.0
		for k := 0; k <= i-1; k++ {
			sum += L[i][k] * y[k]
		}
		y[i] = b[i] - sum
	}
	return y
}
