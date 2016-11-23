package main

// y := Forward(L, b)
func Forward(L [N][N]float64, b [N]float64) [N]float64 {
	// 方程式の解
	var y [N]float64

	// yを求めるループ
	for i := 0; i < N; i++ {

		//sumは
		//	yi = bi - sum
		//と置いたときのsum
		sum := 0.0
		// sumの値を求めるループ
		for k := 0; k <= i-1; k++ {
			sum += L[i][k] * y[k]
		}

		y[i] = b[i] - sum
	}

	return y
}
