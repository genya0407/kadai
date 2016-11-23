package main

// x := Backward(U, c)
func Backward(U [N][N]float64, c [N]float64) [N]float64 {
	// 解を表すベクトル
	var x [N]float64

	// x[i]を求めるループ
	for i := N - 1; 0 <= i; i-- {
		//sumは
		//  xi = (ci - sum)/uii
		//と置いたときのsum
		sum := 0.0
		//sumを求めるループ
		for k := i + 1; k <= N-1; k++ {
			sum += U[i][k] * x[k]
		}

		x[i] = (c[i] - sum) / U[i][i]
	}

	return x
}
