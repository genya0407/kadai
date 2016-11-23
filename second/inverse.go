package main

func Inverse(A [N][N]float64) [N][N]float64 {
	// bsは単位行列ではなく、基本ベクトルの配列と考える
	// bs[0]はb0を表す
	bs := fundamentalMatrix()

	var xs [N][N]float64
	for k := 0; k <= N-1; k++ {
		x := Solve(A, bs[k])
		xs = setCols(xs, k, x)
	}

	return xs
}

// X = setCols(X, j, vector)
// Xのj列に、vectorを代入する
func setCols(X [N][N]float64, j int, vector [N]float64) [N][N]float64 {
	for k := 0; k <= N-1; k++ {
		X[k][j] = vector[k]
	}
	return X
}

func fundamentalMatrix() [N][N]float64 {
	var fm [N][N]float64
	for k := 0; k <= N-1; k++ {
		fm[k][k] = 1
	}
	return fm
}
