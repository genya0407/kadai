package main

// 逆行列を計算する
// A = Inverse(A)
func Inverse(A [N][N]float64) [N][N]float64 {
	// bは単位行列ではなく、単位ベクトルの配列と考える
	// b[0]はb_0を表す
	b := idMatrix()

	var x Matrix
	for k := 0; k <= N-1; k++ {
		x_j := Solve(A, b[k])
		x.setCol(k, x_j)
	}
	return x
}

// lu.goと同様に、"インスタンスメソッド"を定義している

type Matrix [N][N]float64

// X.setCols(j, vector)
// 行列Xのj列に、vectorを代入する
func (m *Matrix) setCol(j int, vector [N]float64) {
	for i := 0; i <= N-1; i++ {
		m[i][j] = vector[i]
	}
}

// N行N列の単位行列を計算する
// I = idMatrix()
func idMatrix() [N][N]float64 {
	var im [N][N]float64
	// [N][N]float64は、初期状態ですべての要素が0なので、
	// 対角成分以外は操作する必要はない。
	for k := 0; k <= N-1; k++ {
		im[k][k] = 1.0
	}
	return im
}
