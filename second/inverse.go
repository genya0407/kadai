package main

// 逆行列を計算する
// A = Inverse(A)
func Inverse(A [N][N]float64) [N][N]float64 {
	// bsは単位行列ではなく、基本ベクトルの配列と考える
	// bs[0]はb0を表す
	bs := idMatrix()

	var xs Matrix
	for k := 0; k <= N-1; k++ {
		x := Solve(NewTargetMatrix(A), bs[k])
		xs.setCol(k, x)
	}
	return xs
}

/*
func (v SomeType) FuncName(ArgType) ReturnType {
	v.someTypeFunc() // vは関数読み出し元のオブジェクト
	...
}

と書くと、SomeType型の"インスタンス"に関数を生やす事ができる。

a := SomeType{}
a.FuncName(arg)

すなわち、いわゆる"インスタンスメソッド"の定義ができる。
ただし、golangでは、組み込み型に新たに関数を生やすことができないので、
ここではMatrixという型を新たに定義し、その型に関数を生やしている。
*/

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
	// imは、初期状態ですべての要素が0.0なので、
	// 対角成分以外は操作する必要はない。
	for k := 0; k <= N-1; k++ {
		im[k][k] = 1.0
	}
	return im
}
