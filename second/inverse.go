package main

func Inverse(A [N][N]float64) [N][N]float64 {
	// bsは単位行列ではなく、基本ベクトルの配列と考える
	// bs[0]はb0を表す
	bs := fundamentalMatrix()

	var xs Matrix
	for k := 0; k <= N-1; k++ {
		x := Solve(A, bs[k])
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
	for k := 0; k <= N-1; k++ {
		m[k][j] = vector[k]
	}
}

// N行N列の単位行列を計算する
func fundamentalMatrix() [N][N]float64 {
	var fm [N][N]float64
	for k := 0; k <= N-1; k++ {
		fm[k][k] = 1
	}
	return fm
}
