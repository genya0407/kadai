package main

// inverse.goと同様に、"インスタンスメソッド"を定義している

type Tuple struct {
	I int
	J int
}

type TargetMatrix struct {
	matrix     [N][N]float64
	cacheLower map[Tuple]float64
	cacheUpper map[Tuple]float64
}

func NewTargetMatrix(A [N][N]float64) TargetMatrix {
	return TargetMatrix{
		matrix:     A,
		cacheLower: map[Tuple]float64{},
		cacheUpper: map[Tuple]float64{},
	}
}

// L, U := A.Decomp()
// L: 下三角行列, U: 上三角行列
// A.lowerとA.upper（後に定義）の再帰呼び出しを利用して、
// 行列Aの下三角行列と上三角行列を計算する。
func (a TargetMatrix) Decomp() ([N][N]float64, [N][N]float64) {
	var L, U [N][N]float64
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			L[i][j] = a.lower(i, j)
			U[i][j] = a.upper(i, j)
		}
	}
	return L, U
}

// A.lower(i, j)
// 下三角行列のi行j列要素を計算する
// 内部でA.upperを呼び出している。
func (a TargetMatrix) lower(i int, j int) float64 {
	value, ok := a.cacheLower[Tuple{i, j}]
	if ok {
		return value
	} else {
		// 対角成分は1.0、右上半分は0.0となるように、
		// iとjの値をもとに分岐している
		l := 0.0
		if i > j {
			sum := 0.0
			for k := 0; k <= j-1; k++ {
				sum += a.lower(i, k) * a.upper(k, j)
			}
			l = (1.0 / a.upper(j, j)) * (a.matrix[i][j] - sum)
		} else if i == j {
			l = 1.0
		} else {
			l = 0.0
		}
		a.cacheLower[Tuple{i, j}] = l
		return l
	}
}

// A.upper(i, j)
// 上三角行列のi行j列要素を計算する
// 内部でA.lowerを呼び出している。
func (a TargetMatrix) upper(i int, j int) float64 {
	// キャッシュを参照して、該当がなければ計算し、キャッシュする
	value, ok := a.cacheUpper[Tuple{i, j}]
	if ok {
		return value
	} else {
		// 左下半分は0.0となるように、
		// iとjの値をもとに分岐している
		u := 0.0
		if i <= j {
			sum := 0.0
			for k := 0; k <= i-1; k++ {
				sum += a.lower(i, k) * a.upper(k, j)
			}
			u = (a.matrix[i][j] - sum)
		} else {
			u = 0.0
		}
		a.cacheUpper[Tuple{i, j}] = u
		return u
	}
}
