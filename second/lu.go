package main

// inverse.goと同様に、"インスタンスメソッド"を定義している

type TargetMatrix [N][N]float64

// L, U := A.Decomp()
// L: 下三角行列, U: 上三角行列
func (a TargetMatrix) Decomp() ([N][N]float64, [N][N]float64) {
	var retL, retU [N][N]float64
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			retL[i][j] = a.lower(i, j)
			retU[i][j] = a.upper(i, j)
		}
	}
	return retL, retU
}

// A.lower(i, j)
// 下三角行列のi行j列要素を計算する
func (a TargetMatrix) lower(i int, j int) float64 {
	if i > j {
		sum := 0.0
		for k := 0; k <= j-1; k++ {
			sum += a.lower(i, k) * a.upper(k, j)
		}
		return (1.0 / a.upper(j, j)) * (a[i][j] - sum)
	} else if i == j {
		return 1.0
	} else {
		return 0.0
	}
}

// A.upper(i, j)
// 上三角行列のi行j列要素を計算する
func (a TargetMatrix) upper(i int, j int) float64 {
	if i <= j {
		sum := 0.0
		for k := 0; k <= i-1; k++ {
			sum += a.lower(i, k) * a.upper(k, j)
		}
		return (a[i][j] - sum)
	} else {
		return 0.0
	}
}
