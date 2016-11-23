package forward

// 配列の内容などを標準出力に表示するライブラリ
import "github.com/k0kubun/pp"

// 行列・ベクトルの長さ
const N = 3

/*
[N][N]float64{
	{1,2,3},
	{4,5,6},
	{7,8,9}
}
は、
|1 2 3|
|4 5 6|
|7 8 9|
を表す。
*/

/*
[N]float64{1,2,3}
は、
|1|
|2|
|3|
を表すと解釈する（縦ベクトル）場合と、
|1 2 3|
を表すと解釈する（横ベクトル）場合がある。
*/

/*
iは行の指定に使う。
jは列の指定に使う。
*/

func main() {
	L := [N][N]float64{
		{1, 0, 0},
		{-1, 1, 0},
		{-2, 3, 1},
	}
	b := [N]float64{9, -1, 2}
	y := Forward(L, b)
	pp.Println(y)
}

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
