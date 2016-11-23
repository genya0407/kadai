package main

// 配列の内容などを標準出力に表示するライブラリ
import "github.com/k0kubun/pp"

const N = 3

func main() {
	U := [N][N]float64{
		{3, 1, -3},
		{0, -2, -1},
		{0, 0, 2},
	}
	c := [N]float64{9, 8, -4}
	x := Backward(U, c)
	pp.Println(x)
}
