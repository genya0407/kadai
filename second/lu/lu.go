package lu

import "fmt"

const N = 3

type Tuple struct {
	I int
	J int
}

// 内部的にはLとUを連想配列として持つ
// LuDecomp関数から値を返すときに初めて[N][N]float64にする
// Tuple{i,j}が、i行j列目の要素のkeyだと考える
var _L map[Tuple]float64
var _U map[Tuple]float64
var _A [N][N]float64

func main() {
	A := [N][N]float64{
		{3, 1, -3},
		{-3, -3, 2},
		{-6, -8, 5},
	}
	L, U := LuDecomp(A)
	fmt.Println("L")
	PrintMatrix(L)
	fmt.Println("U")
	PrintMatrix(U)
}

func PrintMatrix(matrix [N][N]float64) {
	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			fmt.Printf("%3.3f\t", matrix[i][j])
		}
		fmt.Printf("\n")
	}
}

// L, U := LuDecomp(A)
func LuDecomp(A [N][N]float64) ([N][N]float64, [N][N]float64) {
	// _Lと_Uと_Aを初期化
	_L = map[Tuple]float64{}
	_U = map[Tuple]float64{}
	_A = A
	for i := 0; i <= N-1; i++ {
		_L[Tuple{i, i}] = 1
	}
	for j := 0; j <= N-1; j++ {
		_U[Tuple{0, j}] = _A[0][j]
	}

	var retL [N][N]float64
	var retU [N][N]float64

	for i := 0; i <= N-1; i++ {
		for j := 0; j <= N-1; j++ {
			index := Tuple{i, j}
			retL[i][j] = calcL(index)
			retU[i][j] = calcU(index)
		}
	}

	return retL, retU
}

// tupleの値に対応する_Lの値を返す
// _Lにその値が格納されていなかった場合、
// 同時に_Lにその値を格納する
func calcL(tuple Tuple) float64 {
	value, ok := _L[tuple]
	if ok {
		return value
	} else {
		// tuple.I, tuple.Jと毎回書くと読みづらいので
		i := tuple.I
		j := tuple.J

		l := 0.0
		if i > j {
			sum := 0.0
			for k := 0; k <= j-1; k++ {
				sum += calcL(Tuple{i, k}) * calcU(Tuple{k, j})
			}
			l = (1.0 / calcU(Tuple{j, j})) * (_A[i][j] - sum)
		} else {
			l = 0.0
		}
		_L[tuple] = l
		return l
	}
}

// tupleの値に対応する_Uの値を返す
// _Uにその値が格納されていなかった場合、
// 同時に_Uにその値を格納する
func calcU(tuple Tuple) float64 {
	value, ok := _U[tuple]
	if ok {
		return value
	} else {
		// tuple.I, tuple.Jと毎回書くと読みづらいので
		i := tuple.I
		j := tuple.J

		u := 0.0
		if i <= j {
			sum := 0.0
			for k := 0; k <= i-1; k++ {
				sum += calcL(Tuple{i, k}) * calcU(Tuple{k, j})
			}
			u = _A[i][j] - sum
		} else {
			u = 0.0
		}
		_U[tuple] = u
		return u
	}
}
