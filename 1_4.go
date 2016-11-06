package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Print("f1\t")
	newton(f1, f1_prime, 0.0001, 0.1)
	fmt.Print("f2\t")
	newton(f2, f2_prime, 0.0001, 0.1)
}

// newton(f, f_prime, e, x0)
//   f       : 対象関数
//   f_prim  : fの一次導関数
//   e       : "|x_(k+1) - x_k| < e" が満たされたときに終了する
//   x0      : x0
// 表示される値
//   k    : "|x_k - x_(k-1)| < e"が満たされた、最初のkの値
//   x    : x_kの値
//   f(x) : f(x_k)の値
func newton(f func(float64) float64, f_prime func(float64) float64, e float64, x0 float64) {
	var x_prev float64 = x0
	var x_next float64

	k := 0
	for {
		x_next = x_prev - f(x_prev)/f_prime(x_prev)
		if math.Abs(x_next-x_prev) < e {
			fmt.Printf("k=%v, x=%v, f(x)=%v\n", k, x_next, f(x_next))
			break
		}
		x_prev = x_next
		k++
	}
}

func f1(x float64) float64 {
	return -2.2*math.Pow(x, 4) + 3.5*math.Pow(x, 3) + 4.1*math.Pow(x, 2) + 3.3*x - 2.7
}

func f1_prime(x float64) float64 {
	return -8.8*math.Pow(x, 3) + 10.5*math.Pow(x, 2) + 8.2*x + 3.3
}

func f2(x float64) float64 {
	return -3*math.Cos(2*x+2) + math.Exp(x+1) - 2*x - 30
}

func f2_prime(x float64) float64 {
	return 6*math.Sin(2*x+2) + math.Exp(x+1) - 2
}
