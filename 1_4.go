package main

import (
	"fmt"
	"math"
)

func main() {
	newton_f1()
	newton_f2()
}

func newton_f1() {
	var x_prev float64 = 0.1
	var x_next float64

	i := 0
	for {
		x_next = x_prev - f1(x_prev)/f1_prime(x_prev)
		if math.Abs(x_next-x_prev) < 0.0001 {
			fmt.Printf("i=%v, x=%v, f1(x)=%v\n", i, x_next, f1(x_next))
			break
		}
		x_prev = x_next
		i++
	}
}

func f1(x float64) float64 {
	return -2.2*math.Pow(x, 4) + 3.5*math.Pow(x, 3) + 4.1*math.Pow(x, 2) + 3.3*x - 2.7
}

func f1_prime(x float64) float64 {
	return -8.8*math.Pow(x, 3) + 10.5*math.Pow(x, 2) + 8.2*x + 3.3
}

func newton_f2() {
	var x_prev float64 = 0.1
	var x_next float64

	i := 0
	for {
		x_next = x_prev - f2(x_prev)/f2_prime(x_prev)
		if math.Abs(x_next-x_prev) < 0.0001 {
			fmt.Printf("i=%v, x=%v, f2(x)=%v\n", i, x_next, f2(x_next))
			break
		}
		x_prev = x_next
		i++
	}
}

func f2(x float64) float64 {
	return -3*math.Cos(2*x+2) + math.Exp(x+1) - 2*x - 30
}

func f2_prime(x float64) float64 {
	return 6*math.Sin(2*x+2) + math.Exp(x+1) - 2
}
