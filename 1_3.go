package main

import (
	"./locust"
	"fmt"
)

func main() {
	for _, c := range [3]float64{0.7, 0.5, 0.45} {
		res := locust.Calculation(c, 0.15)
		locust.WriteCSV("c_"+fmt.Sprint(c), res)
	}
}
