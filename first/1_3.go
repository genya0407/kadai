package main

import (
	"./locust" // Calculation関数, WriteCSV関数
)

func main() {
	// s=0.15, c=0.7, 0.5, 0.45
	// の各場合について、バッタGの移動確率を求めている
	locust.WriteCSV(
		"c_0.7",
		locust.Calculation(0.7, 0.15),
	)
	locust.WriteCSV(
		"c_0.5",
		locust.Calculation(0.5, 0.15),
	)
	locust.WriteCSV(
		"c_0.45",
		locust.Calculation(0.45, 0.15),
	)
}
