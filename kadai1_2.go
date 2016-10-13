package main

import (
	"./matmul"
	"encoding/csv"
	"fmt"
	"os"
)

// i => 行, j => 列とする
/* ex:)

[[1,2,3]
,[4,5,6]
,[7,8,9]]

i = 1
j = 2
 => 6
*/

const N = matmul.N

func main() {
	// c = 0.5, s = 0.05
	res1 := calculation(0.5, 0.05)
	// c = 0.5, s = 0.15
	res2 := calculation(0.5, 0.15)
	// c = 0.5, s = 0.5
	res3 := calculation(0.5, 0.5)

	writeCSV("0.05", res1)
	writeCSV("0.15", res2)
	writeCSV("0.5", res3)
}

func writeCSV(filename string, resVec [61][N]float64) {
	file, _ := os.Create("res/" + filename + ".csv")
	writer := csv.NewWriter(file)
	writer.Write([]string{"t", "point 1", "point 4"})
	for i := 0; i <= 60; i++ {
		writer.Write([]string{
			fmt.Sprint(i),
			fmt.Sprint(resVec[i][1]),
			fmt.Sprint(resVec[i][4]),
		})
	}
	writer.Flush()
}

func calculation(c float64, s float64) [61][N]float64 {
	var probMat [N][N]float64 = getProbMat(c, s)

	var resultVectors [61][N]float64
	resultVectors[0] = [N]float64{1, 0, 0, 0, 0, 0}
	for i := 1; i <= 60; i++ {
		resultVectors[i] = matmul.MatVec(probMat, resultVectors[i-1])
	}
	return resultVectors
}

func getProbMat(c float64, s float64) [N][N]float64 {
	return matmul.Transpose([N][N]float64{
		{s + (1-s)*(1-c), (1 - s) * c, 0, 0, 0},
		{(1 - s) * (1 - c), s, (1 - s) * c, 0, 0, 0},
		{0, (1 - s) * (1 - c), s, (1 - s) * c, 0, 0},
		{0, 0, (1 - s) * c, s, (1 - s) * (1 - c), 0},
		{0, 0, 0, (1 - s) * c, s, (1 - s) * (1 - c)},
		{0, 0, 0, 0, (1 - s) * c, s + (1-s)*(1-c)},
	})
}
