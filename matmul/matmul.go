package matmul

import "github.com/k0kubun/pp"

// i => 行, j => 列とする
/* ex:)

[[1,2,3]
,[4,5,6]
,[7,8,9]]

i = 1
j = 2
 => 6
*/

const N = 6

func main() {
	A := [N][N]float64{
		{1, 4, 7},
		{2, 5, 8},
		{3, 6, 9},
	}
	b1 := [N]float64{1, 2, 3}
	b2 := [N]float64{4, 5, 6}
	b3 := [N]float64{7, 8, 9}

	Abs := Transpose([N][N]float64{
		MatVec(A, b1),
		MatVec(A, b2),
		MatVec(A, b3),
	})

	AA := MatMlt(A, A)

	pp.Println(Abs)
	pp.Println(AA)

	AA_b1 := MatVec(MatMlt(A, A), b1)
	A_Ab1 := MatVec(A, MatVec(A, b1))

	pp.Println(AA_b1)
	pp.Println(A_Ab1)
}

// 行列とベクトルを受け取り、ベクトルを返す
func MatVec(mat [N][N]float64, vec [N]float64) [N]float64 {
	var ret_vec [N]float64

	for i := 0; i < N; i++ {
		// 行列の各行ベクトルと、ベクトルの内積を取る
		ret_vec[i] = dot(mat[i], vec)
	}

	return ret_vec
}

// 行列と行列を受け取り、行列を返す
func MatMlt(mat1 [N][N]float64, mat2 [N][N]float64) [N][N]float64 {
	var ret_mat [N][N]float64

	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			// 行列1のi行ベクトルと、行列2のj列ベクトルのベクトル積が、
			// 返す行列のi行j列成分
			ret_mat[i][j] = dot(mat1[i], fetch_col(mat2, j))
		}
	}

	return ret_mat
}

// ベクトルとベクトルを受け取り、スカラーを返す
func dot(vec1 [N]float64, vec2 [N]float64) float64 {
	var sum float64 = 0
	for n := 0; n < N; n++ {
		sum += vec1[n] * vec2[n]
	}
	return sum
}

// 行列とスカラーを受け取り、ベクトルを返す
// 行列から列ベクトルを取り出す
func fetch_col(mat [N][N]float64, col_ind int) [N]float64 {
	var col [N]float64

	for i := 0; i < N; i++ {
		col[i] = mat[i][col_ind]
	}

	return col
}

func Transpose(mat [N][N]float64) [N][N]float64 {
	var ret_mat [N][N]float64

	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			ret_mat[i][j] = mat[j][i]
		}
	}

	return ret_mat
}
