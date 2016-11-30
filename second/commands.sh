#! /bin/bash -x
go run 1.go print.go forward.go
go run 2.go print.go backward.go
go run 4.go print.go lu.go
go run 5.go print.go solve.go lu.go forward.go backward.go
go run 6_1.go print.go 6.go solve.go lu.go forward.go backward.go
go run 6_2.go print.go 6.go solve.go lu.go forward.go backward.go
go run 8_1.go print.go 8.go inverse.go solve.go lu.go forward.go backward.go
go run 8_2.go print.go 8.go inverse.go solve.go lu.go forward.go backward.go
