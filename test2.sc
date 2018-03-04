import breeze.linalg._

val x = DenseVector.zeros[Double](5)

x(0)
x(1) = 2
x
x(3 to 4) := 0.5
x
x(0 to 1) := DenseVector(.1,.2)
x

val m = DenseMatrix.zeros[Int](5,5)
m
(m.rows, m.cols)
m(::,1)


