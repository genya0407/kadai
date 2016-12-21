require 'statsample'

# ２つの配列の間に、y=a+bxの関係があると考え、各種係数を求める
def least_square(xs, ys)
  mlr = Statsample::Regression::Simple.new_from_vectors(
    xs.to_vector(:scale),
    ys.to_vector(:scale)
  )
  return mlr
end
