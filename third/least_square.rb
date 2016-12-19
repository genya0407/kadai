require 'statsample'

def least_square(xs, ys)
  mlr = Statsample::Regression::Simple.new_from_vectors(
    xs.to_vector(:scale),
    ys.to_vector(:scale)
  )
  return mlr
end
