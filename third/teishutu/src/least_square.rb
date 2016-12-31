require './vector'

# 最小二乗法のアルゴリズムをナイーブに実装した
def least_square(xs, ys)
  xs = xs.to_v
  ys = ys.to_v
  xys = xs.zip(ys).map { |x, y| x*y }.to_v.sum
  x2s = xs.map{|x| x**2}.to_v.sum
  return (xs.sum * ys.sum - xs.size * xys)/(xs.sum**2 - xs.size * x2s)
end
