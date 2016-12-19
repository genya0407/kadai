# 他のファイルを読み込む
require './vector'
require './calculation'
require './plot'

# 問題で与えられた微分方程式を、関数にしたもの。
# ある点xでの、"傾き"を返す。
# dx/dt = f(x)
# x = (r v)
def f(x)
  r1, r2, v1, v2 = x
  return [v1, v2, v2, -v1].to_v
end

# 一周期の長さを適当に6.4秒とする
period = 6.4

# オイラー法にて数値計算
ts, rc, ra, err, p_err = all_calculations(period, period*5) do |ts, dt|
  xs = []
  ts.each_with_index do |t, i|
    if i == 0
      # 初期値を与える
      xn =[-1.0, 0, 0, 1.0].to_v
    else
      xn = xs.last + f(xs.last) * dt
    end
    xs.push(xn)
  end
  xs
end
draw_graphs(ts, rc, ra, err, p_err, 'euler')

# ホイン法にて数値計算
ts, rc, ra, err, p_err = all_calculations(period, period*5) do |ts, dt|
  xs = []
  ts.each_with_index do |t, i|
    if i == 0
      xn = [-1.0, 0, 0, 1.0].to_v
    else
      x_1 = xs.last + f(xs.last) * dt
      xn = xs.last + (f(x_1) + f(xs.last)) / 2.to_f * dt
    end
    xs.push(xn)
  end
  xs
end
draw_graphs(ts, rc, ra, err, p_err, 'heun')

# 4次のルンゲ・クッタ法にて数値計算
ts, rc, ra, err, p_err = all_calculations(period, period*5) do |ts, dt|
  xs = []
  ts.each_with_index do |t, i|
    if i == 0
      xn = [-1.0, 0, 0, 1.0].to_v
    else
      k1 = f(xs.last)
      k2 = f(xs.last + k1 * (dt/2))
      k3 = f(xs.last + k2 * (dt/2))
      k4 = f(xs.last + k3 * dt)
      k = (k1 + k2 * 2.0 + k3 * 2.0 + k4)/6.to_f
      xn = xs.last + k * dt
    end
    xs.push(xn)
  end
  xs
end
draw_graphs(ts, rc, ra, err, p_err, 'runge_kutta')
