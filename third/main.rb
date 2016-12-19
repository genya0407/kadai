# common.rbを読み込む
require './common'

# 一周期の長さを適当に6.4秒とする
period = 6.4

# オイラー法にて数値計算
ts, rc, ra, err, p_err = all_calculations(period, period*5) do |ts, dt|
  ys = []
  ts.each_with_index do |t, i|
    if i == 0
      # 初期値を与える
      yn =[-1.0, 0, 0, 1.0].to_v
    else
      yn = ys.last + f(ys.last) * dt
    end
    ys.push(yn)
  end
  ys
end
draw_graphs(ts, rc, ra, err, p_err, 'euler')

# ホイン法にて数値計算
ts, rc, ra, err, p_err = all_calculations(period, period*5) do |ts, dt|
  ys = []
  ts.each_with_index do |t, i|
    if i == 0
      yn = [-1.0, 0, 0, 1.0].to_v
    else
      ys_1 = ys.last + f(ys.last) * dt
      yn = ys.last + (f(ys_1) + f(ys.last)) / 2.to_f * dt
    end
    ys.push(yn)
  end
  ys
end
draw_graphs(ts, rc, ra, err, p_err, 'heun')

# 4次のルンゲ・クッタ法にて数値計算
ts, rc, ra, err, p_err = all_calculations(period, period*5) do |ts, dt|
  ys = []
  ts.each_with_index do |t, i|
    if i == 0
      yn = [-1.0, 0, 0, 1.0].to_v
    else
      k1 = f(ys.last)
      k2 = f(ys.last + k1 * (dt/2))
      k3 = f(ys.last + k2 * (dt/2))
      k4 = f(ys.last + k3 * dt)
      k = (k1 + k2 * 2.0 + k3 * 2.0 + k4)/6.to_f
      yn = ys.last + k * dt
    end
    ys.push(yn)
  end
  ys
end
draw_graphs(ts, rc, ra, err, p_err, 'runge_kutta')
