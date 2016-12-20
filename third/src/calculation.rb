# 渡されたブロックで表されたアルゴリズムに基づいて、数値計算を行なう
# 
# 引数の説明：
# dt -> 微小量の多きさ
# max_t -> tの最大値
# ys_block -> アルゴリズムを表すブロック
#
# 返り値の説明：
# ts -> [0, dt, 2*dt, 3*dt, ... max_t - dt]
# rc -> 数値計算から求めたrの値(tsに対応する時刻)
# ra -> 解析解から求めたrの値(tsに対応する時刻)
# err -> 各時刻での、数値計算による解と解析解の誤差([e_r(0), e_r(dt), e_r(dt*2), ... , e_r(max_t - dt)])
#
# アルゴリズムを表すブロックは、tsとdtを受け取り、各時刻(0, dt, 2dt, 3dt, ...)でのyの値を表す配列を返すものとする
def calculate(dt, max_t, &ys_block)
  ts = (0..Float::INFINITY).lazy.map{ |i| i * dt }.take_while { |t| t < max_t }.to_a

  ys = ys_block.call(ts, dt)

  rc = [ ys.transpose[0], ys.transpose[1] ]
  ra = [ ts.map{ |t| - Math.cos(t) }, ts.map{ |t| Math.sin(t) } ]
  err = (rc.transpose).zip(ra.transpose).map { |_rc, _ra| (_rc.to_v - _ra.to_v).norm }

  return ts, rc, ra, err
end

# 渡されたブロックに基づいて、課題3.1と課題3.2の数値計算を行なう
#
# 引数の説明：
# period -> 一周期の時間(2π/ω)
# max_t -> 課題3.1でのtの最大値
# ys_block -> アルゴリズムを表すブロック
#
# 返り値の説明：
# ts -> [0, dt, 2*dt, 3*dt, ... max_t - dt]（課題3.1）
# rc -> 課題3.1で、数値計算から求めたrの値(tsに対応する時刻)
# ra -> 課題3.1で、解析解から求めたrの値(tsに対応する時刻)
# err -> 課題3.1で、各時刻での、数値計算による解と解析解の誤差([e_r(0), e_r(dt), e_r(dt*2), ... , e_r(max_t - dt)])
# p_err -> 課題3.2での、pの値とその時の最大誤差(Er)が組になったものの配列([[p1, Er(p1)], [p2, Er(p2)], ...])
def all_calculations(period, max_t, &ys_block)
  p_err = (3..18).map do |p|
    _, _, _, err = calculate(period * 2**(-p), period, &ys_block)
    [p, err.max]
  end
  
  ts, rc, ra, err = calculate(period/64, max_t, &ys_block)
  
  return ts, rc, ra, err, p_err
end
