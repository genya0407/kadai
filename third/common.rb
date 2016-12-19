require 'gnuplot'

# MyVector（ベクトル）を定義
# Arrayを継承して、四則演算メソッドを作成した
# vect + vect => ベクトル同士の足し算
# vect - vect => ベクトル同士の引き算
# vect * scalar => ベクトルとスカラーの掛け算
# vect / scalar => ベクトルとスカラーの割り算
# また、ベクトルの長さを計算するnormメソッドと、
# 要素の和を求めるsumメソッドをを新たに定義した
class MyVector < Array
  def *(scalar)
    self.map{ |elem| elem * scalar }.to_v
  end
  
  def +(other)
    self.zip(other).map { |self_elem, other_elem| self_elem + other_elem }.to_v
  end
  
  def -(other)
    self + (other * -1)
  end
  
  def /(scalar)
    self.map { |elem| elem / scalar }.to_v
  end
  
  def norm
    Math.sqrt(self.sum { |elem| elem**2 })
  end

  def sum
    if block_given?
      self.map { |elem| yield(elem) }.reduce(:+)
    else
      self.reduce(:+)
    end
  end
end

# Arrayにも新たにメソッドを生やした
# ArrayをMyVectorに変換するto_vメソッド
class Array
  def to_v
    MyVector.new(self)
  end
end

# 問題で与えられた微分方程式を、関数にしたもの
# dx/dt = f(x)
# x = (r v)
def f(x)
  r1, r2, v1, v2 = x
  return [v1, v2, v2, -v1].to_v
end

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

# グラフを実際に出力する処理をまとめたメソッド
def save_graphs(datasets, filename, opts = {})
  Gnuplot.open do |gp|
    Gnuplot::Plot.new(gp) do |plot|
      plot.terminal 'png'
      plot.output filename

      opts.each do |k,v|
        plot.send(k, v)
      end

      datasets.each { |dataset| plot.data << dataset }
    end
  end
end

# ts, rc, ra, err, p_err, nameから、
# rc_ra.png、error_by_time.png, error_by_p.pngの３つのグラフを出力するメソッド
# nameはアルゴリズムの名前を示しており、出力先ディレクトリを変更するのに使われる。
def draw_graphs(ts, rc, ra, err, p_err, name)
  trace_rc = { x: rc[0], y: rc[1], name: 'rc' }
  trace_ra = { x: ra[0], y: ra[1], name: 'ra' }
  datasets = [trace_rc, trace_ra].map do |trace|
    Gnuplot::DataSet.new([trace[:x], trace[:y]]) do |ds|
      ds.with = 'lines'
      ds.title = trace[:name]
    end
  end
  save_graphs(datasets, File.expand_path("../graphs/#{name}/rc_ra.png", __FILE__), {title: 'rc_ra', ylabel: 'y', xlabel: 'x'})

  dataset = Gnuplot::DataSet.new([ts, err]) do |ds|
    ds.with = 'lines'
    ds.notitle
  end
  save_graphs([dataset], File.expand_path("../graphs/#{name}/error_by_time.png", __FILE__), {title: 'error by time', ylabel: 'e_r(t)', xlabel: 't'})
  
  dataset = Gnuplot::DataSet.new(p_err.transpose) do |ds|
    ds.with = 'lines'
  end
  save_graphs([dataset], File.expand_path("../graphs/#{name}/error_by_p.png", __FILE__), {title: 'error by p', logscale: 'y', ylabel: 'Er', xlabel: 'p'})
end
