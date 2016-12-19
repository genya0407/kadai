require 'gnuplot'

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
end

class Array
  def to_v
    MyVector.new(self)
  end

  def sum
    if block_given?
      self.map { |elem| yield(elem) }.reduce(:+)
    else
      self.reduce(:+)
    end
  end
end

def f(y)
  r1, r2, v1, v2 = y
  return [v1, v2, v2, -v1].to_v
end

def calculate(dt, max_t, &ys_block)
  ts = (0..Float::INFINITY).lazy.map{ |i| i * dt }.take_while { |t| t < max_t }.to_a

  ys = ys_block.call(ts, dt)

  rc = [ ys.transpose[0], ys.transpose[1] ]
  ra = [ ts.map{ |t| - Math.cos(t) }, ts.map{ |t| Math.sin(t) } ]
  err = (rc.transpose).zip(ra.transpose).map { |_rc, _ra| (_rc.to_v - _ra.to_v).norm }

  return ts, rc, ra, err
end

def all_calculations(period, max_t, &ys_block)
  p_err = (3..18).map do |p|
    _, _, _, err = calculate(period * 2**(-p), period, &ys_block)
    [p, err.max]
  end
  
  ts, rc, ra, err = calculate(period/64, max_t, &ys_block)
  
  return ts, rc, ra, err, p_err
end

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
