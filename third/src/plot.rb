require 'gnuplot'

# グラフを実際に出力する処理をまとめたメソッド
def save_graphs(datasets, filename, opts = {})
  Gnuplot.open do |gp|
    Gnuplot::Plot.new(gp) do |plot|
      plot.grid
      plot.terminal 'postscript eps enhanced color'
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
  save_graphs(datasets, File.expand_path("../../tex/graphs/#{name}/rc_ra.eps", __FILE__), {title: 'rc_ra', ylabel: 'y', xlabel: 'x'})

  dataset = Gnuplot::DataSet.new([ts, err]) do |ds|
    ds.notitle
  end
  save_graphs([dataset], File.expand_path("../../tex/graphs/#{name}/error_by_time.eps", __FILE__), {title: 'error by time', ylabel: 'e_r(t)', xlabel: 't'})
  
  x = p_err.transpose[0]
  y = p_err.transpose[1]
  dataset = Gnuplot::DataSet.new([x, y.map { |elem| Math.log2(elem) }]) do |ds|
    ds.notitle
  end
  save_graphs([dataset], File.expand_path("../../tex/graphs/#{name}/error_by_p.eps", __FILE__), {title: 'error by p', ylabel: 'Er', xlabel: 'p'})
end
