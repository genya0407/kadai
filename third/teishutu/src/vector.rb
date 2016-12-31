# MyVector（ベクトル）を定義
# Arrayを継承して、四則演算メソッドを作成した
# vect + vect => ベクトル同士の足し算
# vect - vect => ベクトル同士の引き算
# vect * scalar => ベクトルとスカラーの掛け算
# vect / scalar => ベクトルとスカラーの割り算
# また、ベクトルの長さを計算するnormメソッドと、
# 要素の和を求めるsumメソッドを新たに定義した
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
# ex:
#   [1,2,3,4].to_v #=> MyVector.new(1,2,3,4)
class Array
  def to_v
    MyVector.new(self)
  end
end

