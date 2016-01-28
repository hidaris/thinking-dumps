class Maybe
  def initialize(value)
    @value = value
  end

  def map
    if @value.nil?
      self
    else
      Maybe.new(yield @value)
    end
  end
end

p Maybe.new(3).map { |n| 2 * n }.map { |n| n + 1 }
p Maybe.new(nil).map { |n| 2 * n }.map { |n| n + 1 }
