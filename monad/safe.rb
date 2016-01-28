class Safe
  def initialize(value)
    @value = value
  end

  def map
    begin
      Safe.new(yield @value)
    rescue
      self
    end
  end
end

p Safe.new(3).map { |n| raise 'un oh' }.map { |n| n + 1 }
