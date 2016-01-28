class Number < Struct.new(:value)
  def to_ruby
    "-> e { #{value.inspect} }"
  end
end

class Boolean < Struct.new(:value)
  def to_ruby
    "-> e { #{value.inspect} }"
  end
end

class Variable < Struct.new(:name)
  def to_ruby
    "-> e { e[#{name.inspect}] }"
  end
end

class Add < Struct.new(:left, :right)
  def to_ruby
    "-> e { (#{left.to_ruby}).call(e) + (#{right.to_ruby}).call(e) }"
  end
end

class Multiply < Struct.new(:left, :right)
  def to_ruby
    "-> e { (#{left.to_ruby}).call(e) * (#{right.to_ruby}).call(e) }"
  end
end

class LessThan < Struct.new(:left, :right)
  def to_ruby
    "-> e { (#{left.to_ruby}).call(e) < (#{right.to_ruby}).call(e) }"
  end
end

class Assign < Struct.new(:name, :expression)
  def to_ruby
    "-> e { e.merge({ (#{name.inspect})" +
      " => (#{expression.to_ruby}).call(e) })}"
  end
end

class If < Struct.new(:condition, :consequence, :alternative)
  def to_ruby
    "-> e { if (#{condition.to_ruby}).call(e)" +
      " then (#{consequence.to_ruby}).call(e)" +
      " else (#{alternative.to_ruby}).call(e)" +
      " end }"
  end
end

class Sequence < Struct.new(:first, :second)
  def to_ruby
    "-> e { (#{second.to_ruby}).call((#{first.to_ruby}).call(e)) }"
  end
end

class While < Struct.new(:condition, :body)
  def to_ruby
    "-> e {" +
      " while (#{condition.to_ruby}).call(e);" +
      " e = (#{body.to_ruby}).call(e);" +
      " end; " +
      " e }"
  end
end

class DoNothing
  def to_ruby
    "-> e { e }"
  end
end

statement =
  While.new(
  LessThan.new(Variable.new(:x), Number.new(5)),
  Assign.new(:x, Multiply.new(Variable.new(:x), Number.new(3)))
)

p statement.to_ruby

proc = eval(statement.to_ruby)

p proc.call({ x: 1})
