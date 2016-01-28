# coding: utf-8
class Number < Struct.new(:value)
  def to_s
    value.to_s
  end

  def evaluate(environment)
    self
  end

  def inspect
    "<#{self}>"
  end
end

class Boolean < Struct.new(:value)
  def to_s
    value.to_s
  end

  def evaluate(environment)
    self
  end

  def inspect
    "#{self}"
  end
end

class Variable < Struct.new(:name)
  def evaluate(environment)
    environment[name]
  end
end

class Add < Struct.new(:left, :right)
  def evaluate(environment)
    Number.new(left.evaluate(environment).value +
               right.evaluate(environment).value)
  end
end

class Multiply < Struct.new(:left, :right)
  def evaluate(environment)
    Number.new(left.evaluate(environment).value *
               right.evaluate(environment).value)
  end
end

class LessThan < Struct.new(:left, :right)
  def evaluate(environment)
    Boolean.new(left.evaluate(environment).value <
                right.evaluate(environment).value)
  end
end

class Assign < Struct.new(:name, :expression)
  def evaluate(environment)
    environment.merge({ name => expression.evaluate(environment) })
  end
end

class DoNothing
  def evaluate(environment)
    environment
  end
end

class If < Struct.new(:condition, :consequece, :alternative)
  def evaluate(environment)
    case condition.evaluate(environment)
    when Boolean.new(true)
      consequece.evaluate(environment)
    when Boolean.new(false)
      alternative.evaluate(environment)
    else
      nil
    end
  end
end

class Sequence < Struct.new(:first, :second)
  def evaluate(environment)
    second.evaluate(first.evaluate(environment))
  end
end

class While < Struct.new(:condition, :body)
  def evaluate(environment)
    case condition.evaluate(environment)
    when Boolean.new(true)
      evaluate(body.evaluate(environment))
    when Boolean.new(false)
      environment
    else
      "条件不是bool"
    end
  end
end

p LessThan.new(
    Add.new(Variable.new(:x), Number.new(2)),
    Variable.new(:y)
  ).evaluate({ x: Number.new(2), y: Number.new(5)})

statement =
  Sequence.new(
  Assign.new(:x, Add.new(Number.new(1), Number.new(1))),
  Assign.new(:y, Add.new(Variable.new(:x), Number.new(3))))

p statement.evaluate({})

p While.new(LessThan.new(Variable.new(:x), Number.new(5)),
            Assign.new(:x, Add.new(Number.new(1), Variable.new(:x)))
           ).evaluate({ x: Number.new(1) })

p While.new(
  Add.new(Number.new(1), Variable.new(:x)),
  Assign.new(:x, Add.new(Number.new(1), Variable.new(:x)))
).evaluate({ x: Number.new(1) })
