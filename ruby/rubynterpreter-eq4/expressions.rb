# Expressions __________________________________________________________________

# Base class for representations of expression of any kind.
class Expression

  # Returns a string that will produce this same representation of code if it
  # gets parsed.
  def unparse()
    throw "#{self.class.name}.unparse() is not implemented!"
  end

  # Returns the evaluation of this expression. The `state` argument must be a
  # `Hash` mapping variable names to values.
  def evaluate(state)
    throw "#{self.class.name}.evaluate() is not implemented!"
  end
end

# Representation of variables in expressions, e.g. `(x)`.
class VariableExp < Expression
  def initialize(identifier)
    @identifier = identifier
  end

  def unparse()
    "#{@identifier}"
  end

  def evaluate(state = {})
    state[@identifier]
  end

  attr_reader :identifier
end

# Arithmetic expressions _______________________________________________________

# Representation of numerals (i.e. numerical literals), e.g. `(123)`.
class Numeral < Expression
  def initialize(value)
    @value = Float(value)
  end

  attr_reader :value

  def unparse()
    "#{@value}"
  end

  def evaluate(state = {})
    @value
  end
end

# Representation of minus expressions, like `(-right)`.
class Minus < Expression
  def initialize(right)
    @right = right
  end

  def unparse
    "(-#{@right.unparse})"
  end

  def evaluate(state = {})
    -@right.evaluate(state)
  end

  attr_reader :right
end

# Representation of addition expressions, like `(left + right)`.
class Addition < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} + #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) + @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of subtraction expressions, like `(left - right)`.
class Subtraction < Expression 
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} - #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) - @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of multiplication expressions, like `(left * right)`.
class Multiplication < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} * #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) * @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of division expressions, like `(left / right)`.
class Division < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} / #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) / @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Comparisons __________________________________________________________________

# Representation of comparison by equal, like `(left == right)`.
class ComparisonEqual < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} == #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) == @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of comparison by different, like `(left != right)`.
class ComparisonDifferent < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} != #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) != @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of comparison by less than, like `(left < right)`.
class ComparisonLessThan < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} < #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) < @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of comparison by less than or equal, like `(left <= right)`.
class ComparisonLessThanOrEqual < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} <= #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) <= @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of comparison by greater than, like `(left > right)`.
class ComparisonGreaterThan < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} > #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) > @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of comparison by greater than or equal, like `(left >= right)`.
class ComparisonGreaterThanOrEqual < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} >= #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) >= @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Boolean expressions __________________________________________________________

# Representation of boolean literals, e.g. `(true)`.
class TruthValue < Expression
  def initialize(value)
    @value = !!(value)
  end

  def unparse
    "#{@value}"
  end

  attr_reader :value

  def evaluate(state = {})
    @value
  end
end

# Representation of logical negation expressions, like `(!right)`.
class Negation < Expression
  def initialize(right)
    @right = right
  end

  def unparse
    "(!#{@right.unparse})"
  end

  def evaluate(state = {})
    !@right.evaluate(state)
  end

  attr_reader :right
end

# Representation of logical AND expressions, like `(left && right)`.
class LogicalAnd < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} && #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) && @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end

# Representation of logical OR expressions, like `(left || right)`.
class LogicalOr < Expression
  def initialize(left, right)
    @left = left
    @right = right
  end

  def unparse
    "(#{@left.unparse} || #{@right.unparse})"
  end

  def evaluate(state = {})
    @left.evaluate(state) || @right.evaluate(state)
  end

  attr_reader :left
  attr_reader :right
end