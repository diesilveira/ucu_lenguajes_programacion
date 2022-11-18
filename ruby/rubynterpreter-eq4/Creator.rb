class Creator
    def factory_method(value)
        raise NotImplementedError
    end
end

class ExpressionCreator < Creator
    def factory_method(val)
        raise NotImplementedError
    end
end

class VariableExpCreator < ExpressionCreator
    def factory_method(val[0])
        VariableExp.new(val[0])
    end
end

class NumeralCreator < ExpressionCreator
    def factory_method(val[0])
        Numeral.instance(val[0])
    end
end

class MinusCreator < ExpressionCreator
    def factory_method(val[1])
        Minus.new(val[1])
    end
end

class AdditionCreator < ExpressionCreator
    def factory_method(val[0], val[2])
        Addition.new(val[0], val[2])
    end
end

class SubtractionCreator < ExpressionCreator
    def factory_method(val[1])
        Subtraction.new(val[1])
    end
end

class MultiplicationCreator < ExpressionCreator
    def factory_method(val[0], val[2])
        Multiplication.new(val[0], val[2])
    end
end


class DivisionCreator < ExpressionCreator
    def factory_method(val[0], val[2])
        Division.new(val[0], val[2])
    end
end

class ComparisonEqualCreator < ExpressionCreator
    def factory_method(val[0], val[2])
        ComparisonEqual.new(val[0], val[2])
    end
end

class ComparisonDifferentCreator < ExpressionCreator
    def factory_method(val[0], val[2])
        ComparisonDifferent.new(val[0], val[2])
    end
end

class ComparisonLessThanCreator < ExpressionCreator
    def factory_method(val[0], val[2])
        ComparisonLessThan.new(val[0], val[2])
    end
end

class ComparisonLessThanOrEqualCreator < ExpressionCreator
    def factory_method(val[0], val[2])
        ComparisonLessThanOrEqual.new(val[0], val[2])
    end
end



class StatementCreator < Creator
    def factory_method(value)
        raise NotImplementedError
    end
end