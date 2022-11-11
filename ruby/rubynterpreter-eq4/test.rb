require_relative 'parser'

describe Parser do
    parser = Parser.new
    context "Statements" do
        it "Si asigno un valor a x se almacena en el state" do
            ast = parser.parse_string("x = 1;")
            res = ast.evaluate
            expect(res["x"]).to eq 1
        end

        it "Si evaluo una sentencia de bloque se ejecuta correctamente" do
            ast = parser.parse_string("{ x = 1; y = 5; x = 10; }")
            res = ast.evaluate
            expect(res["x"]).to eq 10
        end

        it "Si evaluo un if con una condición true evalua el bodyThen" do
            ast = parser.parse_string("if (true) x = 5; else x = 3;")
            res = ast.evaluate
            expect(res["x"]).to eq 5
        end

        it "Si evaluo un if con una condición false evalua el bodyElse" do
            ast = parser.parse_string("if (false) x = 5; else x = 3;")
            res = ast.evaluate
            expect(res["x"]).to eq 3
        end

        it "Si evaluo un while se ejecuta hasta que la condición se deje de cumplir" do
            ast = parser.parse_string("{x = 0; while (x < 5) x = x+1; }")
            res = ast.evaluate
            expect(res["x"]).to eq 5
        end
    end

    context "Expressions" do
        it "Si le pido el valor de x y ya existe lo devuelve" do
            ast = parser.parse_string("x")
            state = { "x" => 1}
            res = ast.evaluate(state)
            expect(res).to eq 1
        end

        it "Si ingreso un valor numero lo devuelve" do
            ast = parser.parse_string("5")
            res = ast.evaluate
            expect(res).to eq 5
        end
        
        it "Si niego un valor numerico devuelve el valor esperado" do
            ast = parser.parse_string("-2")
            res = ast.evaluate
            expect(res).to eq -2
        end

        it "Si efectuo una resta devuelve el valor esperado" do
            ast = parser.parse_string("7-2")
            res = ast.evaluate
            expect(res).to eq 5
        end

        it "Si efectuo una suma devuelve el valor esperado" do
            ast = parser.parse_string("7+2")
            res = ast.evaluate
            expect(res).to eq 9
        end

        it "Si efectuo una multiplicacion devuelve el valor esperado" do
            ast = parser.parse_string("3*3")
            res = ast.evaluate
            expect(res).to eq 9
        end

        it "Si efectuo una division devuelve el valor esperado" do
            ast = parser.parse_string("8/2")
            res = ast.evaluate
            expect(res).to eq 4
        end

        it "Si efectuo una comparación por igual devuelve el valor esperado" do
            ast = parser.parse_string("2 == 2")
            res = ast.evaluate
            expect(res).to eq true
        end

        it "Si efectuo una comparación por diferente devuelve el valor esperado" do
            ast = parser.parse_string("5 != 2")
            res = ast.evaluate
            expect(res).to eq true
        end

        it "Si efectuo una comparación por menor estricto devuelve el valor esperado" do
            ast = parser.parse_string("2 < 2")
            res = ast.evaluate
            expect(res).to eq false
        end

        it "Si efectuo una comparación por menor o igual devuelve el valor esperado" do
            ast = parser.parse_string("4 <= 4")
            res = ast.evaluate
            expect(res).to eq true
        end

        it "Si efectuo una comparación por mayor estricto devuelve el valor esperado" do
            ast = parser.parse_string("2 > 15")
            res = ast.evaluate
            expect(res).to eq false
        end

        it "Si efectuo una comparación por mayor o igual devuelve el valor esperado" do
            ast = parser.parse_string("20 >= 4")
            res = ast.evaluate
            expect(res).to eq true
        end

        it "Si evaluo un valor boleano devuelve el valor esperado" do
            ast = parser.parse_string("true")
            res = ast.evaluate
            expect(res).to eq true
        end

        it "Si evaluo la negación de un valor boleano devuelve el valor esperado" do
            ast = parser.parse_string("!true")
            res = ast.evaluate
            expect(res).to eq false
        end

        it "Si evaluo el AND de dos valores boleanos devuelve el valor esperado" do
            ast = parser.parse_string("true && false")
            res = ast.evaluate
            expect(res).to eq false
        end

        it "Si evaluo el OR de dos valores boleanos devuelve el valor esperado" do
            ast = parser.parse_string("true || false")
            res = ast.evaluate
            expect(res).to eq true
        end
    end
end
