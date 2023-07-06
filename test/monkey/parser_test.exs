defmodule Monkey.ParserTest do
  use ExUnit.Case

  alias Monkey.AST
  alias Monkey.AST.Node
  alias Monkey.Lexer
  alias Monkey.Parser

  defp parse_program(input) do
    input
    |> Lexer.stream_tokens()
    |> Parser.new()
    |> Parser.parse_program()
    |> case do
      %{errors: []} = parser ->
        parser.program

      %{errors: errors} ->
        message = """
        Parser has #{length(errors)} errors:

        #{errors |> Enum.map(&"  - #{&1}") |> Enum.join("\n")}
        """

        flunk(message)
    end
  end

  test "parsing let statements" do
    # GIVEN
    input = """
    let x = 5;
    let y = 10;
    let foobar = 838383;
    """

    # WHEN
    program = parse_program(input)

    # THEN
    assert length(program.statements) == 3

    assert [
             %AST.LetStatement{
               name: %Monkey.AST.Identifier{token: {:ident, "x"}, value: "x"},
               token: {:let, "let"}
             },
             %AST.LetStatement{
               name: %Monkey.AST.Identifier{token: {:ident, "y"}, value: "y"},
               token: {:let, "let"}
             },
             %AST.LetStatement{
               name: %Monkey.AST.Identifier{token: {:ident, "foobar"}, value: "foobar"},
               token: {:let, "let"}
             }
           ] = program.statements
  end

  test "parsing return statements" do
    # GIVEN
    input = """
    return 5;
    return 10;
    return 838383;
    """

    # WHEN
    program = parse_program(input)

    # THEN
    assert length(program.statements) == 3

    assert [
             %AST.ReturnStatement{
               #  return_value: %Monkey.AST.Identifier{token: {:ident, "x"}, value: "x"},
               token: {:return, "return"}
             },
             %AST.ReturnStatement{
               #  return_value: %Monkey.AST.Identifier{token: {:ident, "y"}, value: "y"},
               token: {:return, "return"}
             },
             %AST.ReturnStatement{
               #  return_value: %Monkey.AST.Identifier{token: {:ident, "foobar"}, value: "foobar"},
               token: {:return, "return"}
             }
           ] = program.statements
  end

  test "identifier expressions" do
    # GIVEN
    input = "foobar;"

    # WHEN
    program = parse_program(input)

    # THEN
    assert length(program.statements) == 1

    assert [
             %AST.ExpressionStatement{
               expression: %AST.Identifier{
                 value: "foobar"
               }
             }
           ] = program.statements
  end

  test "integer literals" do
    # GIVEN
    input = "5;"

    # WHEN
    program = parse_program(input)

    # THEN
    assert length(program.statements) == 1
    assert_literal(hd(program.statements).expression, 5)
  end

  test "prefix expressions" do
    # GIVEN
    tests = [
      %{input: "!5", operator: "!", value: 5},
      %{input: "-15", operator: "-", value: 15},
      %{input: "!true;", operator: "!", value: true},
      %{input: "!false", operator: "!", value: false}
    ]

    # WHEN
    results =
      Enum.map(tests, fn test ->
        Map.put(test, :program, parse_program(test.input))
      end)

    # THEN
    for %{operator: operator, value: value, program: program} <- results do
      assert length(program.statements) == 1
      assert_prefix(hd(program.statements).expression, operator, value)
    end
  end

  test "infix expressions" do
    # GIVEN
    tests = [
      %{input: "5 + 6;", left: 5, operator: "+", right: 6},
      %{input: "5 - 5;", left: 5, operator: "-", right: 5},
      %{input: "5 * 5;", left: 5, operator: "*", right: 5},
      %{input: "5 / 5;", left: 5, operator: "/", right: 5},
      %{input: "5 > 5;", left: 5, operator: ">", right: 5},
      %{input: "5 < 5;", left: 5, operator: "<", right: 5},
      %{input: "5 == 5;", left: 5, operator: "==", right: 5},
      %{input: "5 != 5;", left: 5, operator: "!=", right: 5},
      %{input: "true == true", left: true, operator: "==", right: true},
      %{input: "true != false", left: true, operator: "!=", right: false},
      %{input: "false == false", left: false, operator: "==", right: false}
    ]

    # WHEN
    results =
      for test <- tests do
        Map.put(test, :program, parse_program(test.input))
      end

    # THEN
    for %{left: left, operator: operator, right: right, program: program} <- results do
      assert length(program.statements) == 1

      assert_infix(hd(program.statements).expression, left, operator, right)
    end
  end

  test "operator precedence" do
    # GIVEN
    tests = [
      {"-a", "(-a)"},
      {"-a * b", "((-a) * b)"},
      {"!-a", "(!(-a))"},
      {"a + b + c", "((a + b) + c)"},
      {"a + b - c", "((a + b) - c)"},
      {"a * b * c", "((a * b) * c)"},
      {"a * b / c", "((a * b) / c)"},
      {"a + b / c", "(a + (b / c))"},
      {"a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"},
      {"3 + 4; -5 * 5;", "(3 + 4)((-5) * 5)"},
      {"5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"},
      {"5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"},
      {"3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
      {"true", "true"},
      {"false", "false"},
      {"3 > 5 == false", "((3 > 5) == false)"},
      {"1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"},
      {"(5 + 5) * 2", "((5 + 5) * 2)"},
      {"3 < 5 ==  true", "((3 < 5) == true)"},
      {"2 / (5 + 5)", "(2 / (5 + 5))"},
      {"-(5 + 5)", "(-(5 + 5))"},
      {"!(true == true)", "(!(true == true))"}
    ]

    # WHEN
    results =
      for {input, expected} <- tests do
        %{input: input, expected: expected, program: parse_program(input)}
      end

    # THEN
    for result <- results do
      assert to_string(result.program) == result.expected
    end
  end

  test "boolean expressions" do
    # GIVEN
    inputs = [
      {"true;", true},
      {"false;", false}
    ]

    # WHEN
    results =
      for {input, expected} <- inputs do
        %{input: input, expected: expected, program: parse_program(input)}
      end

    # THEN
    for %{expected: expected, program: program} <- results do
      assert length(program.statements) == 1
      assert_literal(hd(program.statements).expression, expected)
    end
  end

  test "if expressions" do
    # GIVEN
    input = "if (x < y) { x }"

    # WHEN
    program = parse_program(input)

    # THEN
    assert length(program.statements) == 1

    assert [
             %AST.ExpressionStatement{
               expression: %AST.IfExpression{
                 condition: condition,
                 consequence: consequence,
                 alternative: alternative
               }
             }
           ] = program.statements

    assert_infix(condition, "x", "<", "y")

    assert %Monkey.AST.BlockStatement{
             statements: [
               %Monkey.AST.ExpressionStatement{
                 expression: %Monkey.AST.Identifier{value: "x"}
               }
             ]
           } = consequence

    assert alternative == nil
  end

  test "if-else expressions" do
    # GIVEN
    input = "if (x < y) { x } else { y }"

    # WHEN
    program = parse_program(input)

    # THEN
    assert length(program.statements) == 1

    assert [
             %AST.ExpressionStatement{
               expression: %AST.IfExpression{
                 condition: condition,
                 consequence: consequence,
                 alternative: alternative
               }
             }
           ] = program.statements

    assert_infix(condition, "x", "<", "y")

    assert %Monkey.AST.BlockStatement{
             statements: [
               %Monkey.AST.ExpressionStatement{
                 expression: %Monkey.AST.Identifier{value: "x"}
               }
             ]
           } = consequence

    assert %Monkey.AST.BlockStatement{
             statements: [
               %Monkey.AST.ExpressionStatement{
                 expression: %Monkey.AST.Identifier{value: "y"}
               }
             ]
           } = alternative
  end

  defp assert_literal(expression, expected) when is_binary(expected) do
    assert %AST.Identifier{value: ^expected} = expression
    assert Node.token_literal(expression) == expected
  end

  defp assert_literal(expression, expected) when is_integer(expected) do
    assert %AST.IntegerLiteral{value: ^expected} = expression
    assert Node.token_literal(expression) == to_string(expected)
  end

  defp assert_literal(expression, expected) when is_boolean(expected) do
    assert %AST.Boolean{value: ^expected} = expression
    assert Node.token_literal(expression) == to_string(expected)
  end

  defp assert_prefix(%AST.PrefixExpression{} = prefix, operator, right) do
    assert prefix.operator == operator
    assert_literal(prefix.right, right)
  end

  defp assert_infix(%AST.InfixExpression{} = infix, left, operator, right) do
    assert_literal(infix.left, left)
    assert infix.operator == operator
    assert_literal(infix.right, right)
  end
end
