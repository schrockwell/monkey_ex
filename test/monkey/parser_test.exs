defmodule Monkey.ParserTest do
  use ExUnit.Case

  alias Monkey.AST
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

    assert [
             %AST.ExpressionStatement{
               expression: %AST.IntegerLiteral{
                 value: 5
               }
             }
           ] = program.statements
  end

  test "prefix expressions" do
    # GIVEN
    tests = [
      %{input: "!5", operator: "!", value: 5},
      %{input: "-15", operator: "-", value: 15}
    ]

    # WHEN
    results =
      Enum.map(tests, fn test ->
        Map.put(test, :program, parse_program(test.input))
      end)

    # THEN
    for %{operator: operator, value: value, program: program} <- results do
      assert length(program.statements) == 1

      assert [
               %AST.ExpressionStatement{
                 expression: %AST.PrefixExpression{
                   operator: ^operator,
                   right: %AST.IntegerLiteral{
                     value: ^value
                   }
                 }
               }
             ] = program.statements
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
      %{input: "5 != 5;", left: 5, operator: "!=", right: 5}
    ]

    # WHEN
    results =
      for test <- tests do
        Map.put(test, :program, parse_program(test.input))
      end

    # THEN
    for %{left: left, operator: operator, right: right, program: program} <- results do
      assert length(program.statements) == 1

      assert [
               %AST.ExpressionStatement{
                 expression: %AST.InfixExpression{
                   left: %AST.IntegerLiteral{value: ^left},
                   operator: ^operator,
                   right: %AST.IntegerLiteral{value: ^right}
                 }
               }
             ] = program.statements
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
      {"3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"}
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
end
