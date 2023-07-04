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
end
