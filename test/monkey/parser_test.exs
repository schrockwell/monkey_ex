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
end
