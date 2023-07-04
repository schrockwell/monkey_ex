defmodule Monkey.ASTTest do
  use ExUnit.Case

  alias Monkey.AST.Identifier
  alias Monkey.AST.LetStatement
  alias Monkey.AST.Program

  test "to_string/1 works" do
    # GIVEN
    program = %Program{
      statements: [
        %LetStatement{
          token: {:let, "let"},
          name: %Identifier{
            token: {:ident, "myVar"},
            value: "myVar"
          },
          value: %Identifier{
            token: {:ident, "anotherVar"},
            value: "anotherVar"
          }
        }
      ]
    }

    # WHEN
    string = to_string(program)

    # THEN
    assert string == "let myVar = anotherVar;"
  end
end
