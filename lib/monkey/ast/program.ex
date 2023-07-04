defmodule Monkey.AST.Program do
  defstruct statements: []
end

defimpl Monkey.AST.Node, for: Monkey.AST.Program do
  alias Monkey.Token

  def token_literal(%{statements: []}), do: ""

  def token_literal(%{statements: [first | _]}) do
    Token.literal(first)
  end
end

defimpl String.Chars, for: Monkey.AST.Program do
  def to_string(program) do
    Enum.map_join(program.statements, &Kernel.to_string/1)
  end
end
