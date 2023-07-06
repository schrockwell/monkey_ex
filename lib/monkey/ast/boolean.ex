defmodule Monkey.AST.Boolean do
  defstruct [:token, :value]
end

defimpl String.Chars, for: Monkey.AST.Boolean do
  def to_string(node), do: Monkey.AST.Node.token_literal(node)
end
