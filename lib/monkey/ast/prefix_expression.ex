defmodule Monkey.AST.PrefixExpression do
  defstruct [:token, :operator, :right]
end

defimpl String.Chars, for: Monkey.AST.PrefixExpression do
  def to_string(prefix) do
    "#{prefix.operator}#{prefix.right}"
  end
end
