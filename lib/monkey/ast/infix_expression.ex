defmodule Monkey.AST.InfixExpression do
  defstruct [:token, :left, :operator, :right]
end

defimpl String.Chars, for: Monkey.AST.InfixExpression do
  def to_string(prefix) do
    "(#{prefix.left} #{prefix.operator} #{prefix.right})"
  end
end
