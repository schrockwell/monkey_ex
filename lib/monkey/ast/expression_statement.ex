defmodule Monkey.AST.ExpressionStatement do
  defstruct [:token, :expression]
end

defimpl String.Chars, for: Monkey.AST.ExpressionStatement do
  def to_string(%{expression: nil}), do: ""
  def to_string(%{expression: exp}), do: Kernel.to_string(exp)
end
