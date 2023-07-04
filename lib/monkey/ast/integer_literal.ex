defmodule Monkey.AST.IntegerLiteral do
  defstruct [:token, :value]
end

defimpl String.Chars, for: Monkey.AST.IntegerLiteral do
  def to_string(il) do
    Kernel.to_string(il.value)
  end
end
