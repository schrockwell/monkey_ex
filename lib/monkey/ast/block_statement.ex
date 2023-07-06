defmodule Monkey.AST.BlockStatement do
  defstruct [:token, :statements]
end

defimpl String.Chars, for: Monkey.AST.BlockStatement do
  def to_string(node) do
    Enum.join(node.statements, &Kernel.to_string/1)
  end
end
