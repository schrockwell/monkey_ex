defmodule Monkey.AST.ReturnStatement do
  defstruct [:token, :return_value]
end

defimpl String.Chars, for: Monkey.AST.ReturnStatement do
  def to_string(return) do
    "return #{return.return_value};"
  end
end
