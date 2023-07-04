defmodule Monkey.AST.Identifier do
  defstruct [:token, :value]
end

defimpl String.Chars, for: Monkey.AST.Identifier do
  def to_string(ident), do: ident.value
end
