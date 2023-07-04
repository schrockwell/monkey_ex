defmodule Monkey.AST.LetStatement do
  defstruct [:token, :name, :value]
end

defimpl String.Chars, for: Monkey.AST.LetStatement do
  def to_string(let) do
    "let #{let.name} = #{let.value};"
  end
end
