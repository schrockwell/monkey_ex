defmodule Monkey.AST.IfExpression do
  defstruct [:token, :condition, :consequence, :alternative]
end

defimpl String.Chars, for: Monkey.AST.IfExpression do
  def to_string(node) do
    if node.alternative do
      "if #{node.condition} #{node.consequence} else #{node.alternative}"
    else
      "if #{node.condition} #{node.consequence}"
    end
  end
end
