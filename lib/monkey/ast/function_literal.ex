defmodule Monkey.AST.FunctionLiteral do
  defstruct [:token, :parameters, :body]
end

defimpl String.Chars, for: Monkey.AST.FunctionLiteral do
  def to_string(node) do
    params = Enum.map(node.params, &Kernel.to_string/1)
    "fn(#{Enum.join(params, ", ")}) #{node.body}"
  end
end
