defprotocol Monkey.AST.Node do
  @fallback_to_any true

  def token_literal(node)
end

defimpl Monkey.AST.Node, for: Any do
  def token_literal(node), do: Monkey.Token.literal(node)
end
