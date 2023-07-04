defmodule Monkey.Token do
  @type t :: {atom, String.t()}

  def type(%{token: token}), do: type(token)
  def type({type, _literal}), do: type

  def literal(%{token: token}), do: literal(token)
  def literal({_type, literal}), do: literal
end
