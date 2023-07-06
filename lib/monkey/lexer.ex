defmodule Monkey.Lexer do
  @keywords %{
    "fn" => :function,
    "let" => :let,
    "true" => true,
    "false" => false,
    "if" => :if,
    "else" => :else,
    "return" => :return
  }

  @letters [?a..?z, ?A..?Z, '_']
           |> Enum.flat_map(&Enum.to_list/1)
           |> Enum.map(&to_string([&1]))

  @digits ?0..?9
          |> Enum.map(&to_string([&1]))

  def stream_tokens(input) do
    Stream.resource(
      fn -> String.graphemes(input) end,
      &next_token/1,
      &Function.identity/1
    )
  end

  # Skip whitespace
  defp next_token([" " | rest]), do: {[], rest}
  defp next_token(["\t" | rest]), do: {[], rest}
  defp next_token(["\r" | rest]), do: {[], rest}
  defp next_token(["\n" | rest]), do: {[], rest}

  # Two-character operators
  defp next_token(["=", "=" | rest]), do: {[{:eq, "=="}], rest}
  defp next_token(["!", "=" | rest]), do: {[{:not_eq, "!="}], rest}

  # Single-character operators
  for {name, char} <- [
        assign: "=",
        plus: "+",
        minus: "-",
        bang: "!",
        slash: "/",
        asterisk: "*",
        lt: "<",
        gt: ">",
        lparen: "(",
        rparen: ")",
        lbrace: "{",
        rbrace: "}",
        comma: ",",
        semicolon: ";"
      ] do
    defp next_token([unquote(char) | rest]), do: {[{unquote(name), unquote(char)}], rest}
  end

  # Identifiers
  defp next_token([char | _] = chars) when char in @letters do
    {identifier, rest} = Enum.split_while(chars, &(&1 in @letters))
    identifier = Enum.join(identifier)

    type = Map.get(@keywords, identifier, :ident)

    {[{type, identifier}], rest}
  end

  # Integers
  defp next_token([char | _] = chars) when char in @digits do
    {int, rest} = Enum.split_while(chars, &(&1 in @digits))
    int = Enum.join(int)

    {[{:int, int}], rest}
  end

  defp next_token(<<char, rest::binary>>) do
    {[{:illegal, char}], rest}
  end

  defp next_token([]) do
    {[{:eof, ""}], :done}
  end

  defp next_token(:done), do: {:halt, ""}
end
