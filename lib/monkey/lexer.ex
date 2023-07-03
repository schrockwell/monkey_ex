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

  def stream_tokens(input) do
    Stream.resource(
      fn -> input end,
      &next_token/1,
      &Function.identity/1
    )
  end

  # Skip whitespace
  defp next_token(<<" ", rest::binary>>), do: {[], rest}
  defp next_token(<<"\t", rest::binary>>), do: {[], rest}
  defp next_token(<<"\r", rest::binary>>), do: {[], rest}
  defp next_token(<<"\n", rest::binary>>), do: {[], rest}

  # Two-character operators
  defp next_token(<<"==", rest::binary>>), do: {[{:eq, "=="}], rest}
  defp next_token(<<"!=", rest::binary>>), do: {[{:not_eq, "!="}], rest}

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
    defp next_token(<<unquote(char), rest::binary>>), do: {[{unquote(name), unquote(char)}], rest}
  end

  defguardp is_letter_char(char) when char in ?a..?z or char in ?A..?Z or char == '_'
  defguardp is_number_char(char) when char in ?0..?9

  # Identifiers
  defp next_token(<<char, _::binary>> = string) when is_letter_char(char) do
    [identifier] = Regex.run(~r/[a-zA-Z_]+/, string)
    {_, rest} = String.split_at(string, String.length(identifier))

    type = @keywords[identifier] || :ident

    {[{type, identifier}], rest}
  end

  defp next_token(<<char, _::binary>> = string) when is_number_char(char) do
    [int] = Regex.run(~r/[0-9]+/, string)
    {_, rest} = String.split_at(string, String.length(int))

    {[{:int, int}], rest}
  end

  defp next_token(<<char, rest::binary>>) do
    {[{:illegal, char}], rest}
  end

  defp next_token("") do
    {[{:eof, ""}], :done}
  end

  defp next_token(:done), do: {:halt, ""}
end
