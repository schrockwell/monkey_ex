defmodule Monkey.Parser do
  defstruct stream: nil,
            cur_token: nil,
            peek_token: nil,
            program: %Monkey.AST.Program{}

  alias Monkey.AST
  alias Monkey.Token

  def new(token_stream) do
    %__MODULE__{
      stream: token_stream
    }
    |> next_token()
    |> next_token()
  end

  def parse_program(parser) do
    parser = parse_statements(parser)
    parser.program
  end

  defp parse_statements(%{cur_token: {:eof, _}} = parser), do: parser

  defp parse_statements(parser) do
    case parse_statement(parser) do
      {nil, parser} ->
        parser |> next_token() |> parse_statements()

      {statement, parser} ->
        statements = parser.program.statements ++ [statement]
        program = %{parser.program | statements: statements}
        parser = %{parser | program: program}

        parser |> next_token() |> parse_statements()
    end
  end

  defp parse_statement(%{cur_token: {:let, _} = let_token} = parser) do
    with {:ok, parser} <- expect_peek(parser, :ident),
         name = %AST.Identifier{token: parser.cur_token, value: Token.literal(parser.cur_token)},
         {:ok, parser} <- expect_peek(parser, :assign),
         parser <- skip_to_semicolon(parser) do
      {%AST.LetStatement{token: let_token, name: name}, parser}
    else
      {:error, %__MODULE__{} = parser} -> {nil, parser}
    end
  end

  defp parse_statement(parser), do: {nil, parser}

  # Temporary until we can parse expressions
  defp skip_to_semicolon(%{cur_token: {:semicolon, _}} = parser), do: parser
  defp skip_to_semicolon(parser), do: parser |> next_token() |> skip_to_semicolon()

  defp next_token(parser) do
    cur_token = parser.peek_token
    peek_token = parser.stream |> Enum.take(1) |> List.first()
    stream = Stream.drop(parser.stream, 1)

    %{parser | cur_token: cur_token, peek_token: peek_token, stream: stream}
  end

  defp cur_token_is?(%{cur_token: {type, _}}, type), do: true
  defp cur_token_is?(_, _type), do: false

  defp peek_token_is?(%{peek_token: {type, _}}, type), do: true
  defp peek_token_is?(_, _type), do: false

  defp expect_peek(%{peek_token: {type, _}} = parser, type), do: {:ok, next_token(parser)}
  defp expect_peek(parser, _type), do: {:error, parser}

  # defp parse_statement(parser, {:let, _})
end
