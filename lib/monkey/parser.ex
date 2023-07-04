defmodule Monkey.Parser do
  defstruct stream: nil,
            cur_token: nil,
            peek_token: nil,
            program: %Monkey.AST.Program{},
            errors: []

  alias Monkey.AST
  alias Monkey.Token

  @precedences %{
    lowest: 0,
    equals: 1,
    less_greater: 2,
    sum: 3,
    product: 4,
    prefix: 5,
    call: 6
  }

  def new(token_stream) do
    %__MODULE__{
      stream: token_stream
    }
    |> next_token()
    |> next_token()
  end

  def parse_program(parser) do
    parse_statements(parser)
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

  # let
  defp parse_statement(%{cur_token: {:let, _} = let_token} = parser) do
    with {:ok, parser} <- expect_peek(parser, :ident),
         name = %AST.Identifier{token: parser.cur_token, value: Token.literal(parser.cur_token)},
         {:ok, parser} <- expect_peek(parser, :assign),
         parser = skip_to_semicolon(parser) do
      {%AST.LetStatement{token: let_token, name: name}, parser}
    else
      {:error, %__MODULE__{} = parser} -> {nil, parser}
    end
  end

  # return
  defp parse_statement(%{cur_token: {:return, _} = return_token} = parser) do
    # TODO: expression parsing
    return = %AST.ReturnStatement{token: return_token}
    parser = parser |> next_token() |> skip_to_semicolon()
    {return, parser}
  end

  # expression
  defp parse_statement(%{cur_token: token} = parser) do
    {exp, parser} = parse_expression(parser)
    statement = %AST.ExpressionStatement{token: token, expression: exp}
    parser = optional_semicolon(parser)
    {statement, parser}
  end

  defp parse_expression(parser, precedence \\ :lowest) do
    with {:ok, exp, parser} <- parse_prefix_expression(parser, Token.type(parser.cur_token)) do
      {exp, parser}
    else
      :error -> {nil, parser}
    end
  end

  # Temporary until we can parse expressions
  defp skip_to_semicolon(%{cur_token: {:semicolon, _}} = parser), do: parser
  defp skip_to_semicolon(parser), do: parser |> next_token() |> skip_to_semicolon()

  defp optional_semicolon(%{peek_token: {:semicolon, _}} = parser), do: next_token(parser)
  defp optional_semicolon(parser), do: parser

  defp next_token(parser) do
    cur_token = parser.peek_token
    peek_token = parser.stream |> Enum.take(1) |> List.first()
    stream = Stream.drop(parser.stream, 1)

    %{parser | cur_token: cur_token, peek_token: peek_token, stream: stream}
  end

  defp expect_peek(%{peek_token: {type, _}} = parser, type), do: {:ok, next_token(parser)}

  defp expect_peek(parser, type) do
    parser =
      add_error(
        parser,
        "expected next token to be #{inspect(type)}, got #{inspect(Token.type(parser.peek_token))}"
      )

    {:error, parser}
  end

  defp add_error(parser, message) do
    %{parser | errors: parser.errors ++ [message]}
  end

  # identifiers
  defp parse_prefix_expression(parser, :ident) do
    {
      :ok,
      %AST.Identifier{token: parser.cur_token, value: Token.literal(parser.cur_token)},
      parser
    }
  end

  # integer literals
  defp parse_prefix_expression(parser, :int) do
    case parser.cur_token |> Token.literal() |> Integer.parse() do
      {int, _} ->
        {
          :ok,
          %AST.IntegerLiteral{
            token: parser.cur_token,
            value: int
          },
          parser
        }

      :error ->
        {:ok, nil,
         add_error(parser, "could not parse #{Token.literal(parser.cur_token)} as integer")}
    end
  end

  # bang, minus
  defp parse_prefix_expression(%{cur_token: token} = parser, op) when op in [:bang, :minus] do
    {exp, parser} = parser |> next_token() |> parse_expression()

    prefix = %AST.PrefixExpression{
      token: token,
      operator: Token.literal(token),
      right: exp
    }

    {:ok, prefix, parser}
  end

  defp parse_prefix_expression(parser, type) do
    {:ok, nil, add_error(parser, "no prefix parse fn for #{inspect(type)}")}
  end
end
