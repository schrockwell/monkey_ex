defmodule Monkey.Parser do
  defstruct stream: nil,
            cur_token: nil,
            peek_token: nil,
            program: %Monkey.AST.Program{},
            errors: []

  alias Monkey.AST
  alias Monkey.Token

  @lowest 0
  @equals 1
  @less_greater 2
  @sum 3
  @product 4
  @prefix 5
  @call 6

  @precedences %{
    eq: @equals,
    not_eq: @equals,
    lt: @less_greater,
    gt: @less_greater,
    plus: @sum,
    minus: @sum,
    slash: @product,
    asterisk: @product
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

  defp parse_expression(parser, precedence \\ @lowest) do
    {left, parser} = parse_prefix_expression(parser, Token.type(parser.cur_token))

    if left do
      parse_infix_loop(parser, precedence, left)
    else
      {nil, parser}
    end
  end

  defp parse_infix_loop(%{peek_token: peek} = parser, precedence, left) do
    if Token.type(peek) != :semicolon and precedence < peek_precedence(parser) do
      if infix_fn = infix_parse_fn(Token.type(peek)) do
        # Advance token
        parser = next_token(parser)

        # Parse the infix expression
        {infix, parser} = infix_fn.(parser, left)

        # Iterate
        parse_infix_loop(parser, precedence, infix)
      else
        # If next token is not an infix operator, we're done
        {left, parser}
      end
    else
      # If precedence is too high, or reached a semicolon, we're done
      {left, parser}
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
      %AST.Identifier{token: parser.cur_token, value: Token.literal(parser.cur_token)},
      parser
    }
  end

  # integer literals
  defp parse_prefix_expression(parser, :int) do
    case parser.cur_token |> Token.literal() |> Integer.parse() do
      {int, _} ->
        {
          %AST.IntegerLiteral{
            token: parser.cur_token,
            value: int
          },
          parser
        }

      :error ->
        {nil, add_error(parser, "could not parse #{Token.literal(parser.cur_token)} as integer")}
    end
  end

  # booleans
  defp parse_prefix_expression(parser, bool) when is_boolean(bool) do
    {%AST.Boolean{token: parser.cur_token, value: bool}, parser}
  end

  # bang, minus
  defp parse_prefix_expression(%{cur_token: token} = parser, op) when op in [:bang, :minus] do
    {exp, parser} = parser |> next_token() |> parse_expression(@prefix)

    prefix = %AST.PrefixExpression{
      token: token,
      operator: Token.literal(token),
      right: exp
    }

    {prefix, parser}
  end

  # grouped expressions (parens)
  defp parse_prefix_expression(parser, :lparen) do
    parser = next_token(parser)
    {expression, parser} = parse_expression(parser)

    case expect_peek(parser, :rparen) do
      {:ok, parser} -> {expression, parser}
      {:error, parser} -> {nil, parser}
    end
  end

  # if statements
  defp parse_prefix_expression(parser, :if) do
    with {:ok, condition, consequence, parser} <- parse_if_block(parser) do
      case parse_else_block(parser) do
        {:ok, alternative, parser} ->
          {%AST.IfExpression{
             condition: condition,
             consequence: consequence,
             alternative: alternative
           }, parser}

        {:error, parser} ->
          {nil, parser}
      end
    else
      {:error, parser} -> {nil, parser}
    end
  end

  defp parse_prefix_expression(parser, type) do
    {nil, add_error(parser, "no prefix parse fn for #{inspect(type)}")}
  end

  defp parse_if_block(parser) do
    with {:ok, parser} <- expect_peek(parser, :lparen),
         {condition, parser} = parser |> next_token() |> parse_expression(),
         {:ok, parser} <- expect_peek(parser, :rparen),
         {:ok, parser} <- expect_peek(parser, :lbrace),
         {consequence, parser} = parse_block_statement(parser) do
      {:ok, condition, consequence, parser}
    else
      {:error, parser} -> {:error, parser}
    end
  end

  defp parse_else_block(%{peek_token: {:else, _}} = parser) do
    parser
    |> next_token()
    |> expect_peek(:lbrace)
    |> case do
      {:ok, parser} ->
        {block, parser} = parse_block_statement(parser)
        {:ok, block, parser}

      {:error, parser} ->
        {:error, parser}
    end
  end

  defp parse_else_block(parser), do: {:ok, nil, parser}

  defp parse_block_statement(parser) do
    block = %AST.BlockStatement{statements: [], token: parser.cur_token}
    parser |> next_token() |> parse_block_statement(block)
  end

  defp parse_block_statement(%{cur_token: {type, _}} = parser, block)
       when type in [:rbrace, :eof] do
    {%{block | statements: Enum.reverse(block.statements)}, parser}
  end

  defp parse_block_statement(parser, block) do
    {block, parser} =
      case parse_statement(parser) do
        {nil, parser} ->
          {block, parser}

        {statement, parser} ->
          {%{block | statements: [statement | block.statements]}, parser}
      end

    parser
    |> next_token()
    |> parse_block_statement(block)
  end

  defp parse_infix_expression(%{cur_token: token} = parser, left) do
    precedence = cur_precedence(parser)
    parser = next_token(parser)
    {right, parser} = parse_expression(parser, precedence)

    {%AST.InfixExpression{
       token: token,
       left: left,
       operator: Token.literal(token),
       right: right
     }, parser}
  end

  defp cur_precedence(%{cur_token: {type, _}}), do: Map.get(@precedences, type, @lowest)
  defp cur_precedence(_parser), do: @lowest

  defp peek_precedence(%{peek_token: {type, _}}), do: Map.get(@precedences, type, @lowest)
  defp peek_precedence(_parser), do: @lowest

  defp infix_parse_fn(type)
       when type in [:plus, :minus, :slash, :asterisk, :eq, :not_eq, :lt, :gt] do
    &parse_infix_expression/2
  end

  defp infix_parse_fn(_), do: nil
end
