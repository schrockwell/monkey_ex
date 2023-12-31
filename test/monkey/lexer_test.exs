defmodule Monkey.LexerTest do
  use ExUnit.Case

  alias Monkey.Lexer

  test "stream_tokens/1 works" do
    input = """
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
    	x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
    	return true;
    } else {
    	return false;
    }

    10 == 10;
    10 != 9;
    """

    expected = [
      let: "let",
      ident: "five",
      assign: "=",
      int: "5",
      semicolon: ";",
      let: "let",
      ident: "ten",
      assign: "=",
      int: "10",
      semicolon: ";",
      let: "let",
      ident: "add",
      assign: "=",
      function: "fn",
      lparen: "(",
      ident: "x",
      comma: ",",
      ident: "y",
      rparen: ")",
      lbrace: "{",
      ident: "x",
      plus: "+",
      ident: "y",
      semicolon: ";",
      rbrace: "}",
      semicolon: ";",
      let: "let",
      ident: "result",
      assign: "=",
      ident: "add",
      lparen: "(",
      ident: "five",
      comma: ",",
      ident: "ten",
      rparen: ")",
      semicolon: ";",
      bang: "!",
      minus: "-",
      slash: "/",
      asterisk: "*",
      int: "5",
      semicolon: ";",
      int: "5",
      lt: "<",
      int: "10",
      gt: ">",
      int: "5",
      semicolon: ";",
      if: "if",
      lparen: "(",
      int: "5",
      lt: "<",
      int: "10",
      rparen: ")",
      lbrace: "{",
      return: "return",
      true: "true",
      semicolon: ";",
      rbrace: "}",
      else: "else",
      lbrace: "{",
      return: "return",
      false: "false",
      semicolon: ";",
      rbrace: "}",
      int: "10",
      eq: "==",
      int: "10",
      semicolon: ";",
      int: "10",
      not_eq: "!=",
      int: "9",
      semicolon: ";",
      eof: ""
    ]

    tokens =
      input
      |> Lexer.stream_tokens()
      |> Enum.to_list()

    assert tokens == expected
  end
end
