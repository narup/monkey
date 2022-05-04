package lexer

import "github.com/narup/monkey/token"

//This lexer only supports ASCII characters instead of the full
//unicode range for simplicity. To support Unicode and UTF-8 we need
//to change ch from byte to rune
type Lexer struct {
	input        string
	position     int  //current position in input (points to current char)
	readPosition int  //current reading position in input(after current char)
	ch           byte // current char under examination
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

//NextToken basic structure of this function is to look at the
//current character under examination (l.ch) and return a token
//depending on which character it is. Before returning advance the
//pointers into the input so when we call NextToken() again the
//l.ch field is already updated.
func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	//check for EOF at first
	if l.ch == 0 {
		tok.Literal = ""
		tok.Type = token.EOF

		l.readChar()

		return tok
	}

	literal := string(l.ch)
	switch literal {
	case token.ASSIGN:
		tok = newToken(token.ASSIGN, literal)
	case token.SEMICOLON:
		tok = newToken(token.SEMICOLON, literal)
	case token.LPAREN:
		tok = newToken(token.LPAREN, literal)
	case token.RPAREN:
		tok = newToken(token.RPAREN, literal)
	case token.COMMA:
		tok = newToken(token.COMMA, literal)
	case token.PLUS:
		tok = newToken(token.PLUS, literal)
	case token.LBRACE:
		tok = newToken(token.LBRACE, literal)
	case token.RBRACE:
		tok = newToken(token.RBRACE, literal)
	}

	l.readChar()
	return tok
}

//read the next character and advance the read position
//in the input string
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 //ASCII code for NUL
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition += 1
}

func newToken(tokenType token.TokenType, literal string) token.Token {
	return token.Token{Type: tokenType, Literal: literal}
}
