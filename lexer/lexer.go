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

	l.skipWhitespace()

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
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal = string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, literal)
		}
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
	case token.MINUS:
		tok = newToken(token.MINUS, literal)
	case token.BANG:
		if l.peekChar() == '=' {
			ch := l.ch
			l.readChar()
			literal = string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = newToken(token.BANG, literal)
		}
	case token.SLASH:
		tok = newToken(token.SLASH, literal)
	case token.ASTERISK:
		tok = newToken(token.ASTERISK, literal)
	case token.LT:
		tok = newToken(token.LT, literal)
	case token.GT:
		tok = newToken(token.GT, literal)
	case token.LBRACE:
		tok = newToken(token.LBRACE, literal)
	case token.RBRACE:
		tok = newToken(token.RBRACE, literal)
	default:
		if isLetter(l.ch) {
			//if the current character is a letter then read the rest
			//of the identifier/keyword until it encounters a non-letter-character
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupIdent(tok.Literal)
			//return tok here since we alredy call readChar() when calling readIdentifier()
			return tok
		} else if isDigit(l.ch) {
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			return tok
		} else {
			tok = newToken(token.ILLEGAL, literal)
		}
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

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

//we only read integer numbers to keep things simple!
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

//similar to readChar except that it doesn't increment
//l.position and l.readPosition and only peek ahead
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

//recognize if a character is a letter
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func newToken(tokenType token.TokenType, literal string) token.Token {
	return token.Token{Type: tokenType, Literal: literal}
}
