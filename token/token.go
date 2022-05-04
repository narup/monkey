package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"
	//identifiers + literals
	IDENT = "IDENT" // add, foobar, x, y
	INT   = "INT"   //2, 5, 20, 303,5033

	//operators
	ASSIGN = "="
	PLUS   = "+"

	//Delimiters
	COMMA     = ","
	SEMICOLON = ";"
	LPAREN    = "("
	RPAREN    = ")"
	LBRACE    = "{"
	RBRACE    = "}"

	//keywords
	FUNCTION = "FUNCTION"
	LET      = "LET"
)
