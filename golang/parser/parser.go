package parser

import (
	"github.com/narup/monkey/ast"
	"github.com/narup/monkey/lexer"
	"github.com/narup/monkey/token"
)

type Parser struct {
	l *lexer.Lexer

	currentToken token.Token
	peekToken    token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l}

	//Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

//ParseProgram implements recursive descent parser
func (p *Parser) ParseProgram() *ast.Program {
	return nil
}
