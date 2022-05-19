package ast

import "github.com/narup/monkey/token"

//Monkey programming language AST interfaces
type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	statementNode()
}

type Expression interface {
	Node
	expressionNode()
}

// ------

//implementation of AST interfaces

//Program - Node implementation that's the root of AST for Monkey program
type Program struct {
	Statements []Statement
}

func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

//LetStatement - node for the let statement. Needs to track 3 things:
//token of the AST node, identifier, and expression
//Identifier implements the Expression interface to hold the identifier
//of the binding, the x in let x= 5; to keep things simple
type LetStatement struct {
	Token token.Token //the token.LET token
	Name  *Identifier
	Value Expression
}

func (ls *LetStatement) statementNode()       {}
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }

type Identifier struct {
	Token token.Token //the token.IDENT token
	Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
