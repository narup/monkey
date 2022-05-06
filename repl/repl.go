package repl

import (
	"bufio"
	"fmt"
	"io"

	"github.com/narup/monkey/lexer"
	"github.com/narup/monkey/token"
)

const PROMPT = "mky>> "

//Start read from the input source until newline
//and pass it to the lexer and print all the tokens until
//the lexer encounters EOF
func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Print(PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)

		for tok := l.NextToken(); tok.Type != token.EOF; tok = l.NextToken() {
			fmt.Printf("%+v\n", tok)
		}
	}
}
