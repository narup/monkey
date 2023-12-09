## Monkey Programming Language 
from python import Python 
from python.object import PythonObject
from memory.unsafe import Pointer

fn main() raises:
    print("Monkey>>")
    run_tests()

fn run_tests() raises:
    test_lexer()

fn test_lexer() raises:
    var input_cases = Array[String](1)
    input_cases.__set_item__(String("x"))

    let output_cases = Array[Array[String]](1)

    let N: Int = test_case_keys.__len__().__index__()
    for i in range(N):
        let input = test_case_inputs.get(test_case_keys[i])
        let lexer = Lexer(input.to_string())
        let t = lexer.next_token()

        let output = test_case_outputs.get(test_case_keys[i])
        let o = Python.type(output)
        print(o.to_string())

        if t.literal_val() != output.to_string():
            print("Test failed")
        else:
            print("all test passed")


# Lexer implementation 
struct Lexer:
    var input: String
    var current_index: Int
    var peek_index: Int

    fn __init__(inout self, owned input: String):
        self.input = input 
        self.current_index = 0 
        self.peek_index = 0 

    fn next_token(self) raises -> Token:
        for i in range(len(self.input)):
            if self.__is_alphanumeric__(self.input[i]):
                return Token(TokenType.IDENTIFIER, self.input[i]) 

        return Token(TokenType.ILLEGAL, String("ILLEGAL"))

    fn __is_alphanumeric__(self, ch: String) raises -> Bool:
        return isdigit(ord(ch)) or (ord(ch) >= 65 and ord(ch) <= 122)


# Token
@value 
struct Token:
    var token_type: TokenType
    var literal: String 

    fn __init__(inout self, token_type: TokenType, literal: String):
        self.literal = literal
        self.token_type = token_type

    fn literal_val(self) -> String:
        return self.literal

@value
struct TokenType:
    var val: String

    fn __init__(inout self, val: String):
        self.val = val 

    alias LET = TokenType("let")
    alias IDENTIFIER = TokenType("identifier")
    alias NUMBER = TokenType("number")
    alias STRING = TokenType("string")
    alias EOF = TokenType("EOF")
    alias ILLEGAL = TokenType("ILLEGAL")


struct Array[T: AnyType]:
    var data: Pointer[T]
    var used_index: Int
    var size: Int

    fn __init__(inout self, size: Int):
        self.size = size
        self.used_index = 0
        self.data = Pointer[T].alloc(self.size)

    fn __init__(inout self, size: Int, value: T):
        self.size = size
        self.used_index = 0
        self.data = Pointer[T].alloc(self.size)
        for i in range(self.size):
            self.data.store(i, value)
            self.used_index = i

    fn __set_item__(inout self, value: T):
        self.data.store(self.used_index, value)
        self.used_index = self.used_index + 1

    fn __get_item__(self, i: Int) -> T:
        return self.data.load(i)

    fn __del__(owned self):
        self.data.free()
