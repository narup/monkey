const std = @import("std");

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const assert = std.debug.assert;

pub fn main() !void {
    try print("Welcome to Lox. Version 0.01\n");

    var repl_active: bool = true;
    while (repl_active) {
        try print("lox> ");
        const input = try readInput();
        try printf("Output: {s}\n", .{input});

        repl_active = false;
    }
}

fn readInput() error{InputError}![]const u8 {
    return "this is fake input";
}

fn print(comptime msg: []const u8) !void {
    try printf(msg, .{});
}

fn printf(comptime format: []const u8, args: anytype) !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print(format, args);
    try bw.flush();
}

fn play() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!
}

const Lexer = struct {
    input: []const u8,
    current_index: usize = 0,
    peek_index: usize = 0,

    pub fn init(input: []const u8) Lexer {
        return Lexer{
            .input = input,
        };
    }

    pub fn next_token(self: *Lexer) !Token {
        for (self.input, 0..) |ch, index| {
            try printf("character: {c}\n", .{ch});
            try printf("index: {d}\n", .{index});
        }

        return Token{ .token_type = TokenType.illegal, .literal = "x" };
    }
};

const Token = struct {
    token_type: TokenType,
    literal: []const u8,

    pub fn init(token_type: TokenType, literal: []u8) Token {
        return Token{ .token_type = token_type, .literal = literal };
    }
};

const TokenType = enum {
    let,
    identifier,
    number,
    string,
    eof,
    illegal,
};

test "lexer test" {
    try printf("Running lexer test\n", .{});
    try testTokenize("y", &.{test_token("x", TokenType.illegal)});
    try print("lexer test finished\n");
}

fn testTokenize(source: [:0]const u8, expected_token: []const Token) !void {
    var lexer = Lexer{ .input = source };

    for (expected_token) |token| {
        const next_token = try lexer.next_token();
        try expectEqual(next_token.token_type, token.token_type);
        try expectEqual(next_token.literal, token.literal);
    }
}

fn test_token(literal: []const u8, token_type: TokenType) Token {
    return Token{ .token_type = token_type, .literal = literal };
}

fn compare_strings(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
