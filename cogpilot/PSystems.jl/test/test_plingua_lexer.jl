using Test
using PSystems
using PSystems: tokenize, Token, TK_IDENTIFIER, TK_NUMBER, TK_LBRACKET, TK_RBRACKET,
                TK_ARROW, TK_COMMA, TK_SEMICOLON, TK_AT, TK_EQUALS, TK_APOSTROPHE,
                TK_LBRACE, TK_RBRACE, TK_MODEL, TK_DEF, TK_LPAREN, TK_RPAREN,
                TK_LT, TK_GT, TK_EOF

@testset "Lexer Basic Tokens" begin
    # Identifiers
    tokens = tokenize("abc def_123")
    @test tokens[1].type == TK_IDENTIFIER
    @test tokens[1].value == "abc"
    @test tokens[2].type == TK_IDENTIFIER
    @test tokens[2].value == "def_123"
    
    # Numbers
    tokens = tokenize("123 456")
    @test tokens[1].type == TK_NUMBER
    @test tokens[1].value == "123"
    @test tokens[2].type == TK_NUMBER
    @test tokens[2].value == "456"
end

@testset "Lexer Keywords" begin
    tokens = tokenize("model def out in")
    @test tokens[1].type == TK_MODEL
    @test tokens[2].type == TK_DEF
    @test tokens[3].value == "out"
    @test tokens[4].value == "in"
end

@testset "Lexer Operators" begin
    tokens = tokenize("[]{}(),;='@<>-->")
    @test tokens[1].type == TK_LBRACKET
    @test tokens[2].type == TK_RBRACKET
    @test tokens[3].type == TK_LBRACE
    @test tokens[4].type == TK_RBRACE
    @test tokens[5].type == TK_LPAREN
    @test tokens[6].type == TK_RPAREN
    @test tokens[7].type == TK_COMMA
    @test tokens[8].type == TK_SEMICOLON
    @test tokens[9].type == TK_EQUALS
    @test tokens[10].type == TK_APOSTROPHE
    @test tokens[11].type == TK_AT
    @test tokens[12].type == TK_LT
    @test tokens[13].type == TK_GT
    @test tokens[14].type == TK_ARROW
    @test tokens[14].value == "-->"
end

@testset "Lexer Whitespace and Comments" begin
    source = """
    // This is a comment
    abc /* multi
    line comment */ xyz
    """
    tokens = tokenize(source)
    
    # Should only have abc, xyz, EOF
    @test tokens[1].type == TK_IDENTIFIER
    @test tokens[1].value == "abc"
    @test tokens[2].type == TK_IDENTIFIER
    @test tokens[2].value == "xyz"
    @test tokens[3].type == TK_EOF
end

@testset "Lexer Line and Column Tracking" begin
    source = """
    abc
    def
    """
    tokens = tokenize(source)
    
    @test tokens[1].line == 1
    @test tokens[1].column == 1
    @test tokens[2].line == 2
    @test tokens[2].column == 1
end

@testset "Lexer P-Lingua Sample" begin
    source = "@mu = []'1;"
    tokens = tokenize(source)
    
    @test tokens[1].type == TK_AT
    @test tokens[2].type == TK_IDENTIFIER
    @test tokens[2].value == "mu"
    @test tokens[3].type == TK_EQUALS
    @test tokens[4].type == TK_LBRACKET
    @test tokens[5].type == TK_RBRACKET
    @test tokens[6].type == TK_APOSTROPHE
    @test tokens[7].type == TK_NUMBER
    @test tokens[7].value == "1"
    @test tokens[8].type == TK_SEMICOLON
end
