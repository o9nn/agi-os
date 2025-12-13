"""
P-Lingua Lexer - Tokenizes P-Lingua source code.
"""

@enum TokenType begin
    # Keywords
    TK_MODEL
    TK_DEF
    TK_MU
    TK_MS
    TK_OUT
    TK_IN
    
    # Literals
    TK_IDENTIFIER
    TK_NUMBER
    TK_STRING
    
    # Operators and delimiters
    TK_ARROW       # -->
    TK_GT          # >
    TK_LPAREN      # (
    TK_RPAREN      # )
    TK_LBRACKET    # [
    TK_RBRACKET    # ]
    TK_LBRACE      # {
    TK_RBRACE      # }
    TK_COMMA       # ,
    TK_SEMICOLON   # ;
    TK_EQUALS      # =
    TK_COLON       # :
    TK_APOSTROPHE  # '
    TK_AT          # @
    TK_LT          # <
    
    # Special
    TK_EOF
    TK_UNKNOWN
end

"""
    Token

Represents a lexical token.
"""
struct Token
    type::TokenType
    value::String
    line::Int
    column::Int
end

"""
    Lexer

Lexer for P-Lingua source code.
"""
mutable struct Lexer
    source::String
    position::Int
    line::Int
    column::Int
    
    function Lexer(source::String)
        new(source, 1, 1, 1)
    end
end

"""
    current_char(lexer::Lexer)

Get the current character without advancing.
"""
function current_char(lexer::Lexer)
    if lexer.position > length(lexer.source)
        return '\0'
    end
    return lexer.source[lexer.position]
end

"""
    advance!(lexer::Lexer)

Advance to the next character.
"""
function advance!(lexer::Lexer)
    if lexer.position <= length(lexer.source)
        if lexer.source[lexer.position] == '\n'
            lexer.line += 1
            lexer.column = 1
        else
            lexer.column += 1
        end
        lexer.position += 1
    end
end

"""
    peek(lexer::Lexer, offset::Int=1)

Look ahead at character at offset.
"""
function peek(lexer::Lexer, offset::Int = 1)
    pos = lexer.position + offset
    if pos > length(lexer.source)
        return '\0'
    end
    return lexer.source[pos]
end

"""
    skip_whitespace!(lexer::Lexer)

Skip whitespace and comments.
"""
function skip_whitespace!(lexer::Lexer)
    while true
        c = current_char(lexer)
        
        if isspace(c)
            advance!(lexer)
        elseif c == '/' && peek(lexer) == '/'
            # Single-line comment
            while current_char(lexer) != '\n' && current_char(lexer) != '\0'
                advance!(lexer)
            end
        elseif c == '/' && peek(lexer) == '*'
            # Multi-line comment
            advance!(lexer)  # '/'
            advance!(lexer)  # '*'
            while !(current_char(lexer) == '*' && peek(lexer) == '/') && current_char(lexer) != '\0'
                advance!(lexer)
            end
            if current_char(lexer) == '*'
                advance!(lexer)  # '*'
                advance!(lexer)  # '/'
            end
        else
            break
        end
    end
end

"""
    read_identifier(lexer::Lexer)

Read an identifier or keyword.
"""
function read_identifier(lexer::Lexer)
    start_pos = lexer.position
    start_line = lexer.line
    start_col = lexer.column
    
    while isletter(current_char(lexer)) || isdigit(current_char(lexer)) || current_char(lexer) == '_'
        advance!(lexer)
    end
    
    value = lexer.source[start_pos:(lexer.position - 1)]
    
    # Check for keywords
    token_type = if value == "model"
        TK_MODEL
    elseif value == "def"
        TK_DEF
    elseif value == "out"
        TK_OUT
    elseif value == "in"
        TK_IN
    else
        TK_IDENTIFIER
    end
    
    return Token(token_type, value, start_line, start_col)
end

"""
    read_number(lexer::Lexer)

Read a number.
"""
function read_number(lexer::Lexer)
    start_pos = lexer.position
    start_line = lexer.line
    start_col = lexer.column
    
    while isdigit(current_char(lexer))
        advance!(lexer)
    end
    
    value = lexer.source[start_pos:(lexer.position - 1)]
    return Token(TK_NUMBER, value, start_line, start_col)
end

"""
    next_token(lexer::Lexer)

Get the next token from the source.
"""
function next_token(lexer::Lexer)
    skip_whitespace!(lexer)
    
    c = current_char(lexer)
    line = lexer.line
    col = lexer.column
    
    if c == '\0'
        return Token(TK_EOF, "", line, col)
    end
    
    # Multi-character operators
    if c == '-' && peek(lexer) == '-' && peek(lexer, 2) == '>'
        advance!(lexer)
        advance!(lexer)
        advance!(lexer)
        return Token(TK_ARROW, "-->", line, col)
    end
    
    # Single character tokens
    if c == '('
        advance!(lexer)
        return Token(TK_LPAREN, "(", line, col)
    elseif c == ')'
        advance!(lexer)
        return Token(TK_RPAREN, ")", line, col)
    elseif c == '['
        advance!(lexer)
        return Token(TK_LBRACKET, "[", line, col)
    elseif c == ']'
        advance!(lexer)
        return Token(TK_RBRACKET, "]", line, col)
    elseif c == '{'
        advance!(lexer)
        return Token(TK_LBRACE, "{", line, col)
    elseif c == '}'
        advance!(lexer)
        return Token(TK_RBRACE, "}", line, col)
    elseif c == ','
        advance!(lexer)
        return Token(TK_COMMA, ",", line, col)
    elseif c == ';'
        advance!(lexer)
        return Token(TK_SEMICOLON, ";", line, col)
    elseif c == '='
        advance!(lexer)
        return Token(TK_EQUALS, "=", line, col)
    elseif c == ':'
        advance!(lexer)
        return Token(TK_COLON, ":", line, col)
    elseif c == '\''
        advance!(lexer)
        return Token(TK_APOSTROPHE, "'", line, col)
    elseif c == '@'
        advance!(lexer)
        return Token(TK_AT, "@", line, col)
    elseif c == '<'
        advance!(lexer)
        return Token(TK_LT, "<", line, col)
    elseif c == '>'
        advance!(lexer)
        return Token(TK_GT, ">", line, col)
    end
    
    # Identifiers and keywords
    if isletter(c) || c == '_'
        return read_identifier(lexer)
    end
    
    # Numbers
    if isdigit(c)
        return read_number(lexer)
    end
    
    # Unknown character
    advance!(lexer)
    return Token(TK_UNKNOWN, string(c), line, col)
end

"""
    tokenize(source::String)

Tokenize P-Lingua source code into a vector of tokens.
"""
function tokenize(source::String)
    lexer = Lexer(source)
    tokens = Token[]
    
    while true
        token = next_token(lexer)
        push!(tokens, token)
        
        if token.type == TK_EOF
            break
        end
    end
    
    return tokens
end
