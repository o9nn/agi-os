"""
P-Lingua Parser - Parses P-Lingua source code into a PSystem.
"""

"""
    Parser

Parser for P-Lingua source code.
"""
mutable struct Parser
    tokens::Vector{Token}
    position::Int
    
    function Parser(tokens::Vector{Token})
        new(tokens, 1)
    end
end

"""
    current_token(parser::Parser)

Get the current token without advancing.
"""
function current_token(parser::Parser)
    if parser.position > length(parser.tokens)
        return parser.tokens[end]  # EOF
    end
    return parser.tokens[parser.position]
end

"""
    advance!(parser::Parser)

Advance to the next token.
"""
function advance!(parser::Parser)
    if parser.position <= length(parser.tokens)
        parser.position += 1
    end
end

"""
    expect!(parser::Parser, type::TokenType)

Expect a specific token type and advance, or throw error.
"""
function expect!(parser::Parser, type::TokenType)
    token = current_token(parser)
    if token.type != type
        throw(ParseError("Expected $type, got $(token.type) at line $(token.line):$(token.column)"))
    end
    advance!(parser)
    return token
end

struct ParseError <: Exception
    msg::String
end

Base.showerror(io::IO, e::ParseError) = print(io, "ParseError: ", e.msg)

"""
    parse_multiset_notation(parser::Parser)

Parse multiset notation like "a{2}, b, c{3}".
"""
function parse_multiset_notation(parser::Parser)
    objects = Dict{String,Int}()
    
    while true
        token = current_token(parser)
        
        if token.type == TK_IDENTIFIER
            obj_name = token.value
            advance!(parser)
            
            # Check for multiplicity {n}
            if current_token(parser).type == TK_LBRACE
                advance!(parser)
                count_token = expect!(parser, TK_NUMBER)
                count = parse(Int, count_token.value)
                expect!(parser, TK_RBRACE)
                objects[obj_name] = count
            else
                objects[obj_name] = 1
            end
            
            # Check for comma (more objects)
            if current_token(parser).type == TK_COMMA
                advance!(parser)
            else
                break
            end
        else
            break
        end
    end
    
    return objects
end

"""
    parse_membrane_structure(parser::Parser)

Parse membrane structure notation like [[]'2]'1.
"""
function parse_membrane_structure(parser::Parser)
    membranes = Membrane[]
    membrane_counter = [0]
    
    function parse_membrane_recursive(parent_id)
        expect!(parser, TK_LBRACKET)
        
        # Create new membrane
        membrane_counter[1] += 1
        membrane_id = membrane_counter[1]
        
        # Parse child membranes
        children_ids = Int[]
        while current_token(parser).type == TK_LBRACKET
            child_id = parse_membrane_recursive(membrane_id)
            push!(children_ids, child_id)
        end
        
        expect!(parser, TK_RBRACKET)
        expect!(parser, TK_APOSTROPHE)
        label_token = expect!(parser, TK_NUMBER)
        label = parse(Int, label_token.value)
        
        # Create membrane
        membrane = Membrane(membrane_id, label, parent_id)
        for child_id in children_ids
            add_child!(membrane, child_id)
        end
        push!(membranes, membrane)
        
        return membrane_id
    end
    
    parse_membrane_recursive(nothing)
    
    return membranes
end

"""
    parse_plingua(source::String)

Parse P-Lingua source code and create a PSystem.

# Example

```julia
plingua_code = \"\"\"
@model<transition>

def main() {
    @mu = []'1;
    @ms(1) = a{2};
    [a]'1 --> [b]'1;
}
\"\"\"

system = parse_plingua(plingua_code)
```
"""
function parse_plingua(source::String)
    # Tokenize
    tokens = tokenize(source)
    parser = Parser(tokens)
    
    # Storage for parsed elements
    membranes = Membrane[]
    alphabet = Set{String}()
    initial_multisets = Dict{Int,Multiset}()
    rules = Rule[]
    model_type = "transition"
    
    # Parse @model directive
    if current_token(parser).type == TK_AT
        advance!(parser)
        expect!(parser, TK_MODEL)
        expect!(parser, TK_LT)
        type_token = expect!(parser, TK_IDENTIFIER)
        model_type = type_token.value
        expect!(parser, TK_GT)
    end
    
    # Parse def block
    expect!(parser, TK_DEF)
    name_token = expect!(parser, TK_IDENTIFIER)
    expect!(parser, TK_LPAREN)
    expect!(parser, TK_RPAREN)
    expect!(parser, TK_LBRACE)
    
    # Parse body
    while current_token(parser).type != TK_RBRACE && current_token(parser).type != TK_EOF
        token = current_token(parser)
        
        if token.type == TK_AT
            advance!(parser)
            directive = current_token(parser)
            
            if directive.type == TK_IDENTIFIER && directive.value == "mu"
                # Parse membrane structure
                advance!(parser)
                expect!(parser, TK_EQUALS)
                membranes = parse_membrane_structure(parser)
                expect!(parser, TK_SEMICOLON)
                
            elseif directive.type == TK_IDENTIFIER && directive.value == "ms"
                # Parse multiset definition
                advance!(parser)
                expect!(parser, TK_LPAREN)
                id_token = expect!(parser, TK_NUMBER)
                membrane_id = parse(Int, id_token.value)
                expect!(parser, TK_RPAREN)
                expect!(parser, TK_EQUALS)
                
                objects = parse_multiset_notation(parser)
                for obj in keys(objects)
                    push!(alphabet, obj)
                end
                
                initial_multisets[membrane_id] = Multiset(objects)
                expect!(parser, TK_SEMICOLON)
            end
            
        elseif token.type == TK_LBRACKET
            # Parse rule
            advance!(parser)
            
            # Parse LHS
            lhs_objects = parse_multiset_notation(parser)
            for obj in keys(lhs_objects)
                push!(alphabet, obj)
            end
            
            expect!(parser, TK_RBRACKET)
            expect!(parser, TK_APOSTROPHE)
            label_token = expect!(parser, TK_NUMBER)
            membrane_label = parse(Int, label_token.value)
            
            expect!(parser, TK_ARROW)
            
            # Parse RHS and target
            rhs_objects = Dict{String,Int}()
            target = :here
            dissolve = false
            
            if current_token(parser).type == TK_LPAREN
                # Communication rule
                advance!(parser)
                
                # Parse RHS multiset (might be empty)
                if current_token(parser).type != TK_COMMA && current_token(parser).type != TK_OUT && current_token(parser).type != TK_IN
                    rhs_objects = parse_multiset_notation(parser)
                    for obj in keys(rhs_objects)
                        push!(alphabet, obj)
                    end
                end
                
                # Expect comma before target (if multiset was provided)
                if current_token(parser).type == TK_COMMA
                    advance!(parser)
                end
                
                target_token = current_token(parser)
                if target_token.type == TK_OUT
                    target = :out
                    advance!(parser)
                elseif target_token.type == TK_IN
                    advance!(parser)
                    if current_token(parser).type == TK_APOSTROPHE
                        # in_id format
                        expect!(parser, TK_APOSTROPHE)
                        id_token = expect!(parser, TK_NUMBER)
                        target_id = parse(Int, id_token.value)
                        target = (:in, target_id)
                    end
                end
                
                expect!(parser, TK_RPAREN)
                
            elseif current_token(parser).type == TK_LBRACKET
                # Standard rewriting rule or dissolution
                advance!(parser)
                
                if current_token(parser).type == TK_RBRACKET
                    # Dissolution rule
                    dissolve = true
                    advance!(parser)
                else
                    # Normal rewriting
                    rhs_objects = parse_multiset_notation(parser)
                    for obj in keys(rhs_objects)
                        push!(alphabet, obj)
                    end
                    expect!(parser, TK_RBRACKET)
                end
                
                expect!(parser, TK_APOSTROPHE)
                rhs_label_token = expect!(parser, TK_NUMBER)
                # Verify same label (simplified)
            end
            
            # Create rule
            rule = Rule(
                membrane_label,
                Multiset(lhs_objects),
                Multiset(rhs_objects),
                target,
                dissolve,
                0  # Default priority
            )
            push!(rules, rule)
            
            expect!(parser, TK_SEMICOLON)
            
        else
            # Skip unknown tokens
            advance!(parser)
        end
    end
    
    expect!(parser, TK_RBRACE)
    
    # Build PSystem
    if isempty(membranes)
        # Default: single skin membrane
        membranes = [Membrane(1, 1, nothing)]
    end
    
    return PSystem(
        membranes=membranes,
        alphabet=collect(alphabet),
        initial_multisets=initial_multisets,
        rules=rules
    )
end
