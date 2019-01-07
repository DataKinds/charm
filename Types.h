#pragma once

#include <variant>
#include <vector>
#include <algorithm>

#include "ParserTypes.h"
#include "Error.h"

// for Lexer.h
struct Lexeme {
    struct OpenBracket { };
    struct CloseBracket { };
    struct LineBreak { };
    struct SingleColon { };
    struct ColonEqual { };
    struct Arrow { };
    struct String { std::string string; }; // any characters between quotes, skipping quotes preceded by backspaces
    struct Number { std::string number; }; // [+-]?<digits>(.<digits>)?
    struct Ident { std::string ident; };   // any non whitespace characters not otherwise captured

    std::variant<
        OpenBracket,
        CloseBracket,
        LineBreak,
        SingleColon,
        ColonEqual,
        Arrow,
        String,
        Number,
        Ident> lexeme;
};
// for Parser.h
struct Token {
	struct TypeSignature {
		std::string functionName;
		std::vector<CharmTypeSignatureUnit> unit;
	};
	struct Definition {
		std::string functionName;
		std::vector<Token> definition;
	};
	struct List { std::vector<Token> list; };
	struct String { std::string string; };
	struct Number { mpf_class number; };
	struct Function { std::string function; };

	std::variant<
		TypeSignature,
		List,
		String,
		Number,
		Definition,
		Function> token;

    CharmFunction toCharmFunction() {
        Token tok = *this;
        CharmFunction out;
        std::visit([&](auto& arg){
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, Token::TypeSignature>) {
                runtime_die("Couldn't convert a type signature to a charm function");
            }
            if constexpr (std::is_same_v<T, Token::List>) {
                out.functionType = LIST_FUNCTION;
                out.literalFunctions = {};
                for (Token inner : arg.list) {
                    out.literalFunctions.push_back(inner.toCharmFunction());
                }
            }
            if constexpr (std::is_same_v<T, Token::String>) {
                out.functionType = STRING_FUNCTION;
                out.stringValue = arg.string;
            }
            if constexpr (std::is_same_v<T, Token::Number>) {
                out.functionType = NUMBER_FUNCTION;
                CharmNumber n;
                n.whichType = FLOAT_VALUE;
                n.floatValue = arg.number;
                out.numberValue = n;
            }
            if constexpr (std::is_same_v<T, Token::Definition>) {
                // FIXME: this should never be used
                out.functionType = FUNCTION_DEFINITION;
                out.functionName = arg.functionName,
                out.literalFunctions = {};
                for (Token inner : arg.definition) {
                    out.literalFunctions.push_back(inner.toCharmFunction());
                }
                out.definitionInfo = {};
            }
            if constexpr (std::is_same_v<T, Token::Function>) {
                out.functionType = DEFINED_FUNCTION;
                out.functionName = arg.function;
            }
        }, tok.token);
        return out;
    }

};
