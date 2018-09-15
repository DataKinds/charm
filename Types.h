#pragma once

#include <variant>
#include <vector>

#include "ParserTypes.h"

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
};
