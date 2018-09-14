#include "Lexer.h"
#include "Error.h"

#include <iterator>
#include <string>
#include <variant>
#include <vector>

Lexer::Lexer(std::string line) {
    this->rest = line;
}

std::optional<std::string> Lexer::consumeOpenBracket() {
    if (this->rest.at(0) == '[') {
        this->rest = this->rest.substr(1);
        return "[";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeCloseBracket() {
    if (this->rest.at(0) == ']') {
        this->rest = this->rest.substr(1);
        return "]";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeLineBreak() {
    if (this->rest.at(0) == '\n') {
        this->rest = this->rest.substr(1);
        return "\n";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeSingleColon() {
    if (this->rest.at(0) == ':') {
        this->rest = this->rest.substr(1);
        return ":";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeColonEqual() {
    if (this->rest.substr(0,2) == ":=") {
        this->rest = this->rest.substr(2);
        return ":=";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeArrow() {
    if (this->rest.substr(0,2) == "->") {
        this->rest = this->rest.substr(2);
        return "->";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeString() {
    std::string out = "";
    if (this->rest.at(0) == '\"') {
        // TODO: string escaping
        while (this->rest.at(0) != '\"') {
            if (this->rest.length() == 0) {
                parsetime_die("Unterminated string.");
            } else {
                out += this->rest.substr(0,1);
                this->rest = this->rest.substr(1);
            }
        }
        this->rest = this->rest.substr(1);
        return out;
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeNumber() {
    std::string out = "";
    // First, trim out any possible positive/negative signs
    if (this->rest.at(0) == '-') {
        out = "-" + out;
        this->rest = this->rest.substr(1);
    }
    else if (this->rest.at(0) == '+') {
        this->rest = this->rest.substr(1);
    }
    // Then make sure we have an actual number
    if (std::isdigit(this->rest.at(0))) {
        while (std::isdigit(this->rest.at(0))) {
            // Pop that number into out
            out = out + this->rest.substr(0, 1);
            this->rest = this->rest.substr(1);
            // Check edge cases
            if (this->rest.length() == 0) {
                break;
            }
            if (this->rest.at(0) == '.') {
                out = "." + out;
                this->rest = this->rest.substr(1);
            }
        }
        return out;
    } else {
        return std::nullopt;
    }
}
std::optional<std::string> Lexer::consumeWhitespace() {
    std::string out = "";
    if (std::isspace(this->rest.at(0))) {
        while (std::isspace(this->rest.at(0))) {
            out = out + this->rest.at(0);
            this->rest = this->rest.substr(1);
            if (this->rest.length() == 0) {
                return out;
            }
        }
        return out;
    } else {
        return std::nullopt;
    }
}
std::optional<std::string> Lexer::consumeIdent() {
    std::string out = "";
    // This comes after consuming whitespace and consuming
    // a number, so it matches very greedily
    if (!std::isspace(this->rest.at(0))) {
        while (!std::isspace(this->rest.at(0))) {
            out = out + this->rest.at(0);
            this->rest = this->rest.substr(1);
            if (this->rest.length() == 0) {
                return out;
            }
        }
        return out;
    }
    return std::nullopt;
}

Lexeme Lexer::consumeLexeme() {
    std::string backtrack = this->rest;
    // All this code is the equivalent of chaining with the `<<`
    // operator in Haskell
    Lexeme lexeme;
    std::optional<std::string> lexeme_str = "";

    if (Lexer::consumeOpenBracket()) {
        lexeme.lexeme = (struct Lexeme::OpenBracket){};
        goto success;
    }
    if (Lexer::consumeCloseBracket()) {
        lexeme.lexeme = (struct Lexeme::CloseBracket){};
        goto success;
    }
    if (Lexer::consumeLineBreak()) {
        lexeme.lexeme = (struct Lexeme::LineBreak){};
        goto success;
    }
    if (Lexer::consumeSingleColon()) {
        lexeme.lexeme = (struct Lexeme::SingleColon){};
        goto success;
    }
    if (Lexer::consumeArrow()) {
        lexeme.lexeme = (struct Lexeme::Arrow){};
        goto success;
    }
    // the above methods do not need backtracking
    // the below methods do
    if (lexeme_str = Lexer::consumeString()) {
        lexeme.lexeme = (struct Lexeme::String){ .string = lexeme_str.value() };
        goto success;
    }
    this->rest = backtrack;
    if (lexeme_str = Lexer::consumeNumber()) {
        lexeme.lexeme = (struct Lexeme::Number){ .number = lexeme_str.value() };
        goto success;
    }
    this->rest = backtrack;
    if (lexeme_str = Lexer::consumeIdent()) {
        lexeme.lexeme = (struct Lexeme::Ident){ .ident = lexeme_str.value() };
        goto success;
    }
    // Nothing matched, so we error
    parsetime_die("AAAAA");
    success:
    return lexeme;
}

std::vector<Lexeme> Lexer::consumeAllLexemes() {
    std::vector<Lexeme> out;
    while (this->rest.length() > 0) {
        while (Lexer::consumeWhitespace()) {}
        out.push_back(Lexer::consumeLexeme());
    }
}

// f : R^2 -> R^3
// f ([x; y]) = [x; y; 0]

// g : R^2 -> R^2
// g rotates a vector 45 degrees clockwise
