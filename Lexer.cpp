#include "Lexer.h"

#include <iterator>

std::optional<std::string> Lexer::consumeOpenBracket() {
    if (this.rest.at(0) == '[') {
        this.rest = this.rest.substr(1);
        return "[";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeCloseBracket() {
    if (this.rest.at(0) == ']') {
        this.rest = this.rest.substr(1);
        return "]";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeLineBreak() {
    if (this.rest.at(0) == '\n') {
        this.rest = this.rest.substr(1);
        return "\n";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeSingleColon() {
    if (this.rest.at(0) == ':') {
        this.rest = this.rest.substr(1);
        return ":";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeArrow() {
    if (this.rest.substr(0,2) == "->") {
        this.rest = this.rest.substr(2);
        return "->";
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeString() {
    std::string out = "";
    if (this.rest.at(0) == '\"') {
        for (auto& c = std::next(this.rest.begin());; c++) {
            // TODO: string escaping
            if (c == '\"') {
                this.rest = this.rest.substr(1);
                return out;
            } else if (c == this.rest.end()) {
                parsetime_die("Unterminated string.");
            } else {
                out += this.rest.substr(0,1);
                this.rest = this.rest.substr(1);
            }
        }
    }
    return std::nullopt;
}
std::optional<std::string> Lexer::consumeNumber() {
    std::string out = "";
    // TODO
}
std::optional<std::string> Lexer::consumeIdent() {
    // TODO
}
std::optional<std::string> Lexer::consumeWhitespace() {
    // TODO
}
