#include "Lexer.h"

#include <iterator>

std::optional<std::string> Lexer::consumeOpenBracket() {
    if (this.rest.at(0) == '[') {
        this.rest = this.rest.substr(1);
        return "[";
    }
    return std::nullopt;
}
std::optional<std::string> consumeCloseBracket() {
    if (this.rest.at(0) == ']') {
        this.rest = this.rest.substr(1);
        return "]";
    }
    return std::nullopt;
}
std::optional<std::string> consumeLineBreak() {
    if (this.rest.at(0) == '\n') {
        this.rest = this.rest.substr(1);
        return "\n";
    }
    return std::nullopt;
}
std::optional<std::string> consumeSingleColon() {
    if (this.rest.at(0) == ':') {
        this.rest = this.rest.substr(1);
        return ":";
    }
    return std::nullopt;
}
std::optional<std::string> consumeArrow() {
    if (this.rest.substr(0,2) == "->") {
        this.rest = this.rest.substr(2);
        return "->";
    }
    return std::nullopt;
}
std::optional<std::string> consumeString() {
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
std::optional<std::string> consumeNumber() {
    std::string out = "";
    // TODO
}
std::optional<std::string> consumeIdent() {
    // TODO
}
std::optional<std::string> consumeWhitespace() {
    // TODO
}
