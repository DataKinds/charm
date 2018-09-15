#include <string>
#include <variant>
#include <optional>
#include <vector>

#include "Types.h"

class Lexer {
public:
    // all of these functions will fail if fed an empty string
    // it is NOT these functions' job to backtrack on failure
    std::optional<std::string> consumeOpenBracket();
    std::optional<std::string> consumeCloseBracket();
    std::optional<std::string> consumeLineBreak();
    std::optional<std::string> consumeSingleColon();
    std::optional<std::string> consumeColonEqual();
    std::optional<std::string> consumeArrow();
    std::optional<std::string> consumeWhitespace();
    std::optional<std::string> consumeString();
    std::optional<std::string> consumeNumber();
    std::optional<std::string> consumeIdent();

    Lexeme consumeLexeme();
    std::vector<Lexeme> consumeAllLexemes();

    Lexer(std::string line);
private:
    std::string rest;
};
