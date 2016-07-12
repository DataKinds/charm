#include <string>

#include <readline/readline.h>
#include <readline/history.h>

int main(int argc, char const *argv[]) {
	std::string codeInput(readline("Charm$ "));
	return 0;
}
