/*
 * gui.cpp
 *
 *  Created on: Apr 14, 2018
 *      Author: iconmaster
 */

#include "gui.h"
#include "Debug.h"

#include <readline/readline.h>
#include <readline/history.h>
#include <ncurses.h>

// constants
#define CONTROL_C 3

// global variables
Parser* parser;
Runner* runner;
static WINDOW* stack_win;
static WINDOW* readline_win;
static int last_char;
static bool have_input;

// private functions
static int readline_getc(FILE* dummy) {
	have_input = false;
	return last_char;
}

static int readline_input_available() {
	return have_input;
}

static void readline_redisplay() {
	// TODO: handle tabs and other characters with width > 1
	// TODO: handle input going off the edge of the screen
	werase(readline_win);

	mvwprintw(readline_win, 0, 0, "%s%s", rl_display_prompt, rl_line_buffer);
	wmove(readline_win, 0, strlen(rl_display_prompt) + rl_point);
}

static void readline_callback_handler(char* line) {
	add_history(line);
	werase(readline_win);

	// TODO: handle execution here
}

static void exit_gui(int rc) {
	endwin();
	exit(rc);
}

// public interface
void charm_gui_init(Parser _parser, Runner _runner) {
	parser = &_parser;
	runner = &_runner;

	// initialize curses
	initscr();
	raw(); // we want to capture all characters (but NOT via keypad; readline handles that for us)
	noecho(); // headline handles echoing

    if (has_colors()) {
    	start_color();
		use_default_colors();
    }

    stack_win = newwin(LINES-1, COLS, 0, 0);
    readline_win = newwin(1, COLS, LINES-1, 0);

    // initialize readline
    rl_bind_key('\t', rl_insert); // to disable autocomplete (for now)
    rl_catch_signals = 0; // don't catch signals; Curses handles those
    rl_catch_sigwinch = 0;
    rl_deprep_term_function = NULL; // don't handle terminal i/o; Curses also handles that
    rl_prep_term_function = NULL;
    rl_change_environment = 0; // readline will overwrite LINES and COLS if you don't do this!

    // register readline callbacks
    rl_getc_function = readline_getc;
    rl_input_available_hook = readline_input_available;
    rl_redisplay_function = readline_redisplay;
    rl_callback_handler_install("charm> ", readline_callback_handler);

    // do the main GUI loop
    while (true) {
    	int c = wgetch(readline_win);

    	if (c == CONTROL_C)
    		exit_gui(0);

    	last_char = c;
    	have_input = true;
    	rl_callback_read_char();
    }
}
