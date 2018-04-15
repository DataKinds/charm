/*
 * gui.h
 *
 *  Created on: Apr 14, 2018
 *      Author: iconmaster
 */

#ifndef GUI_H_
#define GUI_H_

#include "Parser.h"
#include "Runner.h"

void charm_gui_init(Parser parser, Runner runner);
void display_output(std::string output);
std::string get_input_line();

#endif /* GUI_H_ */
