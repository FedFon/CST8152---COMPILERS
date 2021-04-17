/*
* File name	:	parser.h
* Compiler	:	MS Visual Studio 2019
* Author	:	Federico Fonseca 040845199
* Course	:	CST 8152 - Compilers, Lab Section: 011
* Assignment:	3
* Date		:	2020-08-08
* Professor	:	Sv. Ranev and Paulo Sousa
* Purpose	:	This is the header file for the RDPP
* Function List:
*/
#ifndef PARSER_H_
#define PARSER_H_


#include "token.h"
#include "buffer.h"

#define NO_ATTR -1


/*Variables*/
static Token lookahead;
int synerrno;
extern Token malar_next_token(void); /*from scanner.c*/
extern pBuffer str_LTBL;/*from buffer.h*/
extern int line;/*from scanner.c*/
extern char* kw_table[];/*from table.h*/

typedef enum Keywords {
	ELSE,
	FALSE,
	IF,
	PLATYPUS,
	READ,
	REPEAT,
	THEN,
	TRUE,
	WHILE,
	WRITE
}kw;



/*Function declarations*/
void parser(void);
void match(int, int);
void syn_eh(int);
void syn_printe(void);
void gen_incode(char*);
void program(void);
void opt_statements(void);
void statement(void);
void statements(void);
void statements_prime(void);
void assignment_statement(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void multiplicative_arithmetic_expression(void);
void primary_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void additive_arithmetic_expression_prime(void);
void selection_statement(void);
void iteration_statement(void);
void pre_condition(void);
void conditional_expression(void);
void logical_or_expression(void);
void logical_or_expression_prime(void);
void logical_and_expression(void);
void logical_and_expression_prime(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void primary_s_relational_expression(void);
void relational_operator(void);
void input_statement(void);
void variable_list(void);
void variable_list_prime(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void optional_variable_list(void);
void assignment_expression(void);
void arithmetic_expression(void);
void string_expression(void);
void primary_string_expression(void);
void string_expression_prime(void);


#endif