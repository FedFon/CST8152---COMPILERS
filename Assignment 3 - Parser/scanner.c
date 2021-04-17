/*
* File name	:	scanner.c
* Compiler	:	MS Visual Studio 2019
* Author	:	Svillen Ranev and Federico Fonseca 040845199
* Course	:	CST 8152 - Compilers, Lab Section: 011
* Assignment:	2
* Date		:	2020-07-29
* Professor	:	Sv. Ranev and Paulo Sousa
* Purpose	:	This file contains the implementation of a scanner that uses a buffer and produces tokens from a stream of lexemes
* Function List: scanner_init(), malar_next_token(), get_next_state(), aa_func02(), aa_func03(), aa_func05(), aa_func08()
* aa_func10(), aa_func11(), iskerword()
*/

 /* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
  * to suppress the warnings about using "unsafe" functions like fopen()
  * and standard sting library functions defined in string.h.
  * The define does not have any effect in Borland compiler projects.
  */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */

/*
* Purpose			:	This function set up sc_buf to be able to be used in the program
* Author			:	Svillen Ranev and Federico Fonseca
* History/Versions	:	01 2020-07-29
* Called functions	:	b_isempty(), b_rewind(), b_clear()
* Parameters		:	PBuffer psc_buf
* Return value		:	int
* Algotrithm		:	Checks if buffer is empty, rewinds the buffer, clears a buffer, returns success
*/
/*Initializes scanner */
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
* Purpose			: This function is in charge of producing the token for the Scanner
* Author			: Svillen Ranev and Federico Fonseca
* History/Versions	: 01 2020-07-29
* Called functions	: b_getc(), b_retract(), b_mark(), b_reset(), b_getcoffset(), strcpy(), get_next_state(), b_allocate()
*					  b_addc(), b_free()
* Parameters		: void
* Return value		: Token
* Algotrithm		: Uses switch statement to process special case tokens, then using a finite state machine will process the lexemes outputed by the buffer used
*/
Token malar_next_token(void) {
	{
		Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
		unsigned char c; /* input symbol */
		int state = 0; /* initial state of the FSM */
		short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
		short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

		/*DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED*/

		while (1) { /* endless loop broken by token returns it will generate a warning */

			c = b_getc(sc_buf);


			/* Part 1: Implementation of token driven scanner */
			/*Special case tokens*/
			if (c == NSEOF || c == SEOF) {/*Source end of file checked first*/
				t.code = SEOF_T;
				return t;
			}

			switch (c) {/*switch statement*/

			case '=':
				c = b_getc(sc_buf);/*have to check for second = in case it is a comment*/
				if (c == '=') {/*if it is =*/
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
					return t;
				}
				b_retract(sc_buf);/*if it is not = so we don't lose the next character*/
				t.code = ASS_OP_T;
				return t;

			case ' ':/*space*/
				continue;
			case '(':/*left parentheses*/
				t.code = LPR_T;
				return t;
			case ')':/*right parentheses*/
				t.code = RPR_T;
				return t;
			case '{':/*left bracket*/
				t.code = LBR_T;
				return t;
			case '}':/*left bracket*/
				t.code = RBR_T;
				return t;
			case '<':/*smaller than*/
				c = b_getc(sc_buf);/*have to check next character for >*/
				if (c == '>') {/*if it is >*/
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
					return t;
				}
				b_retract(sc_buf);/*if it is not > so we don't lose the next character*/
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				return t;
			case '>':/*bigger than*/
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;
			case ';':/*end of statement*/
				t.code = EOS_T;
				return t;
			case '!':/*possible comment*/
				c = b_getc(sc_buf);/*get next character to check if comment*/
				if (c == '!') {
					while (c != '\n') {/*ignore the rest of the line*/
						c = b_getc(sc_buf);
						if (c == NSEOF || c == SEOF) {
							b_retract(sc_buf);/*retract SEOF so buffer can end*/
							break;
						}
					}
					line++;/*if broke out of while means line ended*/
				} else {/*is not a comment*/
					t.code = ERR_T;
					t.attribute.err_lex[0] = '!';/*assigning error attribute*/
					t.attribute.err_lex[1] = c;
					while (c != '\n') {/*ignore the rest of the line*/
						if (c == NSEOF || c == SEOF) {
							b_retract(sc_buf);/*retract SEOF so buffer can end*/
							break;
						}
						c = b_getc(sc_buf);
					}
					line++;/*if broke out of while means line ended*/
					return t;
				}
				continue;
			case ',':/*comma*/
				t.code = COM_T;
				return t;
			case '-':/*minus*/
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;
			case '+':/*plus*/
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;
			case '*':/*mulitplication*/
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;
			case '/':/*division*/
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;
			case '#':/*checking foir concatenation*/
				c = b_getc(sc_buf);/*checking for next charact to be #*/
				if (c == '#') {
					t.code = SCC_OP_T;
					return t;
				}
				else {/*it is not #*/
					b_retract(sc_buf);/*so we don't lose the next character*/
					t.code = ERR_T;
					t.attribute.err_lex[0] = '#';/*error attribute*/
					return t;
				}
			case '\n':/*new line*/
				line++;
				continue;
			case '\t':/*tab*/
				continue;
			case '.':/*checking for logical operators*/
				b_markc(sc_buf, b_getcoffset(sc_buf));/*set the mark so we can go back, instead of using retract*/
				c = b_getc(sc_buf);
				if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {/*check is next character are A,N,D and .*/
					t.code = LOG_OP_T;
					t.attribute.log_op = AND;
					return t;
				}
				
				else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.') {/*check is next character are O,R and .*/
					t.code = LOG_OP_T;
					t.attribute.log_op = OR;
					return t;	
				}
				else {/*if not*/
					b_reset(sc_buf);/*returns the buffer to the mark set*/
					t.code = ERR_T;
					t.attribute.err_lex[0] = '.';/*setting error attribute*/
					return t;
				}
			case RT_FAIL_1:
			case RT_FAIL_2:
				scerrnum = 1;
				t.code = RTE_T;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");

			default:/*anything else will move on to next phase*/
				break;

			}/*end of switch statement*/

			

			/* Part 2: Implementation of Finite State Machine (DFA)
					   or Transition Table driven Scanner */
					 

			lexstart = b_markc(sc_buf, b_getcoffset(sc_buf) - 1);/*setting mark so we can return to it*/

			state = get_next_state(state, c);/*get next state*/
			while (as_table[state] == NOAS) {/*while loop to find accepting state*/
				c = b_getc(sc_buf);/*get next character in buffer*/
				state = get_next_state(state, c);
			}
		
			b_retract(sc_buf);/*retract the buffer and recover last character*/
		
			if (as_table[state] == ASWR) {/*if acceptring state with retract*/
				b_retract(sc_buf);
			}

			lexend = b_getcoffset(sc_buf);/*setting the end of the lexeme*/
			
			lex_buf = b_allocate(DEFAULT_INIT_CAPACITY, DEFAULT_INC_FACTOR, 'f');/*allocating a buffer*/
			if (!lex_buf) {/*if failed to allocate*/
				t.code = ERR_T;
				return t;
			}
			b_reset(sc_buf);/*reset to mark*/

			for (int i = lexstart; i <= lexend; i++) {/*from beginning of lexeme to the end*/

				c = b_getc(sc_buf);/*get next character*/

				if (!b_addc(lex_buf, c)) {/* add it to temp buffer*/
					scerrnum = 1;
					t.code = RTE_T;/*if can't add there is a run time error*/
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					return t;
				}
			}
			b_addc(lex_buf, SEOF);/*add SEOF to temp buffer*/
			t = aa_table[state](b_location(lex_buf, NULL));/*call accepting function using aa_table[]*/
			b_free(lex_buf);/*free the temp buffer*/
			return t;
		}//end while(1)
	}
}

			/*
			* Purpose			: The purpose of this function is to return the next state for the FSM.
			* Author			: Svillen Ranev and Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: char_class(), assert()
			* Parameters		: int, char
			* Return value		: int
			* Algotrithm		: Uses the char and int values passed on by the function call. After uses the char_class method to get the next state and retruns it.
			*/
			int get_next_state(int state, char c)
			{	
				int col;
				int next;
				col = char_class(c);
				next = st_table[state][col];
#ifdef DEBUG
				printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
			
				assert(next != IS);

#ifdef DEBUG
				if (next == IS) {
					printf("Scanner Error: Illegal state:\n");
					printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
					exit(1);
				}
#endif
				return next;
			}
			/*
			* Purpose			: The purpose of this function is to return an int depending on the input
			* Author			: Svillen Ranev and Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: N/A
			* Parameters		: char
			* Return value		: int
			* Algotrithm		: Uses the char from the input of the function call and based on what it
			*					is will return an int.
			*/
			int char_class(char c)
			{
				int val;
			
				if (isalpha(c)) {/*[a-zA-Z]*/
					val = 0;
				} 
				else if (c == '0') {/*0*/
					val = 1;
				}
				else if (isdigit(c) && c != '0') {/*[1-9]*/
					val = 2;
				}
				else if (c == '.') {/*.*/
					val = 3;
				}
				else if (c == '#') {/*#*/
					val = 4;
				}
				else if (c == '"') {/*"*/
					val = 6;
				}
				else if (c == NSEOF || c == SEOF) {/*255 or \0*/
					val = 7;
				}
				else {/*anything else*/
					val = 5;
				}
				return val;
			}


			/*
			* Purpose			: ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords(VID - AVID / KW)
			* Author			: Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: iskeyword(), strlen(), strcpy()
			* Parameters		: char[]
			* Return value		: Token
			* Algotrithm		: Checks if keyword, if yes, returns keyword token along with attribute. If no,
			*					 assigns a AVID token along with the lexeme as the attribute. The ammount of the lexeme
			*					 in the attribute depends on the lentgh of the lexeme. Returns the Token
			*/
			Token aa_func02(char lexeme[]) {
				Token t = { 0 };
				if (iskeyword(lexeme) != RT_FAIL_2) {/*check if lexeme is a keyword*/
					t.code = KW_T;/*assign token*/
					t.attribute.kwt_idx = iskeyword(lexeme);/*assigns attribute*/
					return t;
				}

				t.code = AVID_T;/*if it is not a keyword it assigns AVID token*/

				if (strlen(lexeme) > VID_LEN) {/*if lexeme is too long*/
					for (int i = 0; i < VID_LEN; i++) {/*store only until VID_LEN*/
						t.attribute.vid_lex[i] = lexeme[i];
					}
					t.attribute.vid_lex[VID_LEN] = SEOF;/*add SEOF at end*/
					return t;
				}

				strcpy(t.attribute.vid_lex, lexeme);/*if it does not surpass VID_LEN*/
				return t;
			}
			/*
			* Purpose			: ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID)
			* Author			: Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: strlen(), strcpy()
			* Parameters		: char[]
			* Return value		: Token
			* Algotrithm		: Assigns SVID token. Depending on the length of the lexeme will assign an attribute 
			*					 to the token, appends a # to the string and a SEOF before retrurning the token.	
			*/
			Token aa_func03(char lexeme[]) {
				Token t = { 0 };

				t.code = SVID_T;/*Set SVID token*/
				
				if (strlen(lexeme) > VID_LEN) {/*if lexeme is longer than VID_LEN*/
					for (int i = 0; i < (VID_LEN - 1); i++) {/*store only VID_LEN - 1 into vid_lex[]*/
						t.attribute.vid_lex[i] = lexeme[i];
					}
					t.attribute.vid_lex[VID_LEN-1] = '#';/*appends # at the end of the string*/
					t.attribute.vid_lex[VID_LEN] = SEOF;/*appends \0 at the end of the string*/
					return t;
				 }
					
				strcpy(t.attribute.vid_lex, lexeme);
				return t;
			}

			
			/*
			* Purpose			: ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant(DIL)
			* Author			: Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: atol(), strlen(), strcat(), strcpy()
			* Parameters		: char[]
			* Return value		: Token
			* Algotrithm		: converts the lexeme into a int, checks the range of the int. If it is out of range
			*					 returns an error token along with its value in the attribute. If it is still in range,
			*					 returns an INL_T token with its value as the attribute.
			*/
			Token aa_func05(char lexeme[]) {
				Token t = { 0 };
				long dil;
				dil = atol(lexeme);/*convert to long*/
				
				if (dil > SHRT_MAX || dil < SHRT_MIN) {
					/*checking for range*/
						/*out of range*/
						t.code = ERR_T;/*setting error token*/
						if (strlen(lexeme) > ERR_LEN) {/**/
							for (int i = 0; i < (ERR_LEN - 3); i++) {/*characters minus 3 stored*/
								t.attribute.err_lex[i] = lexeme[i];
							}
							strcat(t.attribute.err_lex, "...");/*appending three dots at the end of error attribute*/
						}
						else {
							strcpy(t.attribute.err_lex, lexeme);

						}
					
					return t;
				}
				t.code = INL_T;
				t.attribute.int_value = (int)dil;

				return t;
			}
			/*
			* Purpose			: ACCEPTING FUNCTION FOR THE floating - point literal(FPL)
			* Author			: Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: atof(), strlen(), strcat(), strcpy()
			* Parameters		: char[]
			* Return value		: Token
			* Algotrithm		: Converts lexeme to float. Checks range. If out of range returns error token with
			*					 its value as the attribute. If in range, returns a floating point literal token
			*					 with its value as the attribute.
			*/
			Token aa_func08(char lexeme[]) {
				Token t = { 0 };
				double fpl;
				fpl = atof(lexeme);/*convert lexeme to FPL*/

				if (fpl > FLT_MAX || (fpl < FLT_MIN && fpl != 0)) {
					/*checking for range*/
						/*out of range*/
						t.code = ERR_T;/*setting error token*/

						if (strlen(lexeme) > ERR_LEN) {/**/
							for (int i = 0; i < (ERR_LEN - 3); i++) {/*first three character stored*/
								t.attribute.err_lex[i] = lexeme[i];
							}
							strcat(t.attribute.err_lex, "...");/*appending three dots at the end of error attribute*/
						}
						else {
							strcpy(t.attribute.err_lex, lexeme);
						}
						return t;
					}

				t.code = FPL_T;/*setting FPL token*/
				t.attribute.flt_value = (float)fpl;/*storing value into attribute*/

				return t;
			}
			/*
			* Purpose			: ACCEPTING FUNCTION FOR THE string literal(SL)
			* Author			: Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: b_addcoffset(), strlen(), b_addc()
			* Parameters		: char[]
			* Return value		: Token
			* Algotrithm		: Sets the token for a String Literal with the addcoffset as the attribute.
			*					 uses the b_addc function to add the lexeme into the str_LTBL buffer. Adds an \0 
			*					 to make it a C type string. Returns token.
			*/
			Token aa_func10(char lexeme[]) {
				Token t = { 0 };

				t.code = STR_T;/*set token code*/
				t.attribute.str_offset = b_addcoffset(str_LTBL);/*set the addcoffset to the attribute*/

				for (int i = 0; i < strlen(lexeme); i++) {
					if (lexeme[i] != '"') {/*ignores "*/
						b_addc(str_LTBL, lexeme[i]);/*addc lexeme character to string literal table*/
					}
					if (lexeme[i] == '\n') {
						line++;
					}
				}
				b_addc(str_LTBL, SEOF);
	
				return t;
			}
			/*
			* Purpose			: ACCEPTING FUNCTION FOR THE ERROR TOKEN
			* Author			: Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: strlen(), strcat(), strcpy()
			* Parameters		: char[]
			* Return value		: Token
			* Algotrithm		: Sets an error token. Depending on the length of the lexeme will set
			*					 the attribute of the token. Returns token.
			*/
			Token aa_func11(char lexeme[]) {
				Token t = { 0 };
				t.code = ERR_T;
				if (strlen(lexeme) > ERR_LEN) {
					for (int i = 0; i < (ERR_LEN - 3); i++) {/*characters minus 3 stored*/
						t.attribute.err_lex[i] = lexeme[i];
					}
					strcat(t.attribute.err_lex, "...");/*appending three dots at the end of error attribute*/
				}
				else {
					strcpy(t.attribute.err_lex, lexeme);
				}
				for (int i = 0; i < strlen(lexeme); i++) {/*checking if lexeme has any line terminators*/
					if (lexeme[i] == '\n') {
						line++;
					}
				}
				
				return t;
			}
		
			/*
			* Purpose			: This function checks the lexeme to see if it is a keyword.
			* Author			: Federico Fonseca
			* History/Versions	: 01 2020-07-29
			* Called functions	: N/A
			* Parameters		: char*
			* Return value		: int
			* Algotrithm		: Uses kw_table and compares the lexeme to see if it matches with any of the keywords.
			*					 if yes, returns i. If no, returns RT_FAIL_2.
			*/
			int iskeyword(char* lexeme) {
				for (int i = 0; i < KWT_SIZE; i++) {
					if (strcmp(lexeme, kw_table[i]) == 0) {/*compares with kw_table*/
						return i;
					}
				}
				return RT_FAIL_2;
			}

			