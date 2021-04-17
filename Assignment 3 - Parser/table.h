/* Filename: table.h
 * Transition Table and function declarations necessary for the scanner implementation
 * as required for CST8152 - Assignment #2.
 * Version: 1.20.2
 * Date: 24 May 2020
 * Provided by: Svillen Ranev
 * The file is incomplete. You are to complete it.
 ***************************************************
 * REPLACE THIS HEADER WITH YOUR HEADER
 ***************************************************
 */

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

#define NSEOF 255
#define SEOF '\0'
 /*   Source end-of-file (SEOF) sentinel symbol
  *    '\0' or one of 255,0xFF,EOF
  */

  /*  Special case tokens processed separately one by one
   *  in the token-driven part of the scanner
   *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
   *  white space
   *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', ## ,
   *  .AND., .OR. , SEOF,
   */

//PLACE YOUR CONSTANT DEFINITIONS HERE IF YOU NEED ANY
//
//REPLACE* ESN*and* ESR* WITH YOUR ERROR STATE NUMBER
#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

/* State transition table definition */

//REPLACE* CN* WITH YOUR COLUMN NUMBER

#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
/*[a-zA-Z], 0, [1-9], ., #, other, ", SEOF */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */  {1,6,4,ES,ES,ES,9,ER},
	/* State 1 */  {1,1,1,2,3,2,2,2},
	/* State 2 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 3 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 4 */  {ES,4,4,7,5,5,5,5},
	/* State 5 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 6 */  {ES,6,ES,7,5,5,5,5},
	/* State 7 */  {8,7,7,8,8,8,8,8},
	/* State 8 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 9 */  {9,9,9,9,9,9,10,ER},
	/* State 10 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 11 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 12 */  {IS,IS,IS,IS,IS,IS,IS,IS},
	/* State 13 */  {0,0,0,0,0,0,0,0}
};

	/* Accepting state table definition */
	#define ASWR     0 /* accepting state with retract */
	#define ASNR     1  /* accepting state with no retract */
	#define NOAS     2  /* not accepting state */

int as_table[] = {
	NOAS,
	NOAS,
	ASWR,
	ASNR,
	NOAS,
	ASWR,
	NOAS,
	NOAS,
	ASWR,
	NOAS,
	ASNR,
	ASNR,
	ASWR,
	NULL
	};

/* Accepting action function declarations */

//FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
//ONE FUNCTION PROTOTYPE.THEY ALL RETURN Token AND TAKE
//ONE ARGUMENT : A string REPRESENTING A TOKEN LEXEME.

Token aa_func02(char* lexeme);/*VID AVID/KW*/
Token aa_func03(char* lexeme);/*VID SVID*/
Token aa_func05(char* lexeme);/*DIL*/
Token aa_func08(char* lexeme);/*FPL*/
Token aa_func10(char* lexeme);/*SL*/
Token aa_func11(char* lexeme);/*ES*/
/*Token aa_func12(char* lexeme);ER*/


//Replace XX with the number of the accepting state : 02, 03 and so on.

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/

typedef Token(*PTR_AAF)(char* lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] = {


/*HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
TO ACCEPTING FUNCTIONS.THE ARRAY HAS THE SAME SIZE AS as_table[].
YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
ACCEPTING FUNCTIONS(FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
THE REST OF THE ELEMENTS MUST BE SET TO NULL.*/
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	aa_func11,
	aa_func11

};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char* kw_table[] =
	{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"
	};

#endif
