/*
* File name	:	parser.c
* Compiler	:	MS Visual Studio 2019
* Author	:	Federico Fonseca 040845199
* Course	:	CST 8152 - Compilers, Lab Section: 011
* Assignment:	3
* Date		:	2020-08-08
* Professor	:	Sv. Ranev and Paulo Sousa
* Purpose	:	This is the source code for the RDPP
* Function List: parser(), match(), syn_eh(), syn_printe(), gen_incode(), program(), opt_statements(), statement()
*			 statement(), statements_prime(), assignment_statement(), arithmetic_expression(), unary_arithmetic_expression(),
*			 additive_arithmetic_expression(), multiplicative_arithmetic_expression(), primary_arithmetic_expression(),
*			 multiplicative_arithmetic_expression_prime(), additive_arithmetic_expression_prime(), selection_statement(),
*			iteration_statement(), pre_condition(), conditional_expression(), logical_or_expression(), logical_or_expression_prime(),
*			logical_and_expression(), logical_and_expression_prime(), relational_expression(), primary_a_relational_expression(),
*			primary_s_relational_expression(), relational_operator(), input_statement(), variable_list(), variable_list_prime(),
*			variable_identifier(), output_statement(), output_list(), optional_variable_list(), assignment_expression(),
*			arithmetic_expression(), string_expression(), primary_string_expression(), string_expression_prime()
*/


/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */


#include <stdio.h>
#include <stdlib.h>

#include "parser.h"

/*
* Purpose			: This is the parser function
* Author			: Federico Fonseca
* History/Versions	: 01 2020-08-12
* Called functions	: malar_next_token(), program(), match(), gen_incode()
* Parameters		: void
* Return value		: void
* Algotrithm		: calls the next token, program executes, matches the SEOF_T token and prints the production
*/
void parser(void) {
	lookahead = malar_next_token();/*calls scanner function*/
	program(); match(SEOF_T, NO_ATTR);/*matches last token SEOF*/
	gen_incode("PLATY: Source file parsed");/*production is recognized*/
}

/*
* Purpose			: This is the match function 
* Author			: Federico Fonseca
* History/Versions	: 01 2020-08-12
* Called functions	: syn_eh(), malar_next_token(), synprinte()
* Parameters		: int, int
* Return value		: void
* Algotrithm		: check if tokens match, if not call function synprinte(), switch statement, checks attribute for spe
*					specific tokens because they would have to match, if not, call error function. Checks for SEOF. gets next token.
*					if the token is an ERR_T, prints error function, gets next token and increments the error counter
*/
void match(int pr_token_code, int pr_token_attribute) {
	if (pr_token_code != lookahead.code) {/*if the match is unsuccessful*/
		syn_eh(pr_token_code);
		return;
	}

	switch (pr_token_code) {/*switch statement*/
		/*if any of these tokens, the attribute must be matched*/
		case KW_T:
		case LOG_OP_T:
		case ART_OP_T:
		case REL_OP_T:
			if (lookahead.attribute.get_int != pr_token_attribute) {
				/*if mismatch*/
				syn_eh(pr_token_code);
				return;
			}
	}

	if (lookahead.code == SEOF_T) {/*if it is SEOF_T*/
		return;
	}
	
	lookahead = malar_next_token();/*if match successful*/

	if (lookahead.code == ERR_T) {/*if the next token is ERR_T*/
		syn_printe();
		lookahead = malar_next_token();
		++synerrno;
	}
}

/*
* Purpose			: This is the error handling function syn_eh. implements simple panic mode error recovery
* Author			: Federico Fonseca
* History/Versions	: 01 2020-08-12
* Called functions	: exit(), malar_next_token()
* Parameters		: int
* Return value		: void
* Algotrithm		: calls syn_printe(), iincrements error counter, gets next token in a loop until SEOF is found,
*					if not SEOF it gets next token.
*/
void syn_eh(int sync_token_code) {

	syn_printe();/*function call*/
	++synerrno;/*increment error counter*/

	while (sync_token_code != lookahead.code) {/*while loop*/
		/*check for SEOF_T before advancing to next token*/
		if(lookahead.code == SEOF_T){
			exit(synerrno);
		}
		lookahead = malar_next_token();
	}/*found token*/

	if (sync_token_code != SEOF_T) {/*not SEOF_T*/
		lookahead = malar_next_token();
	}
	return;
}

/*
* Purpose			: Error printing function
* Author			: Federico Fonseca
* History/Versions	: 01 2020-08-12
* Called functions	: printf()
* Parameters		: void
* Return value		: void
* Algotrithm		: prints error messaged depending on the token passed into it.
*/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		printf("%s\n", b_location(str_LTBL, t.attribute.str_offset));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/*
* Purpose			: gen_incode prints the parameter passed onto it
* Author			: Federico Fonseca
* History/Versions	: 01 2020-08-12
* Called functions	: printf()
* Parameters		: char*
* Return value		: void
* Algotrithm		: prints parameter passed into it.
*/
void gen_incode(char* string) {
	/*print the char pointer that was passed when calling this function*/
	printf("%s\n", string);
}

/*<program>  ->
* 							PLATYPUS {<opt_statements>}
*
*						FIRST(<program>) = { KW_T(PLATYPUS) }
*
*/
void program(void) {
	match(KW_T, PLATYPUS);/*matches PLATYPUS keyword*/
	match(LBR_T, NO_ATTR);/*matches left brace*/
	opt_statements();/*call optional statements function*/
	match(RBR_T, NO_ATTR);/*matches right brace*/
	gen_incode("PLATY: Program parsed");/*production is recognized*/
}

/*<statements> -> <statement><statements'>
*					FIRST(<statements>) = { FIRST(<statement>) }
*					= { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*
*/
void statements(void) {
	statement();/*calls statement function*/
	statements_prime();/*calls statement prime function*/
}

/*<opt_statements> -> <statements> | e
*					FIRST(<opt_statements>) = { FIRST(<statements>) }
*					 = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), e}
*
*
*/
void opt_statements(void) {
	switch (lookahead.code) {/*switch statement used beacause of multiple options*/
		case AVID_T:
		case SVID_T:
			/*if AVID_T or SVID_T*/ 
			statements();/*call statements function*/
			break;
		case KW_T:
			/*if keywords: IF, WHILE, READ, WRITE*/
			if (lookahead.attribute.get_int == IF
				|| lookahead.attribute.get_int == WHILE
				|| lookahead.attribute.get_int == READ
				|| lookahead.attribute.get_int == WRITE) {
				statements();/*call statements function*/
				break;
			}
		default: /*for an empty string*/
			gen_incode("PLATY: Opt_statements parsed");/*production is recognized*/
	}
}

/*<statement> ->
*							 <assignment statement>
*							|<selection statement>
*							|<iteration statement>
*							|<input statement>
*							|<output statement>
*					FIRST(<statement>) = { FIRST(<assignment statement>), FIRST(<selection statements>), FIRST(<iteration statement>), FIRST(<input statement>), FIRST(<output statement>) }
*					= { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE) }
*
*
*
*/
void statement(void) {
	switch (lookahead.code) {/*switch statement used beacause of multiple options*/
	case AVID_T:
	case SVID_T:
		/*if AVID_T and SVID_T*/
		assignment_statement();/*calls assignment statement function*/
		break;
	case KW_T:
		/*if KW_T*/
		if (lookahead.attribute.get_int == IF) {
			selection_statement();/*calls selection statement function*/
		}
		if (lookahead.attribute.get_int == WHILE) {
			iteration_statement();/*calls iteration statement function*/
		}
		if (lookahead.attribute.get_int == READ) {
			input_statement();/*calls input statement function*/
		}
		if (lookahead.attribute.get_int == WRITE) {
			output_statement();/*calls output statement function*/
		}
		break;
	default:
		syn_printe();/*error*/
		break;
	}
}

/*<statements'> -> <statement><statements'> | e
*					FIRST(<statements’>) = { AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ),
*					KW_T(WRITE) , e}
*/
void statements_prime(void){
	switch (lookahead.code) {/*switch statement used beacause of multiple options*/
	case AVID_T:
	case SVID_T:
		/*if AVID_T and SVID_T*/
		statement();/*calls statement function*/
		statements_prime();/*calls statement prime function*/
	case KW_T:
		/*if KW_T*/
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statement();/*calls statement function*/
			statements_prime();/*calls statement prime function*/
			break;
		}
		break;
	
	}
}

/*<assignment statement> -> 
*							<assignment expression>;
*					FIRST(<assignment statement>) = { FIRST(<assignment expression>) } 
*												 = { AVID_T, SVID_T }
*
*/
void assignment_statement(void) {
	assignment_expression();/*calls assignment expression function*/
	match(EOS_T, NO_ATTR);/*matching the ;*/
	gen_incode("PLATY: Assignment statement parsed");/*production is recognized*/
}

/*<selection statement> ->
*							IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
*							ELSE { <opt_statements> } ;
*						FIRST(<selection statement>) = { KW_T(IF) }
*
*/
void selection_statement(void) {
	match(KW_T, IF);/*match IF keyword*/
	pre_condition();/*call precondition function*/
	match(LPR_T, NO_ATTR);/*match left parethesis*/
	conditional_expression();/*call conditional expression function*/
	match(RPR_T, NO_ATTR);/*match right parenthesis*/
	match(KW_T, THEN);/*match THEN keyword*/
	match(LBR_T, NO_ATTR);/*match left bracket*/
	opt_statements();/*call optional statements function*/
	match(RBR_T, NO_ATTR);/*match right bracket*/
	match(KW_T, ELSE);/*match ELSE keyword*/
	match(LBR_T, NO_ATTR);/*match left bracket*/
	opt_statements();/*call optional statement function*/
	match(RBR_T, NO_ATTR);/*match right bracket*/
	match(EOS_T, NO_ATTR);/*match end of statement token*/
	gen_incode("PLATY: Selection statement parsed");/*production is recognized*/
}

/*<iteration statement> ->
*							WHILE <pre-condition> (<conditional expression>)
*							REPEAT { <statements>};
*					FIRST(<iteration statement>) = { KW_T(WHILE) }
*/
void iteration_statement(void) {
	match(KW_T, WHILE);/*match WHILE keyword*/
	pre_condition();/*call precondition function*/
	match(LPR_T, NO_ATTR);/*match left parenthesis*/
	conditional_expression();/*call conditional expression function*/
	match(RBR_T, NO_ATTR);/*match right bracket*/
	match(KW_T, REPEAT);/*match REPEAT keyword*/
	match(LBR_T, NO_ATTR);/*match left brace*/
	statements();/*call statements function*/
	match(RBR_T, NO_ATTR);/*match right bracket*/
	match(EOS_T, NO_ATTR);/*match end of statment token*/
	gen_incode("PLATY: Iteration statement parsed");/*production is recognized*/
}

/*<pre-condition> ->
*							TRUE | FALSE
*						FIRST(<pre-condition>) = { KW_T(TRUE), KW_T(FALSE) }
*/
void pre_condition(void) {
		if(lookahead.code == KW_T){/*if the token is a keyword*/
			if (lookahead.attribute.get_int == TRUE) {/*checks if attribute is TRUE*/
				match(KW_T, TRUE);/*matches TRUE keyword*/
			}
			else if (lookahead.attribute.get_int == FALSE) {
				/*if FALSE*/
				match(KW_T, FALSE);/*matches FALSE keyword*/
			}
			else {
				syn_printe();/*error*/
			}
		}
		else {
			syn_printe();/*error*/
		}
}

/*<conditional expression> ->
*							<logical OR  expression>
*						FIRST(<conditional expression>) = { FIRST(<logical OR expression>) }
*						= { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*
*/
void conditional_expression(void) {
	logical_or_expression();/*calls logical or expression*/
	gen_incode("PLATY: Conditional expression parsed");/*production is recognized*/
}

/* <logical OR expression> ->
*							<logical AND expression> <logical OR expression’>
*						FIRST(<logical OR expression>) = { FIRST(<logical AND expression>) }
*						= { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
*
*
*/
void logical_or_expression(void) {
	logical_and_expression();/*calls logical and expression function*/
	logical_or_expression_prime();/*calls logical or expression prime function*/
}

/*<logical OR expression’> -> 
*						.OR. <logical AND expression> <logical OR expression’> | e 
*						FIRST(<logical OR expression’>) = { .OR. , e }
*/
void logical_or_expression_prime(void) {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == OR) {
			match(LOG_OP_T, OR);/*match OR*/
			logical_and_expression();/*call logical AND expression function*/
			logical_or_expression_prime();/*call logical OR expression prime function*/
			gen_incode("PLATY: Logical OR expression parsed");/*production is recognized*/
	}
		
	
}

/*<logical AND expression> -> <relational expression><logical AND expression’>
*				FIRST(<logical AND expression>) = {FIRST(<relational expression>)}
*				= {FIRST(<primary a_relational expression>),FIRST(<primary s_relational expression>)}
*				= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_and_expression(void){
	relational_expression();/*call relational expression function*/
	logical_and_expression_prime();/*call logical and expression prime function*/
}

/*<logical AND expression’> -> .AND.<relational expression> <logical AND expression’> | e
*					FIRST(<logical AND expression'>) = {LOG_OP_T(AND), e}
*/
void logical_and_expression_prime() {
	if (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == AND) {
		match(LOG_OP_T, AND);/*match AND*/
		relational_expression();/*call relation expression function*/
		logical_and_expression_prime();/*call logical AND expression prime function*/
		gen_incode("PLATY: Logical AND expression parsed");/*production is recognized*/
	}
}

/*<relational expression> -> 
*						<primary a_relational expression> <relational operator> <primary a_relational expression> 
*					  | <primary s_relational expression> <relational operator> <primary s_relational expression>
*					FIRST(<relational expression>)
*					= {FIRST(<primary a_relational expression>),FIRST(<primary s_relational expression>)}
*					= {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void) {
	
	switch (lookahead.code) {/*switch statement used beacause of multiple options*/
		case AVID_T:
		case FPL_T:
		case INL_T:
			/*if AVID_T, FPL_T or INL_T*/
			primary_a_relational_expression();/*call a relational expression function*/
			relational_operator();/*call relation operator function*/
			primary_a_relational_expression();/*call a relational expression function*/
			break;
		case SVID_T:
		case STR_T:
			/*if SVID_T, STR_T*/
			primary_s_relational_expression();/*call s relational expression function*/
			relational_operator();/*call relation operator function*/
			primary_s_relational_expression();/*call s relational expression function*/
			break;
		default:
			syn_printe();/*error*/
			break;
	}
	gen_incode("PLATY: Relational expression parsed");/*production is recognized*/
}

/*<primary a_relational expression> ->
*							AVID_T
*						  | FPL_T
*						  | INL_T
*					FIRST(<primary a_relational expression>) = { AVID_T, FPL_T, INL_T }
*/
void primary_a_relational_expression(void) {
	switch (lookahead.code) {/*switch statement used beacause of multiple options*/
		case AVID_T:
		case FPL_T:
		case INL_T:
			/*if AVID_T, FPL_T or INL_T*/
			match(lookahead.code, NO_ATTR);
			break;
		default:
			syn_printe();/*error*/
			break;
	}
	gen_incode("PLATY: Primary a_relational expression parsed");/*production is recognized*/
}

/*<primary s_relational expression> -> <primary string expression>
*					FIRST(<primary s_relational_expression>) = { FIRST(<primary string expression> }
*					= { SVID_T, STR_T }
*
*/
void primary_s_relational_expression(void) {
	primary_string_expression();/*calls primary stirng expression*/
	gen_incode("PLATY: Primary s_relational expression parsed");/*production is recognized*/
}

/*<relational operator> -> == | <> | > | <
*					FIRST(<relational operator>) ={REL_OP_T(EQ),REL_OP_T(NE),REL_OP_T(GT),REL_OP_T(LT)}
*/
void relational_operator(void) {
	if (lookahead.code == REL_OP_T) {
		match(REL_OP_T, lookahead.attribute.log_op);/*matches REL_OP_T along with its attribute*/
	}
	else {
		syn_printe();/*error*/
	}
}

/*<input statement> ->
*							READ (<variable list>);
*						FIRST(<input statement>) = { KW_T(READ) }
*/
void input_statement(void) {
	match(KW_T, READ);/*match READ keyword*/
	match(LPR_T, NO_ATTR);/*match left parenthesis*/
	variable_list();/*call function*/
	match(RPR_T, NO_ATTR);/*match right parenthesis*/
	match(EOS_T, NO_ATTR);/*match end of statement*/
	gen_incode("PLATY: Input statement parsed");/*production is recognized*/
}

/*<variable list> ->
*							<variable identifier> <variable list’>
*					FIRST(<variable list>) = { FIRST(<variable identifier>) } = { AVID_T, SVID_T }
*/
void variable_list(void) {
	variable_identifier();/*calls variable identifier function*/
	variable_list_prime();/*calls variable list prime*/
	gen_incode("PLATY: Variable list parsed");/*production is recognized*/
}

/*<variable list’> -> 
*							, <variable identifier><variable list’> | e
*						FIRST(<variable list’>) = { COM_T, e } 
*/
void variable_list_prime(void) {
	if (lookahead.code == COM_T) {
		match(lookahead.code, NO_ATTR);/*matches comma*/
		variable_identifier();/*calls variable identifier function*/
		variable_list_prime();/*calls variable list prime*/
	}
}

/*<variable identifier> -> AVID_T | SVID_T
*					FIRST(<variable identifier>) = { AVID_T,SVID_T }
*/
void variable_identifier(void) {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
		/*if AVID_T or SVID_T*/
		match(lookahead.code, NO_ATTR);/*matches whichever one it is*/
	}
	else {
		syn_printe(); /*error*/
	}
}

/*<output statement> ->
* 						WRITE (<output list>); (introducing a new nonterminal (<output list>)
*						FIRST(<output statement>) = { KW_T(WRITE) }
*/
void output_statement(void) {
	match(KW_T, WRITE);/*match WRITE keyword*/
	match(LPR_T, NO_ATTR);/*match left parenthsis*/
	output_list();/*callion output list function*/
	match(RPR_T, NO_ATTR);/*match right parenthesis*/
	match(EOS_T, NO_ATTR);/*matches end of statement*/
	gen_incode("PLATY: Output statement parsed");/*production is recognized*/
}

/* <output_list> -> <opt_variable list> | STR_T;
*					FIRST (<output_list>) = { FIRST (<opt_variable list >) , STR_T}
*					= {AVID_T, SVID_T, STR_T, e }
*/
void output_list() {
	if (lookahead.code == STR_T) {
		/*if lookahead is STR_T*/
		match(STR_T, NO_ATTR);/*matches STR_T*/
		gen_incode("PLATY: Output list (string literal) parsed");/*production is recognized*/
	}
	else {
		optional_variable_list();/*calls optional variable list function*/
	}
}

/*<opt_variable list> -> <variable list> | e
*					FIRST (<opt_variable list >) = { AVID_T, SVID_T, e }
*/
void optional_variable_list() {
	if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
		/*if lookahead is AVID_T or SVID_T*/
		variable_list();/*calls variable list function*/
	}
	else {
		gen_incode("PLATY: Output list (empty) parsed");/*production is recognized*/
	}

}

/*< assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
*						FIRST(<assignment expression>) = { AVID_T, SVID_T }
*/
void assignment_expression(void) {
	if (lookahead.code == AVID_T) {
		/*if AVID_T*/
		match(AVID_T, NO_ATTR);/*matches AVID_T*/
		match(ASS_OP_T, NO_ATTR);/*matches =*/
		arithmetic_expression();/*calls arithmetic expression function*/
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");/*production is recognized*/
	}
	else if (lookahead.code == SVID_T) {
		/*if SVID_T*/
		match(SVID_T, NO_ATTR);/*matches SVID_T*/
		match(ASS_OP_T, NO_ATTR);/*matches =*/
		string_expression();/*calls stirng expression function*/
		gen_incode("PLATY: Assignment expression (string) parsed");/*production is recognized*/
	}
	else {
		syn_printe();/*error*/
	}
}

/*<arithmetic expression> - > <unary arithmetic expression>  | <additive arithmetic expression>
*					FIRST(<arithmetic expression>) = { FIRST(<unary arithmetic expression>,
*					FIRST(<additive arithmetic expression>) } = {-,+}
*/
void arithmetic_expression(void) {
	switch (lookahead.code) {
		case ART_OP_T:
			/*if ART_OP_T*/
			if (lookahead.attribute.arr_op == MINUS || lookahead.attribute.arr_op == PLUS) {
				/*if the arrOp is + or -*/
				unary_arithmetic_expression();/*calls unary arithmetic expression*/
			}
			else {
				syn_printe();/*error*/
				break;
			}
			gen_incode("PLATY: Arithmetic expression parsed");/*production is recognized*/
			break;
		case AVID_T:
		case FPL_T:
		case INL_T:
		case LPR_T:
			additive_arithmetic_expression();/*calls additive arithmetic expression function*/
			gen_incode("PLATY: Arithmetic expression parsed");/*production is recognized*/
			break;

		default:
			syn_printe();/*error*/
			break;
	}
	
}

/*<unary arithmetic expression> ->
*						-  <primary arithmetic expression> 
*					  | + <primary arithmetic expression>
*					FIRST(<unary arithmetic expression>) = { -, + }
*/	
void unary_arithmetic_expression(void) {
	if (lookahead.code == ART_OP_T) {
		if (lookahead.attribute.arr_op == MINUS || lookahead.attribute.arr_op == PLUS) {
			/*if the arrOp is + or -*/
			match(ART_OP_T, lookahead.attribute.arr_op);/*matches ART_OP_T*/
			primary_arithmetic_expression();/*calls primary arithmetic expression function*/
		}
		else {
			syn_printe();/*error*/
		}
		gen_incode("PLATY: Unary arithmetic expression parsed");/*production is recognized*/
	}

}

/*<additive arithmetic expression> ->
*							<multiplicative arithmetic expression> <additive arithmetic expression’>
*					FIRST(<additive arithmetic expression>) = {FIRST(<multiplicative arithmetic expression>)}
*					= { AVID_T, FPL_T, INL_T, ( }
*/
void additive_arithmetic_expression(void) {
	multiplicative_arithmetic_expression();/*calls multiplicative arithmetic expression function*/
	additive_arithmetic_expression_prime();/*calls additive arithmetic expression prime function*/
}

/*
* Purpose			: multiplicative arithmetic expression production function
* Author			: Federico Fonseca
* History/Versions	: 01 2020-08-12
* Called functions	: multiplicative_arithmetic_expression_prime(), primary_arithmetic_expression()
* Return value		: void
* Algotrithm		: <multiplicative arithmetic expression> ->
*						<primary arithmetic expression><multiplicative arithmetic expression’>
*					FIRST(<multiplicative arithmetic expression>)
*					= FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
*/
void multiplicative_arithmetic_expression(void) {
	primary_arithmetic_expression();/*calls primary arithmetic expression function*/
	multiplicative_arithmetic_expression_prime();/*calls multiplicative arithmetic expression prime function*/
}

/*<primary arithmetic expression> ->
*									  AVID_T
*									| FPL_T
*									| INL_T
*									| (<arithmetic expression>)
*						FIRST(<primary arithmetic expression>) = { AVID_T, FPL_T, INL_T, ( }
*/
void primary_arithmetic_expression(void) {
	switch (lookahead.code) {/*switch statement used beacause of multiple options*/
		case AVID_T:
		case FPL_T:
		case INL_T:
			/*if AVID_T, FPL_T or INL_T*/
			match(lookahead.code, NO_ATTR);/*matche whichever one it is*/
			gen_incode("PLATY: Primary arithmetic expression parsed");/*production is recognized*/
			break;
		case LPR_T:
			match(LPR_T, NO_ATTR);/*matches (*/
			arithmetic_expression();/*calls arithmetic expression function*/
			match(RPR_T, NO_ATTR);/*matches )*/
			gen_incode("PLATY: Primary arithmetic expression parsed");/*production is recognized*/
			break;
		default:
			syn_printe();/*error*/
			break;
	}
}

/*<multiplicative arithmetic expression’> ->
*						*<primary arithmetic expression><multiplicative arithmetic expression’>
*					   |/<primary arithmetic expression><multiplicative arithmetic expression’>
*					   | e
*					FIRST(<multiplicative arithmetic expression’>) = {ART_OP_T(MULT),ART_OP_T(DIV),e}
*/
void multiplicative_arithmetic_expression_prime() {
	if (lookahead.code == ART_OP_T) {
		/*if ART_OP_T*/
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV) {
			/*if arrOp is * or / */
			match(ART_OP_T, lookahead.attribute.arr_op);/*match ART_OP_T with its attribute*/
			primary_arithmetic_expression();//*calls primary arithmetic expression
			multiplicative_arithmetic_expression_prime();/*calls multiplicative arithmetic expression prime function*/
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");/*production is recognized*/
		}
		
	}
}

/*<additive arithmetic expression’> ->
*								+ <multiplicative arithmetic expression> <additive arithmetic expression’>
*							  | - <multiplicative arithmetic expression> <additive arithmetic expression’>
*							  | e
*						FIRST(<additive arithmetic expression’>) = { +, -, e }
*/
void additive_arithmetic_expression_prime(void) {
	if (lookahead.code == ART_OP_T) {
		/*if ART_OP_T*/
		if (lookahead.attribute.arr_op == MINUS || lookahead.attribute.arr_op == PLUS) {
			/*if arrOp is - or +*/
			match(ART_OP_T, lookahead.attribute.arr_op);/*match ART_OP_T with its attribute*/
			multiplicative_arithmetic_expression();/*calls multiplicative arithmetic expression function*/
			additive_arithmetic_expression_prime();/*calls additive arithmetic expression prime function*/
		}
		gen_incode("PLATY: Additive arithmetic expression parsed");/*production is recognized*/
	}
}

/* <string expression> -> <primary string expression><string expression’>
*					FIRST(<string expression>)	= FIRST(<primary string expression>) = { SVID_T, STR_T }
*/
void string_expression(void) {
	primary_string_expression();/*calls primary string expression function*/
	string_expression_prime();/*calls stirng expression prime function*/
	gen_incode("PLATY: String expression parsed");/*production is recognized*/
}

/*<primary string expression> ->
*									  SVID_T
*									| STR_T
*						FIRST(<primary string expression>) = { SVID_T, STR_T }
*/
void primary_string_expression(void) {
	if (lookahead.code == SVID_T || lookahead.code == STR_T) {
		/*if SVID_T or STR_T*/
		match(lookahead.code, NO_ATTR);/*matches whichever one it is*/
		gen_incode("PLATY: Primary string expression parsed");/*production is recognized*/
	}
	else {
		syn_printe();/*error*/
	}
}

/*<string expression’> -> ## <primary string expression><string expression’>| e
*					FIRST(<string expression'>) = {SCC_OP_T,e}
*/
void string_expression_prime(void) {
	if (lookahead.code == SCC_OP_T) {
		/*if SCC_OP_T*/
		match(SCC_OP_T, NO_ATTR);/*matches SCC_OP_T*/
		primary_string_expression();/*primary string expression function*/
		string_expression_prime();/*string expression prime function*/
	}
}

