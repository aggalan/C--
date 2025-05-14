%{

#include "BisonActions.h"

%}

// You touch this, and you die.
%define api.value.union.name SemanticValue

%union {
	/** Terminals. */

	int integer;
	String string;
	Token token;

	/** Non-terminals. */

	Constant * constant;
	Expression * expression;
	Factor * factor;
	Program * program;
	MatchStatement * matchStatement;
    Case * matchCase;
    CaseList * caseList;
    Statement * statement;
    StatementList * statementList;
    ForLoop * forLoop;
    WhileLoop * whileLoop;
    IfStatement * ifStatement;

}

/**
 * Destructors. This functions are executed after the parsing ends, so if the
 * AST must be used in the following phases of the compiler you shouldn't used
 * this approach. To use this mechanism, the AST must be translated into
 * another structure.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Destructor-Decl.html
 */
/*
%destructor { releaseConstant($$); } <constant>
%destructor { releaseExpression($$); } <expression>
%destructor { releaseFactor($$); } <factor>
%destructor { releaseProgram($$); } <program>
*/

/** Terminals. */
%token <integer> INTEGER
%token <token> ADD
%token <token> CLOSE_PARENTHESIS
%token <token> DIV
%token <token> MUL
%token <token> OPEN_PARENTHESIS
%token <token> SUB
%token <token> INT
%token <token> STRING_TYPE
%token <token> BOOL
%token <token> VOID
%token <String> IDENTIFIER
%token <token> OPEN_BRACKETS
%token <token> CLOSE_BRACKETS

%token <token> ADD_ONE
%token <token> MINUS_ONE

%token <token> GREATER_OR_EQUAL
%token <token> SMALLER_OR_EQUAL
%token <token> CONDITIONAL_EQUAL
%token <token> DIFFERENT
%token <token> SMALLER
%token <token> GREATER

%token <token> AND
%token <token> OR
%token <token> NOT

%token <token> INDENT
%token <token> DEDENT

%token <String> STRING

%token <token> MATCH
%token <token> ASSIGNMENT
%token <token> FOR
%token <token> WHILE
%token <token> TO
%token <token> IF
%token <token> ELSE
%token <token> TO
%token <token> DEFAULT

%token <token> PRINT
%token <token> MACRO
%token <token> SORT


%token <token> UNKNOWN
%token <token>  ARROW  RETURN


/** Non-terminals. */

%type <constant> constant
%type <expression> expression
%type <factor> factor
%type <program> program
%type <matchStatement> match_statement
%type <matchCase> match_case
%type <caseList> match_case_list
%type <statement> statement
%type <statementList> statement_list
%type <forLoop> for_loop
%type <whileLoop> while_loop
%type <ifStatement> if_statement


/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 */
%left ADD SUB
%left MUL DIV

%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: statement_list                                             { $$ = StatementListProgramSemanticAction(currentCompilerState(), $1); }
    ;

statement_list: statement                                           { $$ = SingleStatementListSemanticAction($1); }
	| statement_list statement                                      { $$ = AppendStatementListSemanticAction($1, $2); }
	;

statement: expression                                               { $$ = ExpressionStatementSemanticAction($1); }
  | for_loop                                                        { $$ = ForLoopStatementSemanticAction($1); }
  | match_statement                                                 { $$ = MatchStatementSemanticAction($1); }
  | while_loop                                                      { $$ = WhileLoopStatementSemanticAction($1); }
  | if_statement                                                    { $$ = IfStatementSemanticAction($1); }
  ;

match_statement:
    MATCH IDENTIFIER INDENT match_case_list DEDENT                  { $$ = MatchStatementSemanticAction($2, $4); }
   ;

match_case_list: match_case                                         { $$ = SingleCaseListSemanticAction($1); }
  | match_case_list match_case                                      { $$ = AppendCaseListSemanticAction($1, $2); }
  ;

match_case:
    INTEGER ARROW INDENT statement_list DEDENT                      { $$ = MatchCaseSemanticAction($1, $4); }
    ;

for_loop:
    FOR IDENTIFIER ASSIGNMENT expression TO expression INDENT statement_list DEDENT
                                                                    { $$ = ForLoopSemanticAction($2, $4, $6, $8); }
	;

while_loop:
    WHILE expression INDENT statement_list DEDENT                   { $$ = WhileLoopSemanticAction($2, $4); }
    ;

if_statement: IF expression INDENT statement_list DEDENT            { $$ = IfThenSemanticAction($2, $4); }
  | IF expression INDENT statement_list DEDENT
    ELSE INDENT statement_list DEDENT                               { $$ = IfElseSemanticAction($2, $4, $8); }
  ;

expression: expression[left] ADD expression[right]					{ $$ = ArithmeticExpressionSemanticAction($left, $right, ADDITION); }
	| expression[left] DIV expression[right]						{ $$ = ArithmeticExpressionSemanticAction($left, $right, DIVISION); }
	| expression[left] MUL expression[right]						{ $$ = ArithmeticExpressionSemanticAction($left, $right, MULTIPLICATION); }
	| expression[left] SUB expression[right]						{ $$ = ArithmeticExpressionSemanticAction($left, $right, SUBTRACTION); }
	| factor														{ $$ = FactorExpressionSemanticAction($1); }
	;

factor: OPEN_PARENTHESIS expression CLOSE_PARENTHESIS				{ $$ = ExpressionFactorSemanticAction($2); }
	| constant														{ $$ = ConstantFactorSemanticAction($1); }
	;

constant: INTEGER													{ $$ = IntegerConstantSemanticAction($1); }
	;

%%
