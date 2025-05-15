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
	MathExpression * mathExpression;
	Factor * factor;
	Program * program;
	MatchStatement * matchStatement;
    Case * matchCase;
    CaseList * caseList;
    Statement * statement;
    StatementList * statementList;
    AssignmentMathExpression * assignmentMathExpression;
    ForLoop * forLoop;
    WhileLoop * whileLoop;
    IfStatement * ifStatement;
    ConditionalExpression * conditionalExpression;
    PrintStatement * printStatement;


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
%destructor { releaseExpression($$); } <mathExpression>
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
%token <string> IDENTIFIER
%token <token> OPEN_BRACKETS
%token <token> CLOSE_BRACKETS
%token <token> STRING_START
%token <token> STRING_END

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

%token <string> STRING

%token <token> MATCH
%token <token> ASSIGNMENT
%token <token> FOR
%token <token> WHILE
%token <token> TO
%token <token> IF
%token <token> ELSE
%token <token> DEFAULT
%token <token> PRINT
%token <token> MACRO
%token <token> SORT


%token <token> UNKNOWN
%token <token>  ARROW  RETURN


/** Non-terminals. */

%type <constant> constant
%type <mathExpression> mathExpression
%type <assignmentMathExpression> assignmentMathExpression
%type <factor> factor
%type <program> program
%type <matchStatement> matchStatement
%type <matchCase> matchCase
%type <caseList> matchCaseList
%type <statement> statement
%type <statementList> statementList
%type <forLoop> for_loop
%type <whileLoop> while_loop
%type <ifStatement> if_statement
%type <conditionalExpression> conditionalExpression
%type <printStatement> print_statement


/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 */
%left ADD SUB
%left MUL DIV

%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: statementList                                             { $$ = StatementListProgramSemanticAction(currentCompilerState(), $1); }
    ;

statementList: statement                                           { $$ = SingleStatementListSemanticAction($1); }
	| statementList statement                                      { $$ = AppendStatementListSemanticAction($1, $2); }
	;

statement: mathExpression                                               { $$ = ExpressionStatementSemanticAction($1); }
  | for_loop                                                        { $$ = ForLoopStatementSemanticAction($1); }
  | matchStatement                                                 { $$ = MatchStatementSemanticAction($1); }
  | while_loop                                                      { $$ = WhileLoopStatementSemanticAction($1); }
  | if_statement                                                    { $$ = IfStatementSemanticAction($1); }
  | print_statement                                                 { $$ = PrintStatementSemanticAction($1);}
  ;

matchStatement:
    MATCH IDENTIFIER INDENT matchCaseList DEDENT                  { $$ = MatchSemanticAction($2, $4); }
   ;

matchCaseList: matchCase                                         { $$ = SingleCaseListSemanticAction($1); }
  | matchCaseList matchCase                                      { $$ = AppendCaseListSemanticAction($1, $2); }
  ;

matchCase:
    INTEGER ARROW INDENT statementList DEDENT                      { $$ = MatchCaseSemanticAction($1, $4); }
    ;

for_loop:
    FOR assignmentMathExpression TO constant INDENT statementList DEDENT
                                                                        {$$ = ForLoopSemanticAction($2, $4, $6); }
	;

while_loop:
    WHILE mathExpression INDENT statementList DEDENT                   { $$ = WhileLoopSemanticAction($2, $4); }
    ;

if_statement: IF mathExpression INDENT statementList DEDENT            { $$ = IfThenSemanticAction($2, $4); }
  | IF mathExpression INDENT statementList DEDENT
    ELSE INDENT statementList DEDENT                               { $$ = IfElseSemanticAction($2, $4, $8); }
  ;

mathExpression: mathExpression[left] ADD mathExpression[right]					{ $$ = ArithmeticExpressionSemanticAction($left, $right, ADDITION); }
	| mathExpression[left] DIV mathExpression[right]						{ $$ = ArithmeticExpressionSemanticAction($left, $right, DIVISION); }
	| mathExpression[left] MUL mathExpression[right]						{ $$ = ArithmeticExpressionSemanticAction($left, $right, MULTIPLICATION); }
	| mathExpression[left] SUB mathExpression[right]						{ $$ = ArithmeticExpressionSemanticAction($left, $right, SUBTRACTION); }
	| factor														{ $$ = FactorExpressionSemanticAction($1); }
	;

factor: OPEN_PARENTHESIS mathExpression CLOSE_PARENTHESIS				{ $$ = ExpressionFactorSemanticAction($2); }
	| constant														{ $$ = ConstantFactorSemanticAction($1); }
	;

constant: INTEGER													{ $$ = IntegerConstantSemanticAction($1); }
	;

assignmentMathExpression: IDENTIFIER ASSIGNMENT mathExpression			{ $$ = assignmentMathExpressionSemanticAction($1, $3); }
    ;
conditionalExpression: IDENTIFIER                      { $$ =   conditionalExpressionSemanticAction($1); } /* FIXME */
    ;
print_statement: PRINT IDENTIFIER                                    { $$ = PrintIdentifierSemanticAction( $2);}
|    PRINT STRING_START STRING STRING_END                            { $$ = PrintStringSemanticAction($3); }       ;


%%
