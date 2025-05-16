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
    BoolExpression * boolExpression;
    PrintStatement * printStatement;
    SortStatement * sortStatement;
    MacroStatement * macroStatement;
    StringList * stringList;
    ReturnStatement * returnStatement;
    FunctionStatement * functionStatement;
    FunctionDefinition * functionDefinition;
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
%token <token> IGNORE

%token <token> ADD_ONE
%token <token> MINUS_ONE

%token <token> GTE
%token <token> LTE
%token <token> EQ
%token <token> NEQ
%token <token> LT
%token <token> GT

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
%token <token> OPEN_BRACE
%token <token> CLOSE_BRACE
%token <token> COMMA

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
%type <boolExpression> boolExpression
%type <printStatement> print_statement
%type <sortStatement> sort_statement
%type <macroStatement> macro_statement
%type <stringList> stringList
%type <returnStatement> returnStatement
%type <functionStatement> functionStatement
%type <functionDefinition> functionDefinition
/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 */
%left ADD SUB
%left MUL DIV

%left OR AND
%left NOT

%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: statementList                                              { $$ = StatementListProgramSemanticAction(currentCompilerState(), $1); }
    | functionDefinition                      { $$ = FunctionDefinitionProgramSemanticAction($1); }
    ;

 //statement                                                        { $$ = SingleStatementListSemanticAction($1); }

statementList: statementList statement                              { $$ = AppendStatementListSemanticAction($1, $2); }
    | %empty                                                        { $$ = NULL; }
	;


functionDefinition: INT IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS OPEN_BRACE statementList CLOSE_BRACE  { $$ = FunctionDefinitionSemanticAction($2, $4, $7); }
    | VOID IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS OPEN_BRACE statementList CLOSE_BRACE  { $$ = FunctionDefinitionSemanticAction($2, $4, $7); }
    | STRING IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS OPEN_BRACE statementList CLOSE_BRACE  { $$ = FunctionDefinitionSemanticAction($2, $4, $7); }
    | BOOL IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS OPEN_BRACE statementList CLOSE_BRACE  { $$ = FunctionDefinitionSemanticAction($2, $4, $7); }
    ;

statement:
    for_loop                                                        { $$ = ForLoopStatementSemanticAction($1); }
  | matchStatement                                                  { $$ = MatchStatementSemanticAction($1); }
  | while_loop                                                      { $$ = WhileLoopStatementSemanticAction($1); }
  | if_statement                                                    { $$ = IfStatementSemanticAction($1); }
  | print_statement                                                 { $$ = PrintStatementSemanticAction($1);}
  | sort_statement                                                  { $$ = SortStatementSemanticAction($1);}
   | macro_statement                                                 { $$ = MacroStatementSemanticAction($1); }
   | returnStatement                                               { $$ = ReturnStatementSemanticAction($1); }
    | functionStatement                                              { $$ = FunctionStatementSemanticAction($1); }
  ;



returnStatement: RETURN expression                                   { $$ = ReturnStatementSemanticAction($2); }
    | RETURN functionStatement                       { $$ = ReturnStatementSemanticAction($2); }
    ;

functionStatement: IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS
                                                                        { $$ = FunctionSemanticAction($1, $2); }



macro_statement: MACRO IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS ARROW statement
                                                                        { $$ = MacroSemanticAction($2, $4, $7); }
 ;


sort_statement: SORT IDENTIFIER                                     { $$ = SortSemanticAction($2); }

matchStatement: MATCH IDENTIFIER OPEN_BRACE matchCaseList CLOSE_BRACE
                                                                    { $$ = MatchSemanticAction($2, $4); }
   ;

matchCaseList: matchCase                                            { $$ = SingleCaseListSemanticAction($1); }
  | matchCaseList matchCase                                         { $$ = AppendCaseListSemanticAction($1, $2); }
  ;

matchCase: INTEGER ARROW OPEN_BRACE statementList CLOSE_BRACE       { $$ = MatchCaseSemanticAction($1, $4); }
    ;

for_loop: FOR assignmentMathExpression TO constant OPEN_BRACE statementList CLOSE_BRACE
                                                                    { $$ = ForLoopSemanticAction($2, $4, $6); }
	;

while_loop:
    WHILE conditionalExpression OPEN_BRACE statementList CLOSE_BRACE
                                                                    { $$ = WhileLoopSemanticAction($2, $4); }
    ;

if_statement: IF conditionalExpression OPEN_BRACE statementList CLOSE_BRACE
                                                                    { $$ = IfThenSemanticAction($2, $4); }//FIXME
 //ELSE: { $$ = IfElseSemanticAction($2, $4, $8); }
  ;

mathExpression:
      mathExpression[left] ADD mathExpression[right]		        { $$ = ArithmeticExpressionSemanticAction($left, $right, ADDITION); }
	| mathExpression[left] DIV mathExpression[right]				{ $$ = ArithmeticExpressionSemanticAction($left, $right, DIVISION); }
	| mathExpression[left] MUL mathExpression[right]				{ $$ = ArithmeticExpressionSemanticAction($left, $right, MULTIPLICATION); }
	| mathExpression[left] SUB mathExpression[right]			    { $$ = ArithmeticExpressionSemanticAction($left, $right, SUBTRACTION); }
	| OPEN_PARENTHESIS mathExpression[left] ADD mathExpression[right] CLOSE_PARENTHESIS	        { $$ = ArithmeticExpressionSemanticAction($left, $right, ADDITION); }
    | OPEN_PARENTHESIS mathExpression[left] DIV mathExpression[right] CLOSE_PARENTHESIS				{ $$ = ArithmeticExpressionSemanticAction($left, $right, DIVISION); }
    | OPEN_PARENTHESIS mathExpression[left] MUL mathExpression[right] CLOSE_PARENTHESIS				{ $$ = ArithmeticExpressionSemanticAction($left, $right, MULTIPLICATION); }
    | OPEN_PARENTHESIS mathExpression[left] SUB mathExpression[right] CLOSE_PARENTHESIS			    { $$ = ArithmeticExpressionSemanticAction($left, $right, SUBTRACTION); }
	| factor														{ $$ = FactorExpressionSemanticAction($1); }
	;

factor:
	  constant														{ $$ = ConstantFactorSemanticAction($1); }
	| IDENTIFIER                                                    { $$ = IdentifierFactorSemanticAction($1);}
	;

constant: INTEGER													{ $$ = IntegerConstantSemanticAction($1); }
	;

assignmentMathExpression: IDENTIFIER ASSIGNMENT mathExpression		{ $$ = assignmentMathExpressionSemanticAction($1, $3); }
    ;

conditionalExpression: boolExpression                               { $$ = BooleanExpressionSemanticAction($1);  }
    | conditionalExpression AND conditionalExpression               { $$ = ConditionalExpressionSemanticAction($1, $3, LOGICAL_AND); }
    | conditionalExpression OR conditionalExpression                { $$ = ConditionalExpressionSemanticAction($1, $3, LOGICAL_OR); }
    | OPEN_PARENTHESIS conditionalExpression AND conditionalExpression CLOSE_PARENTHESIS            { $$ = ConditionalExpressionSemanticAction($2, $4, LOGICAL_AND); }
    | OPEN_PARENTHESIS conditionalExpression OR conditionalExpression CLOSE_PARENTHESIS             { $$ = ConditionalExpressionSemanticAction($2, $4, LOGICAL_OR); }
    | NOT conditionalExpression                                     { $$ = NotExpressionSemanticAction($2); }
    ;

boolExpression: mathExpression EQ mathExpression                    { $$ = BooleanSemanticAction($1, $3, EQUAL); }
    | mathExpression NEQ mathExpression                             { $$ = BooleanSemanticAction($1, $3, NOT_EQUAL); }
    | mathExpression LT mathExpression                              { $$ = BooleanSemanticAction($1, $3, LESS_THAN); }
    | mathExpression LTE mathExpression                             { $$ = BooleanSemanticAction($1, $3, LESS_EQUAL); }
    | mathExpression GT mathExpression                              { $$ = BooleanSemanticAction($1, $3, GREATER_THAN); }
    | mathExpression GTE mathExpression                             { $$ = BooleanSemanticAction($1, $3, GREATER_EQUAL); }
    | OPEN_PARENTHESIS mathExpression EQ mathExpression CLOSE_PARENTHESIS    { $$ = BooleanSemanticAction($2, $4, EQUAL); }
    | OPEN_PARENTHESIS mathExpression NEQ mathExpression CLOSE_PARENTHESIS   { $$ = BooleanSemanticAction($2, $4, NOT_EQUAL); }
    | OPEN_PARENTHESIS mathExpression LT mathExpression CLOSE_PARENTHESIS    { $$ = BooleanSemanticAction($2, $4, LESS_THAN); }
    | OPEN_PARENTHESIS mathExpression LTE mathExpression CLOSE_PARENTHESIS   { $$ = BooleanSemanticAction($2, $4, LESS_EQUAL); }
    | OPEN_PARENTHESIS mathExpression GT mathExpression CLOSE_PARENTHESIS    { $$ = BooleanSemanticAction($2, $4, GREATER_THAN); }
    | OPEN_PARENTHESIS mathExpression GTE mathExpression CLOSE_PARENTHESIS   { $$ = BooleanSemanticAction($2, $4, GREATER_EQUAL); }
    | IDENTIFIER                                                    { $$ = IdentifierBooleanSemanticAction($1); }
    ;

print_statement: PRINT IDENTIFIER                                   { $$ = PrintIdentifierSemanticAction($2); }
    |    PRINT STRING_START STRING STRING_END                       { $$ = PrintStringSemanticAction($3); }
    ;
stringList:
    IDENTIFIER                                       { $$ = SingleStringListSemanticAction($1); }
  | stringList COMMA IDENTIFIER                            { $$ = AppendStringListSemanticAction($1, $3); }
  ;

%%
