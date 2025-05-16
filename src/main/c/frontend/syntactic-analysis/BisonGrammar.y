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
    Bool boolean;

	/** Non-terminals. */

	Constant * constant;
	Factor * factor;
	Program * program;
	MatchStatement * matchStatement;
    Case * matchCase;
    CaseList * caseList;
    Statement * statement;
    StatementList * statementList;
    Expression * expression;
    AssignmentStatement * assignmentStatement;
    ForLoop * forLoop;
    WhileLoop * whileLoop;
    IfStatement * ifStatement;
    PrintStatement * printStatement;
    SortStatement * sortStatement;
    MacroStatement * macroStatement;
    StringList * stringList;
    ReturnStatement * returnStatement;
    FunctionStatement * functionStatement;
    FunctionDefinition * functionDefinition;
    Unit * unit;
    ExternalDeclaration * externalDeclaration;
    ElseStatement * else_statement;
    UnaryChangeOperatorStatement * unaryChangeOperatorStatement;
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
%token <boolean> TRUE
%token <boolean> FALSE

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
%type <assignmentStatement> assignmentStatement
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
%type <printStatement> print_statement
%type <sortStatement> sort_statement
%type <expression> expression
%type <macroStatement> macro_statement
%type <stringList> stringList
%type <returnStatement> returnStatement
%type <functionStatement> functionStatement
%type <functionDefinition> functionDefinition
%type <unit> unit
%type <externalDeclaration> externalDeclaration
%type <else_statement> else_statement
%type <unaryChangeOperatorStatement> unaryChangeOperatorStatement
/**
 * Precedence and associativity.
 *
 * @see https://www.gnu.org/software/bison/manual/html_node/Precedence.html
 */
%left OR                  // ||
%left AND                 // &&
%nonassoc EQ NEQ LT LTE GT GTE  // ==, !=, <, <=, >, >=
%left ADD SUB             // +, -
%left MUL DIV             // *, /
%right NOT                // ! (prefijo)


%%

// IMPORTANT: To use λ in the following grammar, use the %empty symbol.

program: unit                                                       { $$ = ProgramSemanticAction(currentCompilerState(), $1); }
    | %empty                                                        { $$ = EmptyProgramSemanticAction(currentCompilerState()); }
    ;
unit:
      externalDeclaration                                           { $$ = SingleExternalDeclarationSemanticAction($1); }
    | unit externalDeclaration                                      { $$ = AppendExternalDeclarationSemanticAction($1, $2); }
    ;
externalDeclaration:
      functionDefinition                                            { $$ = FunctionDefinitionExternalDeclarationSemanticAction($1); }
    | statement                                                     { $$ = StatementExternalDeclarationSemanticAction($1); }
    ;

 //statement                                                        { $$ = SingleStatementListSemanticAction($1); }

statementList: statementList statement                              { $$ = AppendStatementListSemanticAction($1, $2); }
    | statement                                                        { $$ = SingleStatementListSemanticAction($1); }
	;


functionDefinition: INT IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS OPEN_BRACE statementList CLOSE_BRACE  { $$ = FunctionDefinitionSemanticAction($2, $4, $7); }
    ;

statement:
    for_loop                                                        { $$ = ForLoopStatementSemanticAction($1); }
  | matchStatement                                                  { $$ = MatchStatementSemanticAction($1); }
  | while_loop                                                      { $$ = WhileLoopStatementSemanticAction($1); }
  | if_statement                                                    { $$ = IfStatementSemanticAction($1); }
  | print_statement                                                 { $$ = PrintStatementSemanticAction($1);}
  | sort_statement                                                  { $$ = SortStatementSemanticAction($1);}
  | assignmentStatement                                             { $$ = AssignmentStatementSemanticAction($1);}
  | macro_statement                                                 { $$ = MacroStatementSemanticAction($1); }
  | returnStatement                                               { $$ = ReturnStatementSemanticAction($1); }
  | functionStatement                                              { $$ = FunctionStatementSemanticAction($1); }
  | unaryChangeOperatorStatement                                    { $$ = UnaryChangeOperatorStatementSemanticAction($1); }
  ;
unaryChangeOperatorStatement:
    IDENTIFIER ADD_ONE                                          { $$ = UnaryChangeOperatorSemanticAction($1, POST_INCREMENT); }
    | IDENTIFIER MINUS_ONE                                         { $$ = UnaryChangeOperatorSemanticAction($1, POST_DECREMENT); }
    | ADD_ONE IDENTIFIER                                          { $$ = UnaryChangeOperatorSemanticAction($2, PRE_INCREMENT); }
    | MINUS_ONE IDENTIFIER                                         { $$ = UnaryChangeOperatorSemanticAction($2, PRE_DECREMENT); }


returnStatement: RETURN expression                                   { $$ = ReturnSemanticAction($2); }
    | RETURN functionStatement                       { $$ = ReturnFunctionStatementSemanticAction($2); }
    ;

functionStatement: IDENTIFIER OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS
                                                                        { $$ = FunctionSemanticAction($1, $3); }



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

for_loop: FOR assignmentStatement TO constant OPEN_BRACE statementList CLOSE_BRACE
                                                                    { $$ = ForLoopSemanticAction($2, $4, $6); }
	;

while_loop:
    WHILE expression OPEN_BRACE statementList CLOSE_BRACE
                                                                    { $$ = WhileLoopSemanticAction($2, $4); }
    ;

if_statement: IF expression OPEN_BRACE statementList CLOSE_BRACE else_statement  { $$ = IfThenSemanticAction($2, $4,$6); }
  ;

else_statement:
    ELSE OPEN_BRACE statementList CLOSE_BRACE                      { $$ = ElseStatementSemanticAction($3); }
  | ELSE if_statement                                             { $$ = ElseIfStatementSemanticAction($2); }
  | %empty                                                        { $$ = NULL; }
  ;

factor:
    OPEN_PARENTHESIS expression CLOSE_PARENTHESIS                { $$ = ParenthesisFactorSemanticAction($2); }
	|  constant														{ $$ = ConstantFactorSemanticAction($1); }
	| IDENTIFIER                                                    { $$ = IdentifierFactorSemanticAction($1);}
	| TRUE                                                        { $$ = BooleanFactorSemanticAction(TRUE); }
	| FALSE                                                       { $$ = BooleanFactorSemanticAction(FALSE); }
	;

constant: INTEGER													{ $$ = IntegerConstantSemanticAction($1); }
	;

expression:
    expression[left] ADD expression[right]		                    { $$ = ArithmeticExpressionSemanticAction($left, $right, ADDITION); }
    | expression[left] DIV expression[right]				        { $$ = ArithmeticExpressionSemanticAction($left, $right, DIVISION); }
    | expression[left] MUL expression[right]				        { $$ = ArithmeticExpressionSemanticAction($left, $right, MULTIPLICATION); }
    | expression[left] SUB expression[right]			            { $$ = ArithmeticExpressionSemanticAction($left, $right, SUBTRACTION); }
    | factor												        { $$ = FactorExpressionSemanticAction($1); }
    | expression EQ expression                                      { $$ = BooleanSemanticAction($1, $3, EQUAL); }
    | expression NEQ expression                                     { $$ = BooleanSemanticAction($1, $3, NOT_EQUAL); }
    | expression LT expression                                      { $$ = BooleanSemanticAction($1, $3, LESS_THAN); }
    | expression LTE expression                                     { $$ = BooleanSemanticAction($1, $3, LESS_EQUAL); }
    | expression GT expression                                      { $$ = BooleanSemanticAction($1, $3, GREATER_THAN); }
    | expression GTE expression                                     { $$ = BooleanSemanticAction($1, $3, GREATER_EQUAL); }
    | expression AND expression                                     { $$ = ConditionalExpressionSemanticAction($1, $3, LOGICAL_AND); }
    | expression OR expression                                      { $$ = ConditionalExpressionSemanticAction($1, $3, LOGICAL_OR); }
    | NOT expression                                                { $$ = NotExpressionSemanticAction($2); }
    | STRING                                                        { $$ = StringExpressionSemanticAction($1); }
    ;

assignmentStatement: IDENTIFIER ASSIGNMENT expression               { $$ = AssignmentExpressionSemanticAction($1,$3);}
     ;


print_statement: PRINT IDENTIFIER                                   { $$ = PrintIdentifierSemanticAction($2); }
    |    PRINT STRING_START STRING STRING_END                       { $$ = PrintStringSemanticAction($3); }
    ;
stringList:
    IDENTIFIER                                       { $$ = SingleStringListSemanticAction($1); }
  | stringList COMMA IDENTIFIER                            { $$ = AppendStringListSemanticAction($1, $3); }
  | %empty                                                        { $$ = NULL; }
  ;

%%
