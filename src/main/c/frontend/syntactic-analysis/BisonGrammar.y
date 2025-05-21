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
    ElseStatement * elseStatement;
    VariableStatement * variableStatement;
    UnaryChangeOperatorStatement * unaryChangeOperatorStatement;
    ArrayStatement * arrayStatement;
    IntList * integerList;
    StatementBlock * statementBlock;
    ArrayAccess * arrayAccess;
    AssignmentMathStatement * assignmentMathStatement;
    AssignmentBoolStatement * assignmentBoolStatement;
    AssignmentStringStatement * assignmentStringStatement;
    ArgumentList * argumentList;
    ArgumentDefList * argumentDefList;
    ArgumentDef * argumentDef;
    ArgumentValue * argumentValue;
    StringExpression * stringExpression;
    BoolFactor * boolFactor;
    BoolExpression * boolExpression;
    MathExpression * mathExpression;
    MacroInvocationStatement * macroInvocationStatement;
    ArrayAssignment * arrayAssignment;
    BoolList * boolList;

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
%destructor { releaseMatchStatement($$); } <matchStatement>
%destructor { releaseCase($$); } <matchCase>
%destructor { releaseCaseList($$); } <caseList>
%destructor { releaseStatement($$); } <statement>
%destructor { releaseStatementList($$); } <statementList>
%destructor { releaseExpression($$); } <expression>
%destructor { releaseStatement($$); } <assignmentStatement>
%destructor { releaseForLoop($$); } <forLoop>
%destructor { releaseWhileLoop($$); } <whileLoop>
%destructor { releaseIfStatement($$); } <ifStatement>
%destructor { releasePrintStatement($$); } <printStatement>
%destructor { releaseSortStatement($$); } <sortStatement>
%destructor { releaseMacroStatement($$); } <macroStatement>
%destructor { releaseStringList($$); } <stringList>
%destructor { releaseReturnStatement($$); } <returnStatement>
%destructor { releaseFunctionStatement($$); } <functionStatement>
%destructor { releaseFunctionDefinition($$); } <functionDefinition>
%destructor { releaseUnit($$); } <unit>
%destructor { releaseExternalDeclaration($$); } <externalDeclaration>
%destructor { releaseElseStatement($$); } <elseStatement>
%destructor { releaseVariableStatement($$); } <variableStatement>
%destructor { releaseUnaryChangeOperatorStatement($$); } <unaryChangeOperatorStatement>
%destructor { releaseArrayStatement($$); } <arrayStatement>
%destructor { releaseIntList($$); } <integerList>
%destructor { releaseStatementBlock($$); } <statementBlock>
%destructor { releaseArrayAccess($$); } <arrayAccess>
%destructor { releaseAssignmentMathStatement($$); } <assignmentMathStatement>
%destructor { releaseAssignmentBoolStatement($$); } <assignmentBoolStatement>
%destructor { releaseAssignmentStringStatement($$); } <assignmentStringStatement>
%destructor { releaseBoolFactor($$); } <boolFactor>
%destructor { releaseBoolExpression($$); } <boolExpression>
%destructor { releaseMathExpression($$); } <mathExpression>

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
%token <string> GENERIC_ID
%token <string> INT_ID
%token <string> BOOL_ID
%token <string> STRING_ID
%token <string> BOOL_FUNCTION_ID
%token <string> INT_FUNCTION_ID
%token <string> STRING_FUNCTION_ID
%token <string> BOOL_ARRAY_ID
%token <string> INT_ARRAY_ID
%token <string> STRING_ARRAY_ID
%token <string> MACRO_ID
%token <token> OPEN_BRACKETS
%token <token> CLOSE_BRACKETS
%token <token> IGNORE
%token <token> NEW_LINE
%token <boolean> TRUE
%token <boolean> FALSE
%token <token> ADD_ONE
%token <token> MINUS_ONE
%token <token> ASC
%token <token> DESC


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
%token <token>  ARROW
%token <token> RETURN


/** Non-terminals. */

%type <constant> constant
%type <assignmentStatement> assignmentStatement
%type <factor> factor
%type <boolFactor> boolFactor
%type <arrayAccess> intArrayAccess
%type <arrayAccess> boolArrayAccess
%type <arrayAccess> stringArrayAccess
%type <program> program
%type <matchStatement> matchStatement
%type <matchCase> matchCase
%type <caseList> matchCaseList
%type <statement> statement
%type <statementList> statementList
%type <forLoop> forLoop
%type <whileLoop> whileLoop
%type <ifStatement> ifStatement
%type <printStatement> printStatement
%type <sortStatement> sortStatement
%type <expression> expression
%type <boolExpression> boolExpression
%type <mathExpression> mathExpression
%type <stringExpression> stringExpression
%type <assignmentMathStatement> assignmentMathStatement
%type <assignmentBoolStatement> assignmentBoolStatement
%type <assignmentStringStatement> assignmentStringStatement
%type <macroStatement> macroStatement
%type <stringList> stringList
%type <returnStatement> returnStatement
%type <functionStatement> intFunctionStatement
%type <functionStatement> boolFunctionStatement
%type <functionStatement> stringFunctionStatement
%type <functionStatement> functionStatement
%type <functionDefinition> functionDefinition
%type <functionStatement> boolFunctionExpression
%type <functionStatement> intFunctionExpression
%type <functionStatement> stringFunctionExpression
%type <argumentList> argumentList
%type <argumentDefList> argumentDefList
%type <argumentDef> argumentDef
%type <argumentValue> argumentValue
%type <unit> unit
%type <externalDeclaration> externalDeclaration
%type <elseStatement> elseStatement
%type <variableStatement> variableStatement
%type <unaryChangeOperatorStatement> unaryChangeOperatorStatement
%type <unaryChangeOperatorStatement> unaryIncrementOperatorExpression
%type <integerList> integerList
%type <arrayStatement> arrayStatement
%type <assignmentMathStatement> assignmentForLoopExpression
%type <macroInvocationStatement> macroInvocationStatement
%type <statementBlock> statementBlock
%type <arrayAssignment> arrayAssignment
%type <stringList> strings
%type <boolList> boolList
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
     NEW_LINE unit                                                          { $$ = NewLineUnitSemanticAction($2); }
    | externalDeclaration                                                 { $$ = SingleExternalDeclarationSemanticAction($1); }
    | externalDeclaration  unit                                      { $$ = AppendExternalDeclarationSemanticAction($2, $1); }
    ;
externalDeclaration:
      functionDefinition                                            { $$ = FunctionDefinitionExternalDeclarationSemanticAction($1); }
    | statement                                                     { $$ = StatementExternalDeclarationSemanticAction($1); }
    | macroStatement                                                 { $$ = MacroExternalDeclarationSemanticAction($1); }

    ;


statementList: statement statementList                             { $$ = AppendStatementListSemanticAction($2, $1); }
    | statement                                                                 { $$ = SingleStatementListSemanticAction($1); }
	;


functionDefinition:
      INT GENERIC_ID OPEN_PARENTHESIS argumentDefList CLOSE_PARENTHESIS statementBlock  { $$ = FunctionDefinitionSemanticAction(_INT, $2, $4, $6); }
    | VOID GENERIC_ID OPEN_PARENTHESIS argumentDefList CLOSE_PARENTHESIS statementBlock  { $$ = FunctionDefinitionSemanticAction(_VOID, $2, $4, $6); }
    | STRING_TYPE GENERIC_ID OPEN_PARENTHESIS argumentDefList CLOSE_PARENTHESIS statementBlock     { $$ = FunctionDefinitionSemanticAction(_STRING, $2, $4, $6); }
    | BOOL GENERIC_ID OPEN_PARENTHESIS argumentDefList CLOSE_PARENTHESIS statementBlock  { $$ = FunctionDefinitionSemanticAction(_STRING, $2, $4, $6); }
    ;

statement:
    forLoop                                                        { $$ = ForLoopStatementSemanticAction($1); }
  | matchStatement                                                  { $$ = MatchStatementSemanticAction($1); }
  | whileLoop                                                      { $$ = WhileLoopStatementSemanticAction($1); }
  | ifStatement                                                    { $$ = IfStatementSemanticAction($1); }
  | printStatement                                                 { $$ = PrintStatementSemanticAction($1);}
  | sortStatement                                                  { $$ = SortStatementSemanticAction($1);}
  | assignmentStatement                                             { $$ = AssignmentStatementSemanticAction($1);}
  | unaryChangeOperatorStatement                                    { $$ = UnaryChangeOperatorStatementSemanticAction($1); }
  | returnStatement                                                 { $$ = ReturnStatementSemanticAction($1); }
  | functionStatement                                               { $$ = FunctionStatementSemanticAction($1); }
  | variableStatement                                               { $$ = VariableStatementSemanticAction($1); }
  | macroInvocationStatement                                          { $$ = MacroInvocationStatementSemanticAction($1); }
  ;


macroInvocationStatement: MACRO_ID OPEN_PARENTHESIS argumentList CLOSE_PARENTHESIS                        { $$ = MacroInvocationSemanticAction($1, $3); }
    ;




unaryChangeOperatorStatement:
    INT_ID ADD_ONE                  NEW_LINE                                         { $$ = UnaryChangeOperatorSemanticAction($1, POST_INCREMENT); }
    | INT_ID MINUS_ONE              NEW_LINE                                       { $$ = UnaryChangeOperatorSemanticAction($1, POST_DECREMENT); }
    | ADD_ONE INT_ID                NEW_LINE                                        { $$ = UnaryChangeOperatorSemanticAction($2, PRE_INCREMENT); }
    | MINUS_ONE INT_ID              NEW_LINE                                 { $$ = UnaryChangeOperatorSemanticAction($2, PRE_DECREMENT); }
    | intArrayAccess ADD_ONE        NEW_LINE                                  { $$ = UnaryChangeArraySemanticAction($1, POST_INCREMENT); }
    | intArrayAccess MINUS_ONE      NEW_LINE                                     { $$ = UnaryChangeArraySemanticAction($1, POST_DECREMENT); }
    | ADD_ONE intArrayAccess        NEW_LINE                                    { $$ = UnaryChangeArraySemanticAction($2, PRE_INCREMENT); }
    | MINUS_ONE intArrayAccess      NEW_LINE                                   { $$ = UnaryChangeArraySemanticAction($2, PRE_DECREMENT); }
    ;

returnStatement: RETURN expression  NEW_LINE                                 { $$ = ReturnSemanticAction($2); }
    | RETURN NEW_LINE                                     { $$ = ReturnEmptySemanticAction(); }
    ;

boolFunctionStatement: BOOL_FUNCTION_ID OPEN_PARENTHESIS argumentList CLOSE_PARENTHESIS NEW_LINE
                                                                        { $$ = FunctionSemanticAction($1, $3); }
                     ;
intFunctionStatement: INT_FUNCTION_ID OPEN_PARENTHESIS argumentList CLOSE_PARENTHESIS NEW_LINE
                                                                        { $$ = FunctionSemanticAction($1, $3); }
                     ;
stringFunctionStatement: STRING_FUNCTION_ID OPEN_PARENTHESIS argumentList CLOSE_PARENTHESIS NEW_LINE
                                                                        { $$ = FunctionSemanticAction($1, $3); }
                     ;

functionStatement: boolFunctionStatement                                { $$ = $1 ;}
    | intFunctionStatement                                              { $$ = $1 ;}
    | stringFunctionStatement                                           { $$ = $1 ;}
    ;

    boolFunctionExpression: BOOL_FUNCTION_ID OPEN_PARENTHESIS argumentList CLOSE_PARENTHESIS
                                                                            { $$ = FunctionSemanticAction($1, $3); }
                         ;
    intFunctionExpression: INT_FUNCTION_ID OPEN_PARENTHESIS argumentList CLOSE_PARENTHESIS
                                                                            { $$ = FunctionSemanticAction($1, $3); }
                         ;
    stringFunctionExpression: STRING_FUNCTION_ID OPEN_PARENTHESIS argumentList CLOSE_PARENTHESIS
                                                                            { $$ = FunctionSemanticAction($1, $3); }


macroStatement: MACRO GENERIC_ID OPEN_PARENTHESIS stringList CLOSE_PARENTHESIS ARROW  mathExpression NEW_LINE
                                                                        { $$ = MacroSemanticAction($2, $4, $7); }
 ;

statementBlock: OPEN_BRACE statementList CLOSE_BRACE           { $$ = StatementBlockSemanticAction($2); }
            | OPEN_BRACE NEW_LINE statementList CLOSE_BRACE     { $$ =  StatementBlockSemanticAction($3); }
  ;

sortStatement: SORT INT_ARRAY_ID ASC  NEW_LINE                      { $$ = SortSemanticAction($2, ORDER_ASC); }
            | SORT INT_ARRAY_ID DESC NEW_LINE                     { $$ = SortSemanticAction($2, ORDER_DESC); }

matchStatement: MATCH GENERIC_ID OPEN_BRACE matchCaseList CLOSE_BRACE   { $$ = MatchSemanticAction($2, $4); }
    | MATCH GENERIC_ID OPEN_BRACE NEW_LINE matchCaseList CLOSE_BRACE { $$ = MatchSemanticAction($2, $5); }
   ;

matchCaseList:
    matchCase                                   { $$ = SingleCaseListSemanticAction($1); }
  | matchCaseList matchCase                     { $$ = AppendCaseListSemanticAction($2, $1); }

  ;

matchCase: INTEGER ARROW statement                                 { $$ = MatchCaseSemanticAction($1, $3); }
| STRING ARROW statement                                           { $$ = MatchCaseStringSemanticAction($1, $3); }
| DEFAULT ARROW  statement
                                                                    { $$ = MatchDefaultCaseSemanticAction($3); }
    ;

forLoop: FOR assignmentForLoopExpression TO constant statementBlock
                                                                    { $$ = ForLoopSemanticAction($2, $4, $5); }
	;

whileLoop:
    WHILE boolExpression statementBlock
                                                                    { $$ = WhileLoopSemanticAction($2, $3); }
    ;

ifStatement: IF boolExpression statementBlock elseStatement  { $$ = IfThenSemanticAction($2, $3,$4); }
  ;

elseStatement:
    ELSE statementBlock                                            { $$ = ElseStatementSemanticAction($2); }
  | ELSE ifStatement                                             { $$ = ElseIfStatementSemanticAction($2); }
  | %empty                                                        { $$ = NULL; }
  ;

factor:
    OPEN_PARENTHESIS mathExpression CLOSE_PARENTHESIS                { $$ = ParenthesisFactorSemanticAction($2); }
	| constant														{ $$ = ConstantFactorSemanticAction($1); }
	| INT_ID                                                    { $$ = IdentifierFactorSemanticAction($1);}
	| intArrayAccess                                              { $$ = ArrayFactorSemanticAction($1); }
	| intFunctionExpression                                          { $$ = FunctionCallFactorSemanticAction($1); }
	| unaryIncrementOperatorExpression                          { $$ = UnitIncrementOperatorFactorSemanticAction($1); }
	| macroInvocationStatement                                          { $$ = MacroInvocationFactorSemanticAction($1); }
	;

unaryIncrementOperatorExpression:
    ADD_ONE INT_ID                                          { $$ = UnaryChangeOperatorSemanticAction($2, PRE_INCREMENT); }
    | MINUS_ONE INT_ID                                         { $$ = UnaryChangeOperatorSemanticAction($2, PRE_DECREMENT); }
    | INT_ID ADD_ONE                                          { $$ = UnaryChangeOperatorSemanticAction($1, POST_INCREMENT); }
    | INT_ID MINUS_ONE                                         { $$ = UnaryChangeOperatorSemanticAction($1, POST_DECREMENT); }
    | intArrayAccess ADD_ONE                                          { $$ = UnaryChangeArraySemanticAction($1, POST_INCREMENT); }
    | intArrayAccess MINUS_ONE                                         { $$ = UnaryChangeArraySemanticAction($1, POST_DECREMENT); }
    | ADD_ONE intArrayAccess                                           { $$ = UnaryChangeArraySemanticAction($2, PRE_INCREMENT); }
    | MINUS_ONE intArrayAccess                                          { $$ = UnaryChangeArraySemanticAction($2, PRE_DECREMENT); }
    ;

constant: INTEGER													{ $$ = IntegerConstantSemanticAction($1); }
	;

expression:
      mathExpression                                                { $$ = MathExpressionSemanticAction($1); }
    | boolExpression                                                { $$ = BooleanExpressionSemanticAction($1); }
   | stringExpression                                              { $$ = StringExpressionSemanticAction($1); }
    ;

mathExpression:
    mathExpression[left] ADD mathExpression[right]		                    { $$ = ArithmeticExpressionSemanticAction($left, $right, ADDITION); }
    | mathExpression[left] DIV mathExpression[right]				        { $$ = ArithmeticExpressionSemanticAction($left, $right, DIVISION); }
    | mathExpression[left] MUL mathExpression[right]				        { $$ = ArithmeticExpressionSemanticAction($left, $right, MULTIPLICATION); }
    | mathExpression[left] SUB mathExpression[right]			            { $$ = ArithmeticExpressionSemanticAction($left, $right, SUBTRACTION); }
    | factor												                { $$ = FactorExpressionSemanticAction($1); }
    ;

stringExpression: STRING                                            { $$ = FactorStringExpressionSemanticAction($1); }
    | stringArrayAccess                                             { $$ = ArrayStringAccessSemanticAction($1); }
    | STRING_ID                                                { $$ = IdentifierStringExpressionSemanticAction($1); }
    | stringFunctionExpression                                      { $$ = FunctionCallStringExpressionSemanticAction($1); }
    ;

boolExpression:
    boolExpression[left] AND boolExpression[right]		                    { $$ = ConditionalExpressionSemanticAction($left, $right, LOGICAL_AND); }
    | boolExpression[left] OR boolExpression[right]				            { $$ = ConditionalExpressionSemanticAction($left, $right, LOGICAL_OR); }
    | mathExpression EQ mathExpression                                      { $$ = BooleanSemanticAction($1, $3, EQUAL); }
    | mathExpression NEQ mathExpression                                     { $$ = BooleanSemanticAction($1, $3, NOT_EQUAL); }
    | mathExpression LT mathExpression                                      { $$ = BooleanSemanticAction($1, $3, LESS_THAN); }
    | mathExpression LTE mathExpression                                     { $$ = BooleanSemanticAction($1, $3, LESS_EQUAL); }
    | mathExpression GT mathExpression                                      { $$ = BooleanSemanticAction($1, $3, GREATER_THAN); }
    | mathExpression GTE mathExpression                                     { $$ = BooleanSemanticAction($1, $3, GREATER_EQUAL); }
    | boolFactor                                                        { $$ = BoolFactorExpressionSemanticAction($1); }
    ;

    boolArrayAccess: BOOL_ARRAY_ID OPEN_BRACKETS mathExpression CLOSE_BRACKETS
                        { $$ = ArrayAccessSemanticAction($1, $3); }
    ;
    intArrayAccess: INT_ARRAY_ID OPEN_BRACKETS mathExpression CLOSE_BRACKETS
                        { $$ = ArrayAccessSemanticAction($1, $3); }
                        ;
    stringArrayAccess: STRING_ARRAY_ID OPEN_BRACKETS mathExpression CLOSE_BRACKETS
                        { $$ = ArrayAccessSemanticAction($1, $3); }
                        ;

boolFactor:
    OPEN_PARENTHESIS boolExpression CLOSE_PARENTHESIS                { $$ = ParenthesisExpressionSemanticAction($2); }
    | TRUE                                                        { $$ = BooleanConstantSemanticAction(TRUE); }
    | FALSE                                                       { $$ = BooleanConstantSemanticAction(FALSE); }
    | BOOL_ID                                                  { $$ = IdentifierBoolFactorSemanticAction($1);}
    | boolFunctionExpression                                          { $$ = FunctionCallBoolFactorSemanticAction($1); }
    | NOT boolExpression                                                { $$ = NotExpressionSemanticAction($2); }
    | boolArrayAccess                                               { $$ = ArrayBoolFactorSemanticAction($1); }
    ;

assignmentStatement:
    assignmentMathStatement                                          { $$ = AssignmentIntExpressionSemanticAction($1); }
    | assignmentBoolStatement                                         { $$ = AssignmentBoolExpressionSemanticAction($1); }
    | assignmentStringStatement                                       { $$ = AssignmentStringExpressionSemanticAction($1); }
    | arrayStatement                                                 { $$ = AssignmentArrayStatementSemanticAction($1); }
    | arrayAssignment                                                { $$ = AssignmentArrayExpressionSemanticAction($1); }
    ;
assignmentMathStatement: INT_ID ASSIGNMENT mathExpression  NEW_LINE             { $$ = AssignmentIntSemanticAction($1,$3);}
     ;
assignmentForLoopExpression: GENERIC_ID ASSIGNMENT mathExpression                   { $$ = AssignmentIntSemanticAction($1,$3);}
     ;

assignmentBoolStatement: BOOL_ID ASSIGNMENT boolExpression  NEW_LINE             { $$ = AssignmentBoolSemanticAction($1,$3);}
     ;
assignmentStringStatement: STRING_ID ASSIGNMENT stringExpression  NEW_LINE            { $$ = AssignmentStringSemanticAction($1,$3);}

    variableStatement:
          BOOL GENERIC_ID ASSIGNMENT boolExpression   NEW_LINE            { $$ = VariableBoolDeclarationSemanticAction( $2, $4); }
        | INT GENERIC_ID ASSIGNMENT mathExpression   NEW_LINE                      { $$ = VariableIntDeclarationSemanticAction( $2, $4); }
        | STRING_TYPE GENERIC_ID ASSIGNMENT stringExpression  NEW_LINE                    { $$ = VariableStringDeclarationSemanticAction( $2, $4); }
        | BOOL GENERIC_ID NEW_LINE                                      { $$ = VariableDeclarationSemanticAction(_BOOL, $2, NULL); }
        | INT GENERIC_ID  NEW_LINE                                      { $$ = VariableDeclarationSemanticAction(_INT, $2, NULL); }
        | STRING_TYPE GENERIC_ID NEW_LINE                                    { $$ = VariableDeclarationSemanticAction(_STRING, $2, NULL); }
        ;


arrayAssignment: intArrayAccess ASSIGNMENT mathExpression  NEW_LINE             { $$ = AssignmentIntArraySemanticAction($1,$3);}
    | boolArrayAccess ASSIGNMENT boolExpression  NEW_LINE             { $$ = AssignmentBoolArraySemanticAction($1,$3);}
    | stringArrayAccess ASSIGNMENT stringExpression  NEW_LINE            { $$ = AssignmentStringArraySemanticAction($1,$3);}
    ;

arrayStatement:
    GENERIC_ID OPEN_BRACKETS CLOSE_BRACKETS ASSIGNMENT OPEN_BRACE integerList CLOSE_BRACE       { $$ = ArrayIntStatementSemanticAction($1, $6); }
    | INT GENERIC_ID OPEN_BRACKETS CLOSE_BRACKETS ASSIGNMENT OPEN_BRACE integerList CLOSE_BRACE { $$ = ArrayIntStatementSemanticAction($2, $7); }
    | INT GENERIC_ID OPEN_BRACKETS mathExpression CLOSE_BRACKETS                                { $$ = ArrayDeclarationSemanticAction($2, $4, _INT); }
    | BOOL GENERIC_ID OPEN_BRACKETS CLOSE_BRACKETS ASSIGNMENT OPEN_BRACE boolList CLOSE_BRACE { $$ = ArrayBoolStatementSemanticAction($2, $7); }
    | BOOL GENERIC_ID OPEN_BRACKETS mathExpression CLOSE_BRACKETS                                { $$ = ArrayDeclarationSemanticAction($2, $4, _BOOL); }
    | STRING_TYPE GENERIC_ID OPEN_BRACKETS CLOSE_BRACKETS ASSIGNMENT OPEN_BRACE strings CLOSE_BRACE { $$ = ArrayStringStatementSemanticAction($2, $7); }
    | STRING_TYPE GENERIC_ID OPEN_BRACKETS mathExpression CLOSE_BRACKETS                            { $$ = ArrayDeclarationSemanticAction($2, $4, _STRING); }
    ;

integerList:
      INTEGER                                                      { $$ = SingleArrayListSemanticAction($1); }
    | integerList COMMA INTEGER                                    { $$ = AppendArrayListSemanticAction($1, $3); }
    ;
boolList:
      TRUE                                                        { $$ = SingleBoolArrayListSemanticAction($1); }
    | boolList COMMA TRUE                                        { $$ = AppendBoolArrayListSemanticAction($1, $3); }
    | FALSE                                                       { $$ = SingleBoolArrayListSemanticAction($1); }
    | boolList COMMA FALSE                                      { $$ = AppendBoolArrayListSemanticAction($1, $3); }
    ;
strings:
      STRING                                                     { $$ = SingleStringListSemanticAction($1); }
    | strings COMMA STRING                                     { $$ = AppendStringListSemanticAction($1, $3); }

printStatement: PRINT stringExpression  NEW_LINE                                 { $$ = PrintIdentifierSemanticAction($2); }
    | PRINT mathExpression  NEW_LINE                                        { $$ = PrintMathExpressionSemanticAction($2); }
    ;

argumentList:
    argumentValue                                       { $$ = ArgumentValueSemanticAction($1); }
  | argumentList COMMA argumentValue                            { $$ = AppendArgumentListSemanticAction($1, $3); }
  | %empty                                                        { $$ = NULL; }
  ;

  stringList:
      GENERIC_ID                                       { $$ = SingleStringListSemanticAction($1); }
    | stringList COMMA GENERIC_ID                            { $$ = AppendStringListSemanticAction($1, $3); }


  argumentValue: mathExpression                                        { $$ = MathExpressionArgValueSemanticAction($1); }
    | boolExpression                                        { $$ = BoolExpressionArgValueSemanticAction($1); }
    | stringExpression                                        { $$ = StringExpressionArgValueSemanticAction($1); }
    | BOOL_ARRAY_ID                                         { $$ = ArrayBoolSemanticAction($1); }
    | INT_ARRAY_ID                                          { $$ = ArrayIntSemanticAction($1); }
    | STRING_ARRAY_ID                                       { $$ = ArrayStringSemanticAction($1); }
    ;

argumentDefList:
    argumentDef                                       { $$ = SingleArgumentDefListSemanticAction($1); }
  | argumentDefList COMMA argumentDef                            { $$ = AppendArgumentDefListSemanticAction($1, $3); }
  | %empty                                                        { $$ = NULL; }
  ;
argumentDef:
    INT GENERIC_ID                                                       { $$ = ArgumentDefSemanticAction($2, _INT); }
    | BOOL GENERIC_ID                                                    { $$ = ArgumentDefSemanticAction($2, _BOOL); }
    | STRING_TYPE GENERIC_ID                                             { $$ = ArgumentDefSemanticAction($2, _STRING); }
    | INT GENERIC_ID OPEN_BRACKETS CLOSE_BRACKETS GENERIC_ID             { $$ = ArgumentDefSemanticAction($2, _INT_ARRAY); }
    | BOOL GENERIC_ID OPEN_BRACKETS CLOSE_BRACKETS GENERIC_ID            { $$ = ArgumentDefSemanticAction($2, _BOOL_ARRAY); }
    | STRING_TYPE GENERIC_ID OPEN_BRACKETS CLOSE_BRACKETS GENERIC_ID     { $$ = ArgumentDefSemanticAction($2, _STRING_ARRAY); }
    ;

%%
