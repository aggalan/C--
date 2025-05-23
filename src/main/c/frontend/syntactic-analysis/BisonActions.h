#ifndef BISON_ACTIONS_HEADER
#define BISON_ACTIONS_HEADER

#include "../../shared/CompilerState.h"
#include "../../shared/Logger.h"
#include "../../shared/Type.h"
#include "AbstractSyntaxTree.h"
#include "SyntacticAnalyzer.h"
#include <stdlib.h>

/** Initialize module's internal state. */
void initializeBisonActionsModule();

/** Shutdown module's internal state. */
void shutdownBisonActionsModule();

/**
 * Bison semantic actions.
 */
Constant * IntegerConstantSemanticAction(const int value);
MathExpression * ArithmeticExpressionSemanticAction( MathExpression * leftExpression, MathExpression * rightExpression, MathExpressionType type);
MathExpression * FactorExpressionSemanticAction(Factor * factor);
Factor * ConstantFactorSemanticAction(Constant * constant);
StatementList *SingleStatementListSemanticAction(Statement *stmt);
StatementList * AppendStatementListSemanticAction(StatementList *list, Statement *stmt);
Statement *ForLoopStatementSemanticAction(ForLoop *loop);
Statement *MatchStatementSemanticAction(MatchStatement *stmt);
MatchStatement *MatchSemanticAction(String id, CaseList *cases);
CaseList *SingleCaseListSemanticAction(Case *c);
CaseList *AppendCaseListSemanticAction(Case * c, CaseList *list);
Case *MatchCaseSemanticAction(int value, Statement *body); //FIXME
ForLoop *ForLoopSemanticAction(AssignmentMathStatement * assignment, Constant * end, StatementBlock* body);
Statement *WhileLoopStatementSemanticAction(WhileLoop *loop);
WhileLoop *WhileLoopSemanticAction(BoolExpression *condition, StatementBlock *body);
Statement *IfStatementSemanticAction(IfStatement *stmt);
IfStatement *IfThenSemanticAction(BoolExpression *condition, StatementBlock *thenBranch,ElseStatement *elseBranch);
IfStatement *IfElseSemanticAction(BoolExpression *condition, StatementBlock *thenBranch, ElseStatement *elseBranch);
PrintStatement * PrintIdentifierSemanticAction(StringExpression * stringExpression);
BoolExpression * ConditionalExpressionSemanticAction(BoolExpression * conditionalExpression1, BoolExpression * conditionalExpression2, OperatorType type);
BoolExpression * BooleanSemanticAction(MathExpression * mathExpression1, MathExpression * mathExpression2, ComparatorType type );
BoolExpression * BoolFactorExpressionSemanticAction(BoolFactor * boolFactor);
BoolFactor * NotExpressionSemanticAction(BoolExpression * conditionalExpression1);
BoolFactor * ParenthesisExpressionSemanticAction(BoolExpression * conditionalExpression1);
BoolFactor * BooleanConstantSemanticAction(Bool value);
BoolFactor * IdentifierBoolFactorSemanticAction(String identifier);

Statement * PrintStatementSemanticAction(PrintStatement * stmt);
Statement * SortStatementSemanticAction(SortStatement * identifier);
SortStatement * SortSemanticAction(String identifier, Order order);
Factor * IdentifierFactorSemanticAction(String identifier);
BoolFactor * FunctionCallBoolFactorSemanticAction(FunctionStatement * functionStatement);
BoolFactor * ArrayBoolFactorSemanticAction(ArrayAccess * arrayAccess);
Statement  * MacroStatementSemanticAction(MacroStatement * stmt);
MacroStatement * MacroSemanticAction(String identifier, StringList *args, MathExpression * body);
StringList * SingleStringListSemanticAction(String str);
StringList * AppendStringListSemanticAction(StringList *list, String str);
Statement *  AssignmentStatementSemanticAction(AssignmentStatement * assignmentStatement);
Factor * ParenthesisFactorSemanticAction(MathExpression * expression);
StringExpression * FactorStringExpressionSemanticAction(String expression);
StringExpression * IdentifierStringExpressionSemanticAction(String identifier);
FunctionStatement * FunctionSemanticAction(String identifier, ArgumentList * parameters);
Statement * ReturnStatementSemanticAction(ReturnStatement * stmt);
Statement * FunctionStatementSemanticAction(FunctionStatement * stmt);
FunctionDefinition * FunctionDefinitionSemanticAction(Type type,String identifier, ArgumentDefList * parameters, StatementBlock * body);
ReturnStatement * ReturnSemanticAction(Expression * expression);
Factor * FunctionCallFactorSemanticAction(FunctionStatement * functionStatement);
Unit * SingleExternalDeclarationSemanticAction(ExternalDeclaration * externalDeclaration);
Unit * AppendExternalDeclarationSemanticAction(Unit * unit, ExternalDeclaration * externalDeclaration);
ExternalDeclaration * FunctionDefinitionExternalDeclarationSemanticAction(FunctionDefinition * functionDefinition);
ExternalDeclaration * StatementExternalDeclarationSemanticAction(Statement * statement);
Program * ProgramSemanticAction(CompilerState * compilerState, Unit * unit);
Program * EmptyProgramSemanticAction(CompilerState * compilerState);
ElseStatement* ElseStatementSemanticAction(StatementBlock * stml);
ElseStatement * ElseIfStatementSemanticAction(IfStatement * ifStatement);
Statement * VariableStatementSemanticAction(VariableStatement * var);
VariableStatement * VariableDeclarationSemanticAction(Type type, String identifier, Expression * expression);
Statement * UnaryChangeOperatorStatementSemanticAction( UnaryChangeOperatorStatement * stmt);
UnaryChangeOperatorStatement * UnaryChangeOperatorSemanticAction(String identifier, int type);
UnaryChangeOperatorStatement * UnaryChangeArraySemanticAction(ArrayAccess * arrayAccess, int type);
ArrayStatement * ArrayIntStatementSemanticAction(String identifier, IntList * elements);
ArrayStatement * ArrayStringStatementSemanticAction(String identifier, StringList * elements);
ArrayStatement * ArrayBoolStatementSemanticAction(String identifier, BoolList * elements);
ArrayStatement * ArrayDeclarationSemanticAction(String identifier, MathExpression * size, Type type);
Factor * MacroInvocationFactorSemanticAction(MacroInvocationStatement * macroInvocationStatement);
IntList * SingleArrayListSemanticAction(int integer);
IntList * AppendArrayListSemanticAction(IntList *list, int integer);
Case * MatchDefaultCaseSemanticAction(Statement * body);
Case * MatchCaseStringSemanticAction(String str, Statement *body);
ReturnStatement * ReturnEmptySemanticAction();
StatementBlock * StatementBlockSemanticAction(StatementList * statementList);
Unit * NewLineUnitSemanticAction();
Factor * ArrayFactorSemanticAction(ArrayAccess * arrayAccess);
ArrayAccess * ArrayAccessSemanticAction(String identifier, MathExpression * index);
Expression * MathExpressionSemanticAction(MathExpression * mathExpression);
Expression * BooleanExpressionSemanticAction(BoolExpression * boolExpression);
AssignmentStatement * AssignmentIntExpressionSemanticAction(AssignmentMathStatement * assignmentStatement);
AssignmentMathStatement * AssignmentIntSemanticAction(String id, MathExpression * mathExpression);
AssignmentStatement * AssignmentStringExpressionSemanticAction(AssignmentStringStatement * assignmentStatement);
AssignmentStringStatement * AssignmentStringSemanticAction(String id, StringExpression * expression);
AssignmentStatement * AssignmentBoolExpressionSemanticAction(AssignmentBoolStatement * assignmentStatement);
AssignmentBoolStatement * AssignmentBoolSemanticAction(String id, BoolExpression * boolExpression);
AssignmentStatement * AssignmentArrayStatementSemanticAction(ArrayStatement * assignmentStatement);
AssignmentStatement * AssignmentArrayExpressionSemanticAction(ArrayAssignment * assignmentStatement);
VariableStatement * VariableBoolDeclarationSemanticAction(String identifier, BoolExpression * value);
VariableStatement * VariableIntDeclarationSemanticAction(String identifier, MathExpression * value);
VariableStatement * VariableStringDeclarationSemanticAction(String identifier, StringExpression * value);
StringExpression * ArrayStringAccessSemanticAction(ArrayAccess * arrayAccess);
Factor * UnitIncrementOperatorFactorSemanticAction(UnaryChangeOperatorStatement * statement);
ArgumentList * ArgumentValueSemanticAction(ArgumentValue * argumentValue);
ArgumentList * AppendArgumentListSemanticAction(ArgumentList * list, ArgumentValue * argumentValue);
ArgumentValue * MathExpressionArgValueSemanticAction(MathExpression * expression);
ArgumentValue * StringExpressionArgValueSemanticAction(StringExpression * expression);
ArgumentValue * BoolExpressionArgValueSemanticAction(BoolExpression * expression);
ArgumentValue * FunctionExpressionArgValueSemanticAction( FunctionStatement * functionStatement);
ArgumentValue * ArrayBoolSemanticAction(String identifier);
ArgumentValue * ArrayIntSemanticAction(String identifier);
ArgumentValue * ArrayStringSemanticAction(String identifier);
ArgumentDefList * SingleArgumentDefListSemanticAction(ArgumentDef * argumentDef);
ArgumentDefList * AppendArgumentDefListSemanticAction(ArgumentDefList * list,ArgumentDef * argumentDef);
ArgumentDef * ArgumentDefSemanticAction(String identifier, Type type);
Expression * StringExpressionSemanticAction(StringExpression * expression);
StringExpression * FunctionCallStringExpressionSemanticAction(FunctionStatement * functionStatement);
PrintStatement * PrintMathExpressionSemanticAction(MathExpression * mathExpression);
Statement * MacroInvocationStatementSemanticAction(MacroInvocationStatement * macroInvocationStatement);
MacroInvocationStatement * MacroInvocationSemanticAction(String identifier, ArgumentList * args);
ExternalDeclaration * MacroExternalDeclarationSemanticAction(MacroStatement * macroStatement);
ArrayAssignment * AssignmentIntArraySemanticAction(ArrayAccess * arrayAccess, MathExpression * mathExpression);
ArrayAssignment * AssignmentStringArraySemanticAction(ArrayAccess * arrayAccess, StringExpression * stringExpression);
ArrayAssignment * AssignmentBoolArraySemanticAction(ArrayAccess * arrayAccess, BoolExpression * boolExpression);
BoolList * SingleBoolArrayListSemanticAction(Bool boolean);
BoolList * AppendBoolArrayListSemanticAction(BoolList *list, Bool boolean);
#endif
