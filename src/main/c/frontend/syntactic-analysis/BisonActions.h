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
MatchStatement *MatchSemanticAction(String id, CaseList *cases);
PrintStatement * PrintIdentifierSemanticAction(String id);
PrintStatement * PrintStringSemanticAction(String str);
BoolExpression * ConditionalExpressionSemanticAction(BoolExpression * conditionalExpression1, BoolExpression * conditionalExpression2, OperatorType type);
BoolExpression * BooleanSemanticAction(MathExpression * math_expression1, MathExpression * math_expression2, ComparatorType type );
BoolExpression * BoolFactorExpressionSemanticAction(BoolFactor * bool_factor);
BoolFactor * NotExpressionSemanticAction(BoolExpression * conditionalExpression1);
BoolFactor * ParenthesisExpressionSemanticAction(BoolExpression * conditionalExpression1);
BoolFactor * BooleanConstantSemanticAction(Bool value);
BoolFactor * IdentifierBoolFactorSemanticAction(String identifier);

Statement * PrintStatementSemanticAction(PrintStatement * stmt);
Statement * SortStatementSemanticAction(SortStatement * identifier);
SortStatement * SortSemanticAction(String identifier);
Factor * IdentifierFactorSemanticAction(String identifier);
BoolFactor * FunctionCallBoolFactorSemanticAction(FunctionStatement * functionStatement);
BoolFactor * ArrayBoolFactorSemanticAction(ArrayAccess * array_access);
Statement  * MacroStatementSemanticAction(MacroStatement * stmt);
MacroStatement * MacroSemanticAction(String identifier, StringList *args, Statement * body);
StringList * SingleStringListSemanticAction(String str);
StringList * AppendStringListSemanticAction(StringList *list, String str);
Statement *  AssignmentStatementSemanticAction(AssignmentStatement * assignment_statement);
Factor * ParenthesisFactorSemanticAction(MathExpression * expression);
BoolFactor * ParenthesisBoolExpressionSemanticAction(BoolExpression * conditionalExpression1);
Expression * StringExpressionSemanticAction(String expression);
Factor * BooleanFactorSemanticAction(Bool value);
FunctionStatement * FunctionSemanticAction(String identifier, StringList * parameters);
Statement * ReturnStatementSemanticAction(ReturnStatement * stmt);
Statement * FunctionStatementSemanticAction(FunctionStatement * stmt);
FunctionDefinition * FunctionDefinitionSemanticAction(Type type,String identifier, StringList * parameters, StatementBlock * body);
ReturnStatement * ReturnSemanticAction(Expression * expression);
ReturnStatement * ReturnFunctionStatementSemanticAction(FunctionStatement * functionStatement);
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
UnaryChangeOperatorStatement * UnaryChangeArraySemanticAction(ArrayAccess * array_access, int type);
Statement * ArrayStatementSemanticAction(ArrayStatement * array);
ArrayStatement * ArraySemanticAction(String identifier, IntList * elements);

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
AssignmentStatement * AssignmentIntExpressionSemanticAction(AssignmentMathStatement * assignment_statement);
AssignmentMathStatement * AssignmentIntSemanticAction(String id, MathExpression * math_expression);
AssignmentStatement * AssignmentStringExpressionSemanticAction(AssignmentStringStatement * assignment_statement);
AssignmentStringStatement * AssignmentStringSemanticAction(String id, String expression);
AssignmentStatement * AssignmentBoolExpressionSemanticAction(AssignmentBoolStatement * assignment_statement);
AssignmentBoolStatement * AssignmentBoolSemanticAction(String id, BoolExpression * bool_expression);
AssignmentStatement * AssignmentArrayExpressionSemanticAction(ArrayStatement * assignment_statement);
VariableStatement * VariableBoolDeclarationSemanticAction(String identifier, BoolExpression * value);
VariableStatement * VariableIntDeclarationSemanticAction(String identifier, MathExpression * value);
VariableStatement * VariableStringDeclarationSemanticAction(String identifier, String value);
Expression * ArrayStringAccessSemanticAction(ArrayAccess * array_access);
Factor * UnitIncrementOperatorFactorSemanticAction(UnaryChangeOperatorStatement * statement);

#endif
