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
Expression * ArithmeticExpressionSemanticAction( Expression * leftExpression, Expression * rightExpression, MathExpressionType type);
Expression * FactorExpressionSemanticAction(Factor * factor);
Factor * ConstantFactorSemanticAction(Constant * constant);
StatementList *SingleStatementListSemanticAction(Statement *stmt);
StatementList * AppendStatementListSemanticAction(StatementList *list, Statement *stmt);
Statement *ForLoopStatementSemanticAction(ForLoop *loop);
Statement *MatchStatementSemanticAction(MatchStatement *stmt);
MatchStatement *MatchSemanticAction(String id, CaseList *cases);
CaseList *SingleCaseListSemanticAction(Case *c);
CaseList *AppendCaseListSemanticAction(CaseList *list, Case *c);
Case *MatchCaseSemanticAction(int value, StatementList *body); //FIXME
ForLoop *ForLoopSemanticAction(AssignmentStatement * assignment, Constant * end, StatementList* body);
Statement *WhileLoopStatementSemanticAction(WhileLoop *loop);
WhileLoop *WhileLoopSemanticAction(Expression *condition, StatementList *body);
Statement *IfStatementSemanticAction(IfStatement *stmt);
IfStatement *IfThenSemanticAction(Expression *condition, StatementList *thenBranch,ElseStatement *elseBranch);
IfStatement *IfElseSemanticAction(Expression *condition, StatementList *thenBranch, ElseStatement *elseBranch);
MatchStatement *MatchSemanticAction(String id, CaseList *cases);
PrintStatement * PrintIdentifierSemanticAction(String id);
PrintStatement * PrintStringSemanticAction(String str);
Expression * ConditionalExpressionSemanticAction(Expression * conditionalExpression1, Expression * conditionalExpression2, OperatorType type);
Expression * BooleanSemanticAction(Expression * math_expression1,Expression * math_expression2, ComparatorType type );
Expression * NotExpressionSemanticAction(Expression * conditionalExpression1);
Statement * PrintStatementSemanticAction(PrintStatement * stmt);
Statement * SortStatementSemanticAction(SortStatement * identifier);
SortStatement * SortSemanticAction(String identifier);
Factor * IdentifierFactorSemanticAction(String identifier);
Statement  * MacroStatementSemanticAction(MacroStatement * stmt);
MacroStatement * MacroSemanticAction(String identifier, StringList *args, Statement * body);
StringList * SingleStringListSemanticAction(String str);
StringList * AppendStringListSemanticAction(StringList *list, String str);
Statement *  AssignmentStatementSemanticAction(AssignmentStatement * assignment_statement);
AssignmentStatement * AssignmentExpressionSemanticAction(String id, Expression * assignment_expression);
Factor * ParenthesisFactorSemanticAction(Expression * expression);
Expression * StringExpressionSemanticAction(String expression);
Factor * BooleanFactorSemanticAction(Bool value);
FunctionStatement * FunctionSemanticAction(String identifier, StringList * parameters);
Statement * ReturnStatementSemanticAction(ReturnStatement * stmt);
Statement * FunctionStatementSemanticAction(FunctionStatement * stmt);
FunctionDefinition * FunctionDefinitionSemanticAction(Type type,String identifier, StringList * parameters, StatementList * body);
ReturnStatement * ReturnSemanticAction(Expression * expression);
ReturnStatement * ReturnFunctionStatementSemanticAction(FunctionStatement * functionStatement);
Unit * SingleExternalDeclarationSemanticAction(ExternalDeclaration * externalDeclaration);
Unit * AppendExternalDeclarationSemanticAction(Unit * unit, ExternalDeclaration * externalDeclaration);
ExternalDeclaration * FunctionDefinitionExternalDeclarationSemanticAction(FunctionDefinition * functionDefinition);
ExternalDeclaration * StatementExternalDeclarationSemanticAction(Statement * statement);
Program * ProgramSemanticAction(CompilerState * compilerState, Unit * unit);
Program * EmptyProgramSemanticAction(CompilerState * compilerState);
ElseStatement* ElseStatementSemanticAction(StatementList * stml);
ElseStatement * ElseIfStatementSemanticAction(IfStatement * ifStatement);
Statement * VariableStatementSemanticAction(VariableStatement * var);
VariableStatement * VariableDeclarationSemanticAction(Type type, String identifier, Expression * expression);
Statement * UnaryChangeOperatorStatementSemanticAction( UnaryChangeOperatorStatement * stmt);
UnaryChangeOperatorStatement * UnaryChangeOperatorSemanticAction(String identifier, int type);
#endif
