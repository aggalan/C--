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
Factor * ExpressionFactorSemanticAction(MathExpression * expression);
Program * ExpressionProgramSemanticAction(CompilerState * compilerState, MathExpression * expression);
Program * StatementListProgramSemanticAction(CompilerState * compilerState, StatementList *statementList);
StatementList *SingleStatementListSemanticAction(Statement *stmt);
StatementList * AppendStatementListSemanticAction(StatementList *list, Statement *stmt);
Statement *ExpressionStatementSemanticAction(MathExpression *expr);
Statement *ForLoopStatementSemanticAction(ForLoop *loop);
Statement *MatchStatementSemanticAction(MatchStatement *stmt);
MatchStatement *MatchSemanticAction(String id, CaseList *cases);
CaseList *SingleCaseListSemanticAction(Case *c);
CaseList *AppendCaseListSemanticAction(CaseList *list, Case *c);
Case *MatchCaseSemanticAction(int value, StatementList *body);
ForLoop *ForLoopSemanticAction(AssignmentMathExpression * assignment, Constant * end, StatementList* body);
Statement *WhileLoopStatementSemanticAction(WhileLoop *loop);
WhileLoop *WhileLoopSemanticAction(MathExpression *condition, StatementList *body);
Statement *IfStatementSemanticAction(IfStatement *stmt);
IfStatement *IfThenSemanticAction(MathExpression *condition, StatementList *thenBranch);
IfStatement *IfElseSemanticAction(MathExpression *condition, StatementList *thenBranch, StatementList *elseBranch);
AssignmentMathExpression *assignmentMathExpressionSemanticAction(String id, MathExpression *expr);
MatchStatement *MatchSemanticAction(String id, CaseList *cases);
PrintStatement * PrintIdentifierSemanticAction(String id);
PrintStatement * PrintStringSemanticAction(String str);
ConditionalExpression * ConditionalExpressionSemanticAction(String id);
Statement * PrintStatementSemanticAction(PrintStatement * stmt);
#endif
