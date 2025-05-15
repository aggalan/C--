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
Case *MatchCaseSemanticAction(int value, StatementList *body); //FIXME
ForLoop *ForLoopSemanticAction(AssignmentMathExpression * assignment, Constant * end, StatementList* body);
Statement *WhileLoopStatementSemanticAction(WhileLoop *loop);
WhileLoop *WhileLoopSemanticAction(ConditionalExpression *condition, StatementList *body);
Statement *IfStatementSemanticAction(IfStatement *stmt);
IfStatement *IfThenSemanticAction(ConditionalExpression *condition, StatementList *thenBranch);
IfStatement *IfElseSemanticAction(ConditionalExpression *condition, StatementList *thenBranch, StatementList *elseBranch);
AssignmentMathExpression *assignmentMathExpressionSemanticAction(String id, MathExpression *expr);
MatchStatement *MatchSemanticAction(String id, CaseList *cases);
PrintStatement * PrintIdentifierSemanticAction(String id);
PrintStatement * PrintStringSemanticAction(String str);
ConditionalExpression * ConditionalExpressionSemanticAction(BoolExpression * bool_expression);
BoolExpression * BooleanSemanticAction(MathExpression * math_expression1,MathExpression * math_expression2, ComparatorType type );
ConditionalExpression * BooleanExpressionSemanticAction(BoolExpression * bool_expression);
ConditionalExpression * NotExpressionSemanticAction(ConditionalExpression * conditionalExpression1);
ConditionalExpression * ParenthesizedExpressionSemanticAction(ConditionalExpression * conditionalExpression1);
Statement * PrintStatementSemanticAction(PrintStatement * stmt);
Statement * MacroStatementSemanticAction(MacroStatement * stmt);
MacroStatement * MacroSemanticAction(String id, StringList *args, Statement * body);
StringList * SingleStringListSemanticAction(String str);
StringList * AppendStringListSemanticAction(StringList *list, String str);
Factor * IdentifierFactorSemanticAction(String id);
ConditionalExpression * FactorConditionalExpression(Factor * factor);
BoolExpression * IdentifierBooleanSemanticAction(String id);
ConditionalExpression * IdentifierExpressionSemanticAction(String id);
Condition * ConditionFromConditionalExpression(ConditionalExpression * conditionalExpression);
Condition * ConditionFromFactor(Factor * factor);
ConditionalExpression * SimpleConditionSemanticAction(Condition * condition);
BoolExpression * BoolExpressionSemanticAction(BoolExpression * exp1,BoolExpression * exp2, ConditionalType type);
BoolExpression * ComparisonBoolExpressionSemanticAction(ComparisonExpression * comparisonExpression);
ComparisonExpression * ComparisonExpressionSemanticAction(MathExpression * math_expression1, MathExpression * math_expression2, ComparatorType type);
ConditionalExpression * ParenthesizedConditionalExpressionSemanticAction(BoolExpression * conditionalExpression1);
ComparisonExpression * ParenthesisBoolExpressionSemanticAction(BoolExpression * bool_expression);
ConditionalExpression * IdentifierConditionalExpressionSemanticAction(String id);
ComparisonExpression * SingleMathComparisonExpression(MathExpression * math_expression);
#endif
