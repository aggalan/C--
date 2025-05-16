#include "BisonActions.h"

#include "AbstractSyntaxTree.h"
#include "BisonParser.h"

/* MODULE INTERNAL STATE */

static Logger * _logger = NULL;

void initializeBisonActionsModule() {
	_logger = createLogger("BisonActions");
}

void shutdownBisonActionsModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
}

/** IMPORTED FUNCTIONS */

extern unsigned int flexCurrentContext(void);

/* PRIVATE FUNCTIONS */

static void _logSyntacticAnalyzerAction(const char * functionName);

/**
 * Logs a syntactic-analyzer action in DEBUGGING level.
 */
static void _logSyntacticAnalyzerAction(const char * functionName) {
	logDebugging(_logger, "%s", functionName);
}

/* PUBLIC FUNCTIONS */

Constant * IntegerConstantSemanticAction(const int value) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Constant * constant = calloc(1, sizeof(Constant));
	constant->value = value;
	return constant;
}

MathExpression * ArithmeticExpressionSemanticAction(MathExpression * leftExpression, MathExpression * rightExpression, MathExpressionType type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MathExpression * expression = calloc(1, sizeof(MathExpression));
	expression->leftExpression = leftExpression;
	expression->rightExpression = rightExpression;
	expression->type = type;
	return expression;
}

MathExpression * FactorExpressionSemanticAction(Factor * factor) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MathExpression * expression = calloc(1, sizeof(MathExpression));
	expression->factor = factor;
	expression->type = FACTOR;
	return expression;
}

Factor * ConstantFactorSemanticAction(Constant * constant) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->constant = constant;
	factor->type = CONSTANT;
	return factor;
}

Factor * ExpressionFactorSemanticAction(MathExpression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->mathExpression = expression;
	factor->type = EXPRESSION;
	return factor;
}

Program * ExpressionProgramSemanticAction(CompilerState * compilerState, MathExpression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Program * program = calloc(1, sizeof(Program));
	// program->expression = expression;
	compilerState->abstractSyntaxtTree = program;
	if (0 < flexCurrentContext()) {
		logError(_logger, "The final context is not the default (0): %d", flexCurrentContext());
		compilerState->succeed = false;
	}
	else {
		compilerState->succeed = true;
	}
	return program;
}




Program * StatementListProgramSemanticAction(CompilerState * compilerState, StatementList * statementList) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Program * program = calloc(1, sizeof(Program));
	program->statements = statementList;
	compilerState->abstractSyntaxtTree = program;
	if (0 < flexCurrentContext()) {
		logError(_logger, "The final context is not the default (0): %d", flexCurrentContext());
		compilerState->succeed = false;
	}
	else {
		compilerState->succeed = true;
	}
	return program;
}
StatementList* SingleStatementListSemanticAction(Statement* stmt) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementList* list = calloc(1, sizeof(StatementList));
	list->statements = calloc(1, sizeof(Statement*));
	list->statements[0] = stmt;
	list->count = 1;
	return list;
}

// StatementList* AppendStatementListSemanticAction(StatementList* list, Statement* stmt) {
// 	_logSyntacticAnalyzerAction(__FUNCTION__);
// 	list->statements = realloc(list->statements, sizeof(Statement*) * (list->count + 1));
// 	list->statements[list->count++] = stmt;
// 	return list;
// }

StatementList* AppendStatementListSemanticAction(StatementList* list, Statement* stmt) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	if (list == NULL) {
		StatementList* newList = calloc(1, sizeof(StatementList));
		newList->statements = calloc(1, sizeof(Statement*));
		newList->statements[0] = stmt;
		newList->count = 1;
		return newList;
	}
	list->statements = realloc(list->statements, sizeof(Statement*) * (list->count + 1));
	list->statements[list->count++] = stmt;
	return list;
}

Statement* ExpressionStatementSemanticAction(MathExpression* expr) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement* stmt = calloc(1, sizeof(Statement));
	stmt->type = STATEMENT_MathExpression;
	stmt->mathExpression = expr;
	return stmt;
}

Statement* ForLoopStatementSemanticAction(ForLoop* loop) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement* stmt = calloc(1, sizeof(Statement));
	stmt->type = STATEMENT_FOR;
	stmt->forLoop = loop;
	return stmt;
}

Statement* MatchStatementSemanticAction(MatchStatement* stmt) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement* statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_MATCH;
	statement->matchStatement = stmt;
	return statement;
}


CaseList* SingleCaseListSemanticAction(Case* c) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	CaseList* list = calloc(1, sizeof(CaseList));
	list->cases = calloc(1, sizeof(Case*));
	list->cases[0] = c;
	list->count = 1;
	return list;
}

CaseList* AppendCaseListSemanticAction(CaseList* list, Case* c) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	list->cases = realloc(list->cases, sizeof(Case*) * (list->count + 1));
	list->cases[list->count++] = c;
	return list;
}

Case* MatchCaseSemanticAction(int value, StatementList* body) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Case* caseNode = calloc(1, sizeof(Case));
	caseNode->matchValue = value;
	caseNode->body = body;
	return caseNode;
}

ForLoop* ForLoopSemanticAction(AssignmentMathExpression * assignment, Constant * end, StatementList* body) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ForLoop* loop = calloc(1, sizeof(ForLoop));
	loop->assignment = assignment;
	loop->endValue = end;
	loop->body = body;
	return loop;
}



Statement *WhileLoopStatementSemanticAction(WhileLoop *loop) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *stmt = calloc(1, sizeof(Statement));
	stmt->type = STATEMENT_WHILE;
	stmt->whileLoop = loop;
	return stmt;
}
WhileLoop *WhileLoopSemanticAction(BoolExpression *condition, StatementList *body) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	WhileLoop *loop = calloc(1, sizeof(WhileLoop));
	loop->condition = condition;
	loop->body = body;
	return loop;
}
Statement *IfStatementSemanticAction(IfStatement *stmt) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement *statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_IF;
	statement->ifStatement = stmt;
	return statement;
}
IfStatement *IfThenSemanticAction(BoolExpression *condition, StatementList *thenBranch) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IfStatement *stmt = calloc(1, sizeof(IfStatement));
	stmt->condition = condition;
	stmt->thenBranch = thenBranch;
	stmt->elseBranch = NULL;
	return stmt;
}
IfStatement *IfElseSemanticAction(BoolExpression *condition, StatementList *thenBranch, StatementList *elseBranch) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IfStatement *stmt = calloc(1, sizeof(IfStatement));
	stmt->condition = condition;
	stmt->thenBranch = thenBranch;
	stmt->elseBranch = elseBranch;
	return stmt;
}
AssignmentMathExpression *assignmentMathExpressionSemanticAction(String id, MathExpression *expr)
{
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentMathExpression *assignment = calloc(1, sizeof(AssignmentMathExpression));
	assignment->identifier = id;
	assignment->mathExpression = expr;
	return assignment;
}
MatchStatement *MatchSemanticAction(String id, CaseList *cases) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MatchStatement *match = calloc(1, sizeof(MatchStatement));
	match->identifier = id;
	match->caseList = cases;
	return match;
}
PrintStatement * PrintIdentifierSemanticAction(String id){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	PrintStatement * print = calloc(1, sizeof(PrintStatement));
	print->identifier = id;
	return print;
}
PrintStatement * PrintStringSemanticAction(String str){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	PrintStatement * print = calloc(1, sizeof(PrintStatement));
	print->identifier = str;
	return print;
}
BoolExpression * BooleanBinaryExpressionSemanticAction(BoolExpression * boolExpression1, BoolExpression * boolExpression2, BooleanOperator type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolExpression * boolExpr = calloc(1, sizeof(BoolExpression));
	boolExpr->bool_exp_1 = boolExpression1;
	boolExpr->bool_exp_2 = boolExpression2;
	boolExpr->operatorTypeBinary = type;
	boolExpr->type= BOOL_BINARY;
	return boolExpr;
}

BoolExpression * BooleanMathSemanticAction(MathExpression * math_expression1, MathExpression * math_expression2, ComparatorType type ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolExpression * boolExpr = calloc(1, sizeof(BoolExpression));
	boolExpr->math_exp_1 = math_expression1;
	boolExpr->math_exp_2 = math_expression2;
	boolExpr->comparatorType = type;
	boolExpr->type = MATH_BINARY;
	return boolExpr;
}

BoolExpression * BooleanConstantSemanticAction(BoolConstant constant) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolExpression * boolExpr = calloc(1, sizeof(BoolExpression));
  boolExpr->type = BOOL_CONSTANT;
  boolExpr->constant = constant;

  return boolExpr;
}

BoolExpression * NotExpressionSemanticAction(BoolExpression * boolExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolExpression * boolExpr = calloc(1, sizeof(BoolExpression));
	boolExpr->bool_exp = boolExpression;
	boolExpr->operatorTypeUnary = LOGICAL_NOT;
  boolExpr->type = BOOL_UNARY;
	return boolExpr;
}

Statement * PrintStatementSemanticAction(PrintStatement * stmt){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_PRINT;
	statement->printStatement = stmt;
	return statement;
}

Factor * IdentifierFactorSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->identifier = identifier;
	factor->type = FACTOR_IDENTIFIER;
	return factor;
};
