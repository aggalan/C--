#include "BisonActions.h"

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

Expression * ArithmeticExpressionSemanticAction(Expression * leftExpression, Expression * rightExpression, ExpressionType type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * expression = calloc(1, sizeof(Expression));
	expression->leftExpression = leftExpression;
	expression->rightExpression = rightExpression;
	expression->type = type;
	return expression;
}

Expression * FactorExpressionSemanticAction(Factor * factor) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * expression = calloc(1, sizeof(Expression));
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

Factor * ExpressionFactorSemanticAction(Expression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->expression = expression;
	factor->type = EXPRESSION;
	return factor;
}

Program * ExpressionProgramSemanticAction(CompilerState * compilerState, Expression * expression) {
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



// StatementList* SingleStatementListSemanticAction(Statement* stmt);
// StatementList* AppendStatementListSemanticAction(StatementList* list, Statement* stmt);
// Statement* ExpressionStatementSemanticAction(MathExpression* expr);
// Statement* ForLoopStatementSemanticAction(ForLoop* loop);
// Statement* MatchStatementSemanticAction(MatchStatement* stmt);
// MatchStatement* MatchStatementSemanticAction(String id, CaseList* cases);
// CaseList* SingleCaseListSemanticAction(Case* c);
// CaseList* AppendCaseListSemanticAction(CaseList* list, Case* c);
// Case* MatchCaseSemanticAction(int value, StatementList* body);
// ForLoop* ForLoopSemanticAction(String id, AssignmentMathExpression* start, MathExpression* end, StatementList* body);

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

StatementList* AppendStatementListSemanticAction(StatementList* list, Statement* stmt) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	list->statements = realloc(list->statements, sizeof(Statement*) * (list->count + 1));
	list->statements[list->count++] = stmt;
	return list;
}

Statement* ExpressionStatementSemanticAction(MathExpression* expr) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement* stmt = calloc(1, sizeof(Statement));
	stmt->type = STATEMENT_MathExpression;
	stmt->MathExpression = expr;
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

MatchStatement* MatchStatementSemanticAction(String id, CaseList* cases) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MatchStatement* match = calloc(1, sizeof(MatchStatement));
	match->identifier = id;
	match->caseList = cases;
	return match;
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


