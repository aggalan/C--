#include "BisonActions.h"

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

Expression * ArithmeticExpressionSemanticAction(Expression * leftExpression, Expression * rightExpression, MathExpressionType type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * expression = calloc(1, sizeof(Expression));
	expression->math_expression.leftExpression = leftExpression;
	expression->math_expression.rightExpression = rightExpression;
	expression->math_expression.mathType = type;
	expression->type = MATH_EXPRESSION;
	return expression;
}

Expression * FactorExpressionSemanticAction(Factor * factor) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * expression = calloc(1, sizeof(Expression));
	expression->math_expression.factor = factor;
	expression->math_expression.mathType = FACTOR;
	expression->type = MATH_EXPRESSION;
	return expression;
}

Factor * ConstantFactorSemanticAction(Constant * constant) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->constant = constant;
	factor->type = CONSTANT;
	return factor;
}

StatementList* SingleStatementListSemanticAction(Statement* stmt) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    StatementList* list = calloc(1, sizeof(StatementList));
    list->statements = calloc(1, sizeof(StatementNode ));
    list->statements->statement = stmt;
    list->last = list->statements;
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
    StatementNode * node = calloc(1, sizeof(StatementNode));
    node->statement = stmt;
    list->last->next = node;
    list->last = node;
    list->count++;
    return list;
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
    list->cases = calloc(1, sizeof(CaseNode));
    list->cases->Case = c;
    list->last = list->cases;
    list->count = 1;
    return list;
}

CaseList* AppendCaseListSemanticAction(CaseList* list, Case* c) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
    CaseNode * node = calloc(1, sizeof(CaseNode));
    node->Case = c;
    list->last->next = node;
    list->last = node;
    list->count++;
    return list;
}

Case* MatchCaseSemanticAction(int value, Statement  * body) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Case *caseNode = calloc(1, sizeof(Case));
    caseNode->matchValue = value;
    caseNode->body = body;
    caseNode->type = INTEGER_CASE;
	return caseNode;
}

ForLoop* ForLoopSemanticAction(AssignmentStatement * assignment, Constant * end, StatementList* body) {
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
WhileLoop *WhileLoopSemanticAction(Expression *condition, StatementList *body) {
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
IfStatement *IfThenSemanticAction(Expression *condition, StatementList *thenBranch, ElseStatement *elseBranch) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IfStatement *stmt = calloc(1, sizeof(IfStatement));
	stmt->condition = condition;
	stmt->thenBranch = thenBranch;
    stmt->elseBranch = elseBranch;
	return stmt;
}
IfStatement *IfElseSemanticAction(Expression *condition, StatementList *thenBranch, ElseStatement *elseBranch) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IfStatement *stmt = calloc(1, sizeof(IfStatement));
	stmt->condition = condition;
	stmt->thenBranch = thenBranch;
	stmt->elseBranch = elseBranch;
	return stmt;
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
Expression * ConditionalExpressionSemanticAction(Expression * conditionalExpression1, Expression * conditionalExpression2, OperatorType type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * condition = calloc(1, sizeof(Expression));
	condition->conditional_expression.condition1 = conditionalExpression1;
	condition->conditional_expression.condition2 = conditionalExpression2;
	condition->conditional_expression.operatorType = type;
	condition->type= OPERATOR;
	return condition;
}
Expression * BooleanSemanticAction(Expression * math_expression1,Expression * math_expression2, ComparatorType type ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * condition = calloc(1, sizeof(Expression));
	condition->type = CONDITIONAL_EXPRESSION;
	condition->conditional_expression.bool_expression.math_expression1 = math_expression1;
	condition->conditional_expression.bool_expression.math_expression2 = math_expression2;
	condition->conditional_expression.bool_expression.comparatorType = type;
	condition->conditional_expression.type = BOOLEAN;
	return condition;
}


Expression * NotExpressionSemanticAction(Expression * conditionalExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * condition = calloc(1, sizeof(Expression));
	condition->conditional_expression.conditional_expression = conditionalExpression;
	condition->type = CONDITIONAL_EXPRESSION;
	condition->conditional_expression.type = CONDITIONAL_EXPRESSION;
	return condition;
}

Statement * PrintStatementSemanticAction(PrintStatement * stmt){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_PRINT;
	statement->printStatement = stmt;
	return statement;
}


Statement * SortStatementSemanticAction(SortStatement * sort_statement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type= STATEMENT_SORT;
	statement->sort_statement = sort_statement;
	return statement;
}

SortStatement * SortSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	SortStatement * statement = calloc(1, sizeof(SortStatement));
	statement->identifier = identifier;
	return statement;
}
Factor * IdentifierFactorSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->identifier = identifier;
	factor->type= FACTOR_IDENTIFIER;
	return factor;
}

Statement *  AssignmentStatementSemanticAction(AssignmentStatement * assignment_statement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_ASSIGNMENT;
	statement->assignment_statement = assignment_statement;
	return statement;
}

AssignmentStringExpression * assignmentStringExpressionSemanticAction(String id, String statement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStringExpression * assignment_expression = calloc(1, sizeof(AssignmentStringExpression));
	assignment_expression->identifier = id;
	assignment_expression->expression = statement;
	return assignment_expression;
}
AssignmentStatement * AssignmentExpressionSemanticAction(String id, Expression * assignment_expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->identifier = id;
	statement->expression = assignment_expression;
	return statement;
}

Expression * StringExpressionSemanticAction(String string) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * expression = calloc(1, sizeof(Expression));
	expression->string_expression = string;
	expression->type = STRING_EXPRESSION;
	return expression;
}
Factor * ParenthesisFactorSemanticAction(Expression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->expression = expression;
	factor->type = EXPRESSION;
	return factor;
}

Factor * BooleanFactorSemanticAction(Bool value) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->boolean = value;
	factor->type = BOOLEAN_FACTOR;
	return factor;
}
MacroStatement * MacroSemanticAction(String identifier, StringList *args, Statement * body) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    MacroStatement * macro = calloc(1, sizeof(MacroStatement));
    macro->identifier = identifier;
    macro->parameters = args;
    macro->statement = body;
    return macro;
}

Statement * MacroStatementSemanticAction(MacroStatement * stmt) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Statement * statement = calloc(1, sizeof(Statement));
    statement->type = STATEMENT_MACRO;
    statement->macroStatement = stmt;
    return statement;
}

FunctionStatement * FunctionSemanticAction(String identifier, StringList * parameters) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    FunctionStatement * function = calloc(1, sizeof(FunctionStatement));
    function->identifier = identifier;
    function->parameters = parameters;
    return function;
}

ReturnStatement * ReturnSemanticAction(Expression * expression){
    _logSyntacticAnalyzerAction(__FUNCTION__);
    ReturnStatement * returnStatement = calloc(1, sizeof(ReturnStatement));
    returnStatement->expression = expression;
    return returnStatement;

}
ReturnStatement * ReturnFunctionStatementSemanticAction(FunctionStatement * functionStatement){
    _logSyntacticAnalyzerAction(__FUNCTION__);
    ReturnStatement * returnStatement = calloc(1, sizeof(ReturnStatement));
    returnStatement->expression = NULL;
    returnStatement->functionStatement = functionStatement;
    return returnStatement;

}



FunctionDefinition  * FunctionDefinitionSemanticAction(Type type, String identifier, StringList * parameters, StatementList * body){
    _logSyntacticAnalyzerAction(__FUNCTION__);
    FunctionDefinition * functionDefinition = calloc(1, sizeof(FunctionDefinition));
    functionDefinition->identifier = identifier;
    functionDefinition->parameters = parameters;
	functionDefinition->type = type;
    functionDefinition->body = body;
    return functionDefinition;
}
Statement * ReturnStatementSemanticAction(ReturnStatement * stmt){
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Statement * statement = calloc(1, sizeof(Statement));
    statement->type = STATEMENT_RETURN;
    statement->returnStatement = stmt;
    return statement;
}
Statement * FunctionStatementSemanticAction(FunctionStatement * stmt){
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Statement * statement = calloc(1, sizeof(Statement));
    statement->type = STATEMENT_FUNCTION;
    statement->functionStatement = stmt;
    return statement;

}
Unit * SingleExternalDeclarationSemanticAction(ExternalDeclaration * externalDeclaration) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Unit * unit = calloc(1, sizeof(Unit));
	unit->externalDeclaration = externalDeclaration;
	unit->type = SINGLE;
	return unit;
}
Unit * AppendExternalDeclarationSemanticAction(Unit * unit, ExternalDeclaration * externalDeclaration) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Unit * newUnit = calloc(1, sizeof(Unit));
	unit->externalDeclaration = externalDeclaration;
	unit->units = unit;
	unit->type = NODE;
	return newUnit;
}
ExternalDeclaration * FunctionDefinitionExternalDeclarationSemanticAction(FunctionDefinition * functionDefinition) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ExternalDeclaration * externalDeclaration = calloc(1, sizeof(ExternalDeclaration));
	externalDeclaration->functionDefinition = functionDefinition;
	externalDeclaration->type = FUNCTION_DEFINITION;
	return externalDeclaration;
}
ExternalDeclaration * StatementExternalDeclarationSemanticAction(Statement * statement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ExternalDeclaration * externalDeclaration = calloc(1, sizeof(ExternalDeclaration));
	externalDeclaration->statement = statement;
	externalDeclaration->type = STATEMENT;
	return externalDeclaration;
}
Program * ProgramSemanticAction(CompilerState * compilerState, Unit * unit) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Program * program = calloc(1, sizeof(Program));
	program->unit = unit;
	program->type = NOT_EMPTY;
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

Program * EmptyProgramSemanticAction(CompilerState * compilerState) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Program * program = calloc(1, sizeof(Program));
	compilerState->abstractSyntaxtTree = program;
	program->unit = NULL;
	program->type = EMPTY;
	if (0 < flexCurrentContext()) {
		logError(_logger, "The final context is not the default (0): %d", flexCurrentContext());
		compilerState->succeed = false;
	}
	else {
		compilerState->succeed = true;
	}
	return program;
}


ElseStatement* ElseStatementSemanticAction(StatementList * stml){
    _logSyntacticAnalyzerAction(__FUNCTION__);
    ElseStatement * elseStatement = calloc(1, sizeof(ElseStatement));
    elseStatement->body = stml;
    elseStatement->type = ELSE_STATEMENT;
    return elseStatement;
}
ElseStatement * ElseIfStatementSemanticAction(IfStatement * ifStatement){
     _logSyntacticAnalyzerAction(__FUNCTION__);
     ElseStatement * elseStatement = calloc(1, sizeof(ElseStatement));
     elseStatement->elseIfStatement = ifStatement;
     elseStatement->type = ELSE_IF_STATEMENT;
     return elseStatement;
}
Statement * UnaryChangeOperatorStatementSemanticAction(UnaryChangeOperatorStatement * stmt) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_UNARY_CHANGE_OPERATOR;
	statement->unaryChangeOperatorStatement = stmt;
	return statement;
}
UnaryChangeOperatorStatement * UnaryChangeOperatorSemanticAction(String identifier, int type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryChangeOperatorStatement * statement = calloc(1, sizeof(UnaryChangeOperatorStatement));
	statement->identifier = identifier;
	statement->type = type;
	return statement;
}

Statement * VariableStatementSemanticAction(VariableStatement * var) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_VARIABLE;
	statement->variableStatement = var;
	return statement;
}


VariableStatement * VariableDeclarationSemanticAction(Type type, String identifier, Expression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VariableStatement * variable = calloc(1, sizeof(VariableStatement));
	variable->identifier = identifier;
	variable->type = type;
	variable->expression = expression;
	return variable;
}

StringList * SingleStringListSemanticAction(String str) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
    StringList * list = calloc(1, sizeof(StringList));
    list->strings = calloc(1, sizeof(StringNode));
    list->strings->string = str;
    list->last = list->strings;
    list->count = 1;
    return list;
}
StringList * AppendStringListSemanticAction(StringList *list, String str) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
    StringNode * node = calloc(1, sizeof(StringNode));
    node->string = str;
    list->last->next = node;
    list->last = node;
    list->count++;
    return list;
}

IntList * SingleArrayListSemanticAction(int integer) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IntList * list = calloc(1, sizeof(IntList));
	list->integers = calloc(1, sizeof(IntNode));
	list->integers->integer = integer;
    list->last = list->integers;
	list->count = 1;
	return list;
}
IntList * AppendArrayListSemanticAction(IntList *list, int integer) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    IntNode * node = calloc(1, sizeof(IntNode));
    node->integer = integer;
    list->last->next = node;
    list->last = node;
    list->count++;
    return list;
}

Statement * ArrayStatementSemanticAction(ArrayStatement * array) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_ARRAY;
	statement->arrayStatement = array;
	return statement;
}
ArrayStatement * ArraySemanticAction(String identifier, IntList * elements) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayStatement * array = calloc(1, sizeof(ArrayStatement));
	array->identifier = identifier;
	array->elements = elements;
	return array;
}

Case * MatchDefaultCaseSemanticAction(Statement * body) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Case * matchDefault = calloc(1, sizeof(Case));
    matchDefault->type = DEFAULT_CASE;
    matchDefault->body = body;
    return matchDefault;
}

ReturnStatement * ReturnEmptySemanticAction() {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    ReturnStatement * returnStatement = calloc(1, sizeof(ReturnStatement));
    returnStatement->expression = NULL;
    return returnStatement;
}
Case * MatchCaseStringSemanticAction(String str, Statement *body) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Case * matchCase = calloc(1, sizeof(Case));
    matchCase->type = STRING_CASE;
    matchCase->body = body;
    matchCase->string = str;
    return matchCase;
}

// ConditionalExpression *MathConditionalExpressionSemanticAction(MathExpression *math_expression) {
// 	_logSyntacticAnalyzerAction(__FUNCTION__);
// 	ConditionalExpression * condition = calloc(1, sizeof(ConditionalExpression));
// 	condition->math_expression = math_expression;
// 	condition->type= MATH_EXPRESSION;
// 	return condition;
// }