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

MathExpression * ArithmeticExpressionSemanticAction(MathExpression * leftExpression, MathExpression * rightExpression, MathExpressionType type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MathExpression * expression = calloc(1, sizeof(MathExpression));
	expression->leftExpression = leftExpression;
	expression->rightExpression = rightExpression;
	expression->mathType = type;
	expression->type = OPERATOR_EXPRESSION;
	return expression;
}

MathExpression * FactorExpressionSemanticAction(Factor * factor) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MathExpression * expression = calloc(1, sizeof(MathExpression));
	expression->factor = factor;
	expression->type = FACTOR_EXPRESSION;
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

CaseList* AppendCaseListSemanticAction(Case* c,CaseList* list) {
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

ForLoop* ForLoopSemanticAction(AssignmentMathStatement * assignment, Constant * end, StatementBlock* body) {
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
WhileLoop *WhileLoopSemanticAction(BoolExpression *condition, StatementBlock *body) {
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
IfStatement *IfThenSemanticAction(BoolExpression *condition, StatementBlock *thenBranch, ElseStatement *elseBranch) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	IfStatement *stmt = calloc(1, sizeof(IfStatement));
	stmt->condition = condition;
	stmt->thenBranch = thenBranch;
    stmt->elseBranch = elseBranch;
	return stmt;
}
IfStatement *IfElseSemanticAction(BoolExpression *condition, StatementBlock *thenBranch, ElseStatement *elseBranch) {
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
BoolExpression * ConditionalExpressionSemanticAction(BoolExpression * conditionalExpression1, BoolExpression * conditionalExpression2, OperatorType type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolExpression * condition = calloc(1, sizeof(BoolExpression));
	condition->logicalExpression.expression1 = conditionalExpression1;
	condition->logicalExpression.expression1 = conditionalExpression2;
	condition->logicalExpression.operatorType = type;
	condition->type= LOGICAL_EXPRESSION;
	return condition;
}

BoolExpression * BooleanSemanticAction(MathExpression * mathExpression1,MathExpression * mathExpression2, ComparatorType type ) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolExpression * condition = calloc(1, sizeof(BoolExpression));
	condition->type = COMPARATOR_EXPRESSION;
	condition->comparatorExpression.expression1 = mathExpression1;
	condition->comparatorExpression.expression2 = mathExpression2;
	condition->comparatorExpression.comparatorType = type;
	return condition;
}


BoolFactor * NotExpressionSemanticAction(BoolExpression * conditionalExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolFactor * factor = calloc(1, sizeof(BoolFactor));
	factor->expression = conditionalExpression;
	factor->type = NOT_EXPRESSION;
	return factor;
}

Statement * PrintStatementSemanticAction(PrintStatement * stmt){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_PRINT;
	statement->printStatement = stmt;
	return statement;
}


Statement * SortStatementSemanticAction(SortStatement * sortStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type= STATEMENT_SORT;
	statement->sortStatement = sortStatement;
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

Statement *  AssignmentStatementSemanticAction(AssignmentStatement * assignmentStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_ASSIGNMENT;
	statement->assignmentStatement = assignmentStatement;
	return statement;
}

AssignmentStringExpression * assignmentStringExpressionSemanticAction(String id, String statement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStringExpression * assignment_expression = calloc(1, sizeof(AssignmentStringExpression));
	assignment_expression->identifier = id;
	assignment_expression->expression = statement;
	return assignment_expression;
}

StringExpression * FactorStringExpressionSemanticAction(String string) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StringExpression * expression = calloc(1, sizeof(StringExpression));
	expression->string = string;
	expression->type = STRING_VALUE_EXPRESSION;
	return expression;
}
Factor * ParenthesisFactorSemanticAction(MathExpression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->expression = expression;
	factor->type = EXPRESSION;
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

FunctionStatement * FunctionSemanticAction(String identifier, ArgumentList * parameters) {
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
	returnStatement->type = RETURN_EXPRESSION;
    return returnStatement;

}



FunctionDefinition  * FunctionDefinitionSemanticAction(Type type, String identifier, ArgumentDefList * parameters, StatementBlock * body){
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
	newUnit->externalDeclaration = externalDeclaration;
	newUnit->units = unit;  // Link to previous units
	newUnit->type = NODE;
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


ElseStatement* ElseStatementSemanticAction(StatementBlock * stml){
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
UnaryChangeOperatorStatement * UnaryChangeArraySemanticAction(ArrayAccess * arrayAccess, int type) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    UnaryChangeOperatorStatement * statement = calloc(1, sizeof(UnaryChangeOperatorStatement));
    statement->arrayAccess = arrayAccess;
    statement->operator_type = type;
	statement->type= ARRAY;
    return statement;
}
UnaryChangeOperatorStatement * UnaryChangeOperatorSemanticAction(String identifier, int type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	UnaryChangeOperatorStatement * statement = calloc(1, sizeof(UnaryChangeOperatorStatement));
	statement->identifier = identifier;
	statement->operator_type = type;
	statement->type = VARIABLE;
	return statement;
}
Factor * UnitIncrementOperatorFactorSemanticAction(UnaryChangeOperatorStatement * statement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->type= UNARY_CHANGE_FACTOR;
	factor->unaryChangeOperatorStatement = statement;
	return factor;
}

ReturnStatement * ReturnEmptySemanticAction() {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ReturnStatement * returnStatement = calloc(1, sizeof(ReturnStatement));
	returnStatement->type = RETURN_EMPTY;
	return returnStatement;
}
Factor * FunctionCallFactorSemanticAction(FunctionStatement * functionStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->functionStatement = functionStatement;
	factor->type = FUNCTION;
	return factor;
}
StatementBlock * StatementBlockSemanticAction(StatementList * statementList) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StatementBlock * statementBlock = calloc(1, sizeof(StatementBlock));
	statementBlock->statementList = statementList;
	return statementBlock;
}
Unit * NewLineUnitSemanticAction(Unit * unit) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Unit * newUnit = calloc(1, sizeof(Unit));
	newUnit->units = unit;
    unit->type = NEW_LINE_UNIT;
    return newUnit;
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
    return list;
}
StringList * AppendStringListSemanticAction(StringList *list, String str) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
    StringNode * node = calloc(1, sizeof(StringNode));
    node->string = str;
    list->last->next = node;
    list->last = node;
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

Case * MatchCaseStringSemanticAction(String str, Statement *body) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    Case * matchCase = calloc(1, sizeof(Case));
    matchCase->type = STRING_CASE;
    matchCase->body = body;
    matchCase->string = str;
    return matchCase;
}
Factor * ArrayFactorSemanticAction(ArrayAccess * arrayAccess) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->arrayAccess = arrayAccess;
	factor->type = ARRAY_FACTOR;
	return factor;
}
ArrayAccess * ArrayAccessSemanticAction(String identifier, MathExpression * index) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayAccess * arrayAccess = calloc(1, sizeof(ArrayAccess));
	arrayAccess->identifier = identifier;
	arrayAccess->index = index;
	return arrayAccess;
}
Expression * MathExpressionSemanticAction(MathExpression * mathExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * expression = calloc(1, sizeof(Expression));
	expression->mathExpression = mathExpression;
	expression->type = MATH_EXPRESSION;
	return expression;
}
Expression * BooleanExpressionSemanticAction(BoolExpression * boolExpression) {
	Expression * expression = calloc(1, sizeof(Expression));
	_logSyntacticAnalyzerAction(__FUNCTION__);
	expression->boolExpression = boolExpression;
	expression->type = BOOLEAN_EXPRESSION;
	return expression;
}
AssignmentStatement * AssignmentIntExpressionSemanticAction(AssignmentMathStatement * assignmentStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->type = MATH_ASSIGNMENT;
	statement->mathAssignment = assignmentStatement;
	return statement;
}
AssignmentMathStatement * AssignmentIntSemanticAction(String id, MathExpression * mathExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentMathStatement * assignmentStatement = calloc(1, sizeof(AssignmentMathStatement));
	assignmentStatement->identifier = id;
	assignmentStatement->mathExpression = mathExpression;
	return assignmentStatement;
}
AssignmentStatement * AssignmentStringExpressionSemanticAction(AssignmentStringStatement * assignmentStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->type = STRING_ASSIGNMENT;
	statement->stringAssignment = assignmentStatement;
	return statement;
}
AssignmentStringStatement * AssignmentStringSemanticAction(String id, String expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStringStatement * assignmentStatement = calloc(1, sizeof(AssignmentStringStatement));
	assignmentStatement->identifier = id;
	assignmentStatement->expression = expression;
	return assignmentStatement;
}
AssignmentStatement * AssignmentBoolExpressionSemanticAction(AssignmentBoolStatement * assignmentStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->type = BOOL_ASSIGNMENT;
	statement->boolAssignment = assignmentStatement;
	return statement;
}
AssignmentBoolStatement * AssignmentBoolSemanticAction(String id, BoolExpression * boolExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentBoolStatement * assignmentStatement = calloc(1, sizeof(AssignmentBoolStatement));
	assignmentStatement->identifier = id;
	assignmentStatement->expression = boolExpression;
	return assignmentStatement;
}
AssignmentStatement * AssignmentArrayExpressionSemanticAction(ArrayStatement * assignmentStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->type = ARRAY_ASSIGNMENT;
	statement->arrayAssignment = assignmentStatement;
	return statement;
}
VariableStatement * VariableBoolDeclarationSemanticAction(String identifier, BoolExpression * value) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VariableStatement * variable = calloc(1, sizeof(VariableStatement));
	variable->identifier = identifier;
	variable->type = _BOOL;
	variable->expression = calloc(1, sizeof(Expression));
	variable->expression->boolExpression = value;
	variable->expression->type = BOOLEAN_EXPRESSION;
	return variable;
}
VariableStatement * VariableIntDeclarationSemanticAction(String identifier, MathExpression * value) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VariableStatement * variable = calloc(1, sizeof(VariableStatement));
	variable->identifier = identifier;
	variable->type = _INT;
	variable->expression = calloc(1, sizeof(Expression));
	variable->expression->mathExpression = value;
	variable->expression->type = MATH_EXPRESSION;
	return variable;
}
VariableStatement * VariableStringDeclarationSemanticAction(String identifier, StringExpression * value) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	VariableStatement * variable = calloc(1, sizeof(VariableStatement));
	variable->identifier = identifier;
	variable->type = _STRING;
	variable->expression = calloc(1, sizeof(Expression));
	variable->expression->stringExpression = value;
	variable->expression->type = STRING_EXPRESSION;
	return variable;
}
BoolFactor * FunctionCallBoolFactorSemanticAction(FunctionStatement * functionStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolFactor * factor = calloc(1, sizeof(BoolFactor));
	factor->functionStatement = functionStatement;
	factor->type = BOOL_FUNCTION;
	return factor;
}
BoolFactor * ArrayBoolFactorSemanticAction(ArrayAccess * arrayAccess) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolFactor * factor = calloc(1, sizeof(BoolFactor));
	factor->arrayAccess = arrayAccess;
	factor->type = BOOL_ARRAY;
	return factor;
}
BoolFactor * ParenthesisExpressionSemanticAction(BoolExpression * conditionalExpression1) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolFactor * factor = calloc(1, sizeof(BoolFactor));
	factor->expression = conditionalExpression1;
	factor->type = PARENTHESIS_EXPRESSION;
	return factor;
}
BoolExpression * BoolFactorExpressionSemanticAction(BoolFactor * boolFactor) {
	BoolExpression * expression = calloc(1, sizeof(BoolExpression));
	_logSyntacticAnalyzerAction(__FUNCTION__);
	expression->boolFactor = boolFactor;
	expression->type = BOOL_FACTOR;
	return expression;
}
BoolFactor * ParenthesizedExpressionSemanticAction(BoolExpression * conditionalExpression1) {
	BoolFactor * factor = calloc(1, sizeof(BoolFactor));
	_logSyntacticAnalyzerAction(__FUNCTION__);
	factor->expression = conditionalExpression1;
	factor->type = PARENTHESIS_EXPRESSION;
	return factor;
}
BoolFactor * BooleanConstantSemanticAction(Bool value) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolFactor * factor = calloc(1, sizeof(BoolFactor));
	factor->boolean = value;
	factor->type = BOOL_CONSTANT;
	return factor;
}
BoolFactor * IdentifierBoolFactorSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolFactor * factor = calloc(1, sizeof(BoolFactor));
	factor->identifier = identifier;
	factor->type = BOOLEAN_ID;
	return factor;
}
StringExpression * ArrayStringAccessSemanticAction(ArrayAccess * arrayAccess) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StringExpression * expression = calloc(1, sizeof(StringExpression));
	expression->arrayAccess = arrayAccess;
	expression->type = STRING_EXPRESSION_ARRAY;
	return expression;
}
StringExpression * IdentifierStringExpressionSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StringExpression * expression = calloc(1, sizeof(StringExpression));
	expression->identifier = identifier;
	expression->type = STRING_IDENTIFIER_EXPRESSION;
	return expression;
}
ArgumentList * ArgumentValueSemanticAction(ArgumentValue * argumentValue) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentList * list = calloc(1, sizeof(ArgumentList));
	list->arguments = calloc(1, sizeof(ArgumentNode));
	list->arguments->argument = argumentValue;
	list->last = list->arguments;
	return list;
}
ArgumentList * AppendArgumentListSemanticAction(ArgumentList * list, ArgumentValue * argumentValue) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentNode * node = calloc(1, sizeof(ArgumentNode));
	node->argument = argumentValue;
	list->last->next = node;
	list->last = node;
	return list;
}

ArgumentValue * MathExpressionArgValueSemanticAction( MathExpression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentValue * argumentValue = calloc(1, sizeof(ArgumentValue));
	argumentValue->mathExpression = expression;
	argumentValue->type = ARGUMENT_MATH_EXPRESSION;
	return argumentValue;
}
ArgumentValue * StringExpressionArgValueSemanticAction(StringExpression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentValue * argumentValue = calloc(1, sizeof(ArgumentValue));
	argumentValue->stringExpression = expression;
	argumentValue->type = ARGUMENT_STRING_EXPRESSION;
	return argumentValue;
}
ArgumentValue * BoolExpressionArgValueSemanticAction( BoolExpression * expression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentValue * argumentValue = calloc(1, sizeof(ArgumentValue));
	argumentValue->boolExpression = expression;
	argumentValue->type = ARGUMENT_BOOL_EXPRESSION;
	return argumentValue;
}

ArgumentValue * FunctionExpressionArgValueSemanticAction( FunctionStatement * functionStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentValue * argumentValue = calloc(1, sizeof(ArgumentValue));
	argumentValue->functionExpression = functionStatement;
	argumentValue->type = ARGUMENT_FUNCTION_EXPRESSION;
	return argumentValue;
}
ArgumentValue * ArrayBoolSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentValue * argumentValue = calloc(1, sizeof(ArgumentValue));
	argumentValue->identifier = identifier;
	argumentValue->type = ARGUMENT_BOOL_ARRAY_ID;
	return argumentValue;
}
ArgumentValue * ArrayIntSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentValue * argumentValue = calloc(1, sizeof(ArgumentValue));
	argumentValue->identifier = identifier;
	argumentValue->type = ARGUMENT_INT_ARRAY_ID;
	return argumentValue;
}
ArgumentValue * ArrayStringSemanticAction(String identifier) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentValue * argumentValue = calloc(1, sizeof(ArgumentValue));
	argumentValue->identifier = identifier;
	argumentValue->type = ARGUMENT_STRING_ARRAY_ID;
	return argumentValue;
}
ArgumentDefList * SingleArgumentDefListSemanticAction(ArgumentDef * argumentDef) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentDefList * list = calloc(1, sizeof(ArgumentDefList));
	list->arguments = calloc(1, sizeof(ArgumentDefNode));
	list->arguments->argumentDef = argumentDef;
	list->last = list->arguments;
	return list;
}
ArgumentDefList * AppendArgumentDefListSemanticAction(ArgumentDefList * list,ArgumentDef * argumentDef) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentDefNode * node = calloc(1, sizeof(ArgumentDefNode));
	node->argumentDef = argumentDef;
	list->last->next = node;
	list->last = node;
	return list;
}
ArgumentDef * ArgumentDefSemanticAction(String identifier, Type type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentDef * argumentValue = calloc(1, sizeof(ArgumentDef));
	argumentValue->identifier = identifier;
	argumentValue->type = type;
	return argumentValue;
}
Expression * StringExpressionSemanticAction(StringExpression * stringExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Expression * expression = calloc(1, sizeof(Expression));
	expression->stringExpression = stringExpression;
	expression->type = STRING_EXPRESSION;
	return expression;
}

StringExpression * FunctionCallStringExpressionSemanticAction(FunctionStatement * functionStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	StringExpression * expression = calloc(1, sizeof(StringExpression));
	expression->functionStatement = functionStatement;
	expression->type = STRING_EXPRESSION_FUNCTION;
	return expression;
}

// ConditionalExpression *MathConditionalExpressionSemanticAction(MathExpression *math_expression) {
// 	_logSyntacticAnalyzerAction(__FUNCTION__);
// 	ConditionalExpression * condition = calloc(1, sizeof(ConditionalExpression));
// 	condition->math_expression = math_expression;
// 	condition->type= MATH_EXPRESSION;
// 	return condition;
// }