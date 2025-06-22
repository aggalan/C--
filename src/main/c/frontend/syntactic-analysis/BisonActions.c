#include "BisonActions.h"

#include "BisonParser.h"
#include "../../backend/domain-specific/Calculator.h"

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

Type * createTypeArray(Type type, const ArgumentDefList * parameters);

/* PUBLIC FUNCTIONS */

void PushScopeBison() {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	pushScope(currentCompilerState()->scopeStack);
	logDebugging(_logger, "Size: %d", currentCompilerState()->scopeStack->size );
	logDebugging(_logger, "Last scope: %d", currentCompilerState()->scopeStack->lastScope);

}
void PopScopeBison() {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    popScope(currentCompilerState()->scopeStack);
    logDebugging(_logger,"pop scope");
    logDebugging(_logger, "Size: %d", currentCompilerState()->scopeStack->size );
    logDebugging(_logger, "Last scope: %d", currentCompilerState()->scopeStack->lastScope);
}

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
MatchStatement *MatchSemanticAction(String id, CaseList *cases, Type type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MatchStatement *match = calloc(1, sizeof(MatchStatement));
	match->identifier = id;
	match->caseList = cases;
	match->type = type;
	return match;
}
PrintStatement * PrintIdentifierSemanticAction(StringExpression * semanticAction) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	PrintStatement * print = calloc(1, sizeof(PrintStatement));
	print->identifier = semanticAction;
	print->type = PRINT_IDENTIFIER;
	return print;
}
PrintStatement * PrintMathExpressionSemanticAction(MathExpression * mathExpression){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	PrintStatement * print = calloc(1, sizeof(PrintStatement));
	print->mathExpression = mathExpression;
	print->type = PRINT_MATH_EXPRESSION;
	return print;
}

BoolExpression * ConditionalExpressionSemanticAction(BoolExpression * conditionalExpression1, BoolExpression * conditionalExpression2, OperatorType type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolExpression * condition = calloc(1, sizeof(BoolExpression));
	condition->logicalExpression.expression1 = conditionalExpression1;
	condition->logicalExpression.expression2 = conditionalExpression2;
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

SortStatement * SortSemanticAction(String identifier, Order order) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	SortStatement * statement = calloc(1, sizeof(SortStatement));
	statement->identifier = identifier;
	statement->order = order;
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

MacroStatement * MacroSemanticAction(String identifier, StringList *args, MathExpression * body) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    MacroStatement * macro = calloc(1, sizeof(MacroStatement));
    macro->identifier = identifier;
    macro->parameters = args;
    macro->statement = body;
	Type * typeArray = createMacroTypeArray(_MACRO, args);
	addSymbol(currentCompilerState()->symbolTable, identifier, typeArray, args == NULL ? 1 : args->count + 1,MACRO_SYMBOL);
	free(typeArray);
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
    Symbol * symbol = findSymbol(currentCompilerState()->symbolTable, identifier);
    if (symbol == NULL) {
		logError(_logger, "Function %s not defined", identifier);
    	currentCompilerState()->hasError = true;
		return NULL;
	}
	if (symbol->isFunction == 0) {
		logError(_logger, "Identifier %s is not a function", identifier);
		currentCompilerState()->hasError = true;
		return NULL;
	}
    if (symbol->typeCount-1 != parameters->count) {
        logError(_logger, "Function %s has a parameter count mismatch: expected %d, got %d", identifier, symbol->typeCount - 1, parameters->count);
        currentCompilerState()->hasError = true;
    }
	ArgumentNode *argNode = parameters->arguments;
	for (int i = 1; i < symbol->typeCount && argNode != NULL; i++, argNode = argNode->next) {
		Type expected = symbol->types[i];
		Type actual = _VOID;

		switch (argNode->argument->type) {
			case ARGUMENT_MATH_EXPRESSION:
				actual = _INT;
				break;
			case ARGUMENT_STRING_EXPRESSION:
				actual = _STRING;
				break;
			case ARGUMENT_BOOL_EXPRESSION:
				actual = _BOOL;
				break;
			case ARGUMENT_INT_ARRAY_ID:
				actual = _INT_ARRAY;
				break;
			case ARGUMENT_STRING_ARRAY_ID:
				actual = _STRING_ARRAY;
				break;
			case ARGUMENT_BOOL_ARRAY_ID:
				actual = _BOOL_ARRAY;
				break;
			case ARGUMENT_UNARY_CHANGE_OPERATOR:
			     actual = _INT;
			     break;
			case ARGUMENT_ARRAY_ACCESS:
			actual = findSymbol(currentCompilerState()->symbolTable, argNode->argument->arrayAccess->identifier)->types[0];
				break;
			case ARGUMENT_FUNCTION_EXPRESSION:
			actual = findSymbol(currentCompilerState()->symbolTable, argNode->argument->functionExpression->identifier)->types[0];
			break;

}
		if (actual != expected) {
			currentCompilerState()->hasError = true;
			logError(_logger, "Function %s has a parameter type mismatch at position %d: expected %d, got %d WITH IDENTIFIER %s", identifier, i, expected, actual,argNode->argument->identifier);
		}
	}
	_logSyntacticAnalyzerAction(__FUNCTION__);
	FunctionStatement * function = calloc(1, sizeof(FunctionStatement));
	function->identifier = identifier;
	function->parameters = parameters;
    return function;
}

ReturnStatement * ReturnSemanticAction(Expression * expression){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Type actual = _VOID;
	switch(expression->type) {
	case MATH_EXPRESSION:
		actual = _INT;
		break;
	case STRING_EXPRESSION:
		actual = _STRING;
		break;
	case BOOLEAN_EXPRESSION:
		actual = _BOOL;
		break;
	}

	ReturnStatement * returnStatement = calloc(1, sizeof(ReturnStatement));
	returnStatement->expression = expression;
	returnStatement->type = RETURN_EXPRESSION;
    if (currentCompilerState()->symbolTable->currentFunction != NULL && currentCompilerState()->symbolTable->currentFunction->types[0] != actual) {
        logError(_logger, "Function %s has a return type mismatch: expected %d, got %d", currentCompilerState()->symbolTable->currentFunction->name, currentCompilerState()->symbolTable->currentFunction->types[0], actual);
        currentCompilerState()->hasError = true;
    }
	return returnStatement;
}

Type * createTypeArray(const Type type, const ArgumentDefList * parameters) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Type * typeArray;
	if (parameters == NULL) {
		typeArray = calloc(1, sizeof(Type));
		typeArray[0] = type;
		return typeArray;
	}
	typeArray = calloc(parameters->count + 1, sizeof(Type));
	typeArray[0] = type;
	ArgumentDefNode * parametersNode = parameters->arguments;
	int i=1;
	while (parametersNode != NULL) {
		typeArray[i]=parametersNode->argumentDef->type;
		parametersNode = parametersNode->next;
		i++;
	}
	return typeArray;
}

Type * createMacroTypeArray(const Type type, const StringList * parameters) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Type * typeArray;
	if (parameters == NULL) {
		typeArray = calloc(1, sizeof(Type));
		typeArray[0] = type;
		return typeArray;
	}
	typeArray = calloc(parameters->count + 1, sizeof(Type));
	typeArray[0] = type;
	StringNode * parametersNode = parameters->strings;
    logError(_logger, "parameterscount: %d", parameters->count);
	int i=1;
	while (parametersNode != NULL) {
        typeArray[i]= _INT;
		parametersNode = parametersNode->next;
		i++;
	}
	return typeArray;
}
void AddFunctionToSymbolTable(const Type type, String identifier, ArgumentDefList * parameters) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Type * typeArray = createTypeArray(type, parameters);
	addSymbol(currentCompilerState()->symbolTable, identifier, typeArray, parameters == NULL ? 1 : parameters->count + 1,FUNCTION_SYMBOL);
	free(typeArray);
}

void AddMacroToSymbolTable(String identifier, StringList * parameters) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
    Type * typeArray = createMacroTypeArray(_MACRO, parameters);
    addSymbol(currentCompilerState()->symbolTable, identifier, typeArray,parameters == NULL ? 1 : parameters->count + 1,MACRO_SYMBOL);
    StringNode * parametersNode = parameters->strings;
    while (parametersNode != NULL) {
        addSymbol(currentCompilerState()->symbolTable, parametersNode->string, (Type[]){_INT}, 1,VARIABLE_SYMBOL);
        parametersNode = parametersNode->next;
    }
	free(typeArray);

}


FunctionDefinition  * FunctionDefinitionSemanticAction(const Type type, String identifier, ArgumentDefList * parameters, StatementBlock * body){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	FunctionDefinition * functionDefinition = calloc(1, sizeof(FunctionDefinition));
	functionDefinition->identifier = identifier;
	functionDefinition->parameters = parameters;
	functionDefinition->type = type;
	functionDefinition->body = body;
	Type * typeArray = createTypeArray(type, parameters);
	if(parameters != NULL && type != _VOID) {
	ArgumentDefNode * arg =  parameters->arguments;
	logDebugging(_logger, "Function %s has return type %d", identifier, type);
	}
	free(typeArray);
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
    if (currentCompilerState()->symbolTable->currentFunction!=NULL &&  currentCompilerState()->symbolTable->currentFunction->types[0]!= _VOID){
        logError(_logger, "Function %s has a return type mismatch: expected %d, got %d", currentCompilerState()->symbolTable->currentFunction->name, currentCompilerState()->symbolTable->currentFunction->types[0], _VOID);
        currentCompilerState()->hasError = true;
    }
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
    newUnit->type = NEW_LINE_UNIT;
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
	addSymbol(currentCompilerState()->symbolTable, identifier, &type, 1,VARIABLE_SYMBOL);
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
IntList * AppendArrayListSemanticAction(IntList *list, const int integer) {
    _logSyntacticAnalyzerAction(__FUNCTION__);
    IntNode * node = calloc(1, sizeof(IntNode));
    node->integer = integer;
    list->last->next = node;
    list->last = node;
    list->count++;
    return list;
}

ArrayStatement * ArrayIntStatementSemanticAction(String identifier, IntList * elements) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayStatement * array = calloc(1, sizeof(ArrayStatement));
	array->identifier = identifier;
	array->elements = elements;
	array->type = INT_LIST;
	addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_INT_ARRAY}, 1,VARIABLE_SYMBOL); //TODO: checkear lo del _int_array
	return array;
}
ArrayStatement * ArrayStringStatementSemanticAction(String identifier, StringList * elements) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayStatement * array = calloc(1, sizeof(ArrayStatement));
	array->identifier = identifier;
	array->stringElements = elements;
	array->type = STRING_LIST;
	addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_STRING_ARRAY}, 1,VARIABLE_SYMBOL); //TODO: checkear lo del _string_array
	return array;
}
ArrayStatement * ArrayBoolStatementSemanticAction(String identifier, BoolList * elements) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayStatement * array = calloc(1, sizeof(ArrayStatement));
	array->identifier = identifier;
	array->boolElements = elements;
	array->type = BOOL_LIST;
	addSymbol(currentCompilerState()->symbolTable, identifier,(Type[]){_BOOL_ARRAY}, 1,VARIABLE_SYMBOL); //TODO: checkear lo del _bool_array
	return array;
}
ArrayStatement * ArrayDeclarationSemanticAction(String identifier, MathExpression * size, const Type type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayStatement * array = calloc(1, sizeof(ArrayStatement));
	array->identifier = identifier;
	array->mathExpression = size;
	if (type == _INT) {
		array->type = INT_SIZE;
		addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_INT_ARRAY}, 1,VARIABLE_SYMBOL);
	} else if (type == _STRING) {
		array->type = STRING_SIZE;
		addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_STRING_ARRAY}, 1,VARIABLE_SYMBOL);
	} else if (type == _BOOL) {
		array->type = BOOL_SIZE;
		addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_BOOL_ARRAY}, 1,VARIABLE_SYMBOL);
	}
	return array;
}
ArrayAssignment * AssignmentIntArraySemanticAction(ArrayAccess * arrayAccess, MathExpression * mathExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayAssignment * array = calloc(1, sizeof(ArrayAssignment));
	array->arrayAccess = arrayAccess;
	array->mathExpression = mathExpression;
	array->type = ARRAY_MATH_EXPRESSION;
	return array;
}
ArrayAssignment * AssignmentStringArraySemanticAction(ArrayAccess * arrayAccess, StringExpression * stringExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayAssignment * array = calloc(1, sizeof(ArrayAssignment));
	array->arrayAccess = arrayAccess;
	array->stringExpression = stringExpression;
	array->type = ARRAY_STRING_EXPRESSION;
	return array;
}
ArrayAssignment * AssignmentBoolArraySemanticAction(ArrayAccess * arrayAccess, BoolExpression * boolExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArrayAssignment * array = calloc(1, sizeof(ArrayAssignment));
	array->arrayAccess = arrayAccess;
	array->boolExpression = boolExpression;
	array->type = ARRAY_BOOL_EXPRESSION;
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
	addSymbol(currentCompilerState()->symbolTable, id, (Type[]){_INT}, 1,VARIABLE_SYMBOL); // TODO: Checkear tema de memoria
	return assignmentStatement;
}
AssignmentStatement * AssignmentStringExpressionSemanticAction(AssignmentStringStatement * assignmentStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->type = STRING_ASSIGNMENT;
	statement->stringAssignment = assignmentStatement;
	return statement;
}
AssignmentStringStatement * AssignmentStringSemanticAction(String id, StringExpression * expression) {
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
AssignmentStatement * AssignmentArrayStatementSemanticAction(ArrayStatement * assignmentStatement) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->type = ARRAY_STATEMENT;
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
	addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_BOOL}, 1,VARIABLE_SYMBOL); // TODO: Checkear tema de memoria

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
	addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_INT}, 1,VARIABLE_SYMBOL); // TODO: Checkear tema de memoria
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
	addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){_STRING}, 1,VARIABLE_SYMBOL); // TODO: Checkear tema de memoria
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
    list->count = 1;
	list->last = list->arguments;
	return list;
}
ArgumentList * AppendArgumentListSemanticAction(ArgumentList * list, ArgumentValue * argumentValue) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentNode * node = calloc(1, sizeof(ArgumentNode));
	node->argument = argumentValue;
	list->last->next = node;
	list->last = node;
    list->count++;
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
	list->arguments->next = NULL;
	list->count=1;
	list->last = list->arguments;
	return list;
}
ArgumentDefList * AppendArgumentDefListSemanticAction(ArgumentDefList * list,ArgumentDef * argumentDef) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentDefNode * node = calloc(1, sizeof(ArgumentDefNode));
	node->argumentDef = argumentDef;
	node->next= NULL;
	list->last->next = node;
	list->count++;
	list->last = node;
	return list;
}
ArgumentDef * ArgumentDefSemanticAction(String identifier, Type type) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ArgumentDef * argumentValue = calloc(1, sizeof(ArgumentDef));
	argumentValue->identifier = identifier;
	argumentValue->type = type;
	addSymbol(currentCompilerState()->symbolTable, identifier, (Type[]){type}, 1,VARIABLE_SYMBOL);
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

Statement * MacroInvocationStatementSemanticAction(MacroInvocationStatement * macroInvocationStatement){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Statement * statement = calloc(1, sizeof(Statement));
	statement->type = STATEMENT_MACRO_INVOCATION;
	statement->macroInvocationStatement = macroInvocationStatement;
	return statement;


}
MacroInvocationStatement * MacroInvocationSemanticAction(String identifier, ArgumentList * args){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	MacroInvocationStatement * macroInvocationStatement = calloc(1, sizeof(MacroInvocationStatement));
	macroInvocationStatement->identifier = identifier;
	macroInvocationStatement->arguments = args;
	ArgumentNode * node = args->arguments;
    Symbol * symbol = findSymbol(currentCompilerState()->symbolTable, identifier);
    int size=symbol->typeCount-1;
	while(node != NULL){
		if(node->argument->type != ARGUMENT_MATH_EXPRESSION &&
		   node->argument->type != ARGUMENT_UNARY_CHANGE_OPERATOR  ) {
		   if(node->argument->type == ARGUMENT_ARRAY_ACCESS && findSymbol(currentCompilerState()->symbolTable, node->argument->arrayAccess->identifier)->types[0] != _INT) {
		   		currentCompilerState()->hasError = true;
               logError(_logger,"error invocation macro");
		   }else if (node->argument->type == ARGUMENT_FUNCTION_EXPRESSION && findSymbol(currentCompilerState()->symbolTable, node->argument->functionExpression->identifier)->types[0] != _INT) {
		   		currentCompilerState()->hasError = true;
               logError(_logger,"error invocation macro");

           }else{
			logError(_logger, "Invalid argument type in macro invocation: %d", node->argument->type);
			currentCompilerState()->hasError = true;
               logError(_logger,"error invocation macro");
           }
	}
        size--;
    node = node->next;
    }
    if (size!= 0) {
        currentCompilerState()->hasError = true;
        logError(_logger, "Macro %s wrong amount of parameters", identifier);
    }
	return macroInvocationStatement;
}


Factor * MacroInvocationFactorSemanticAction(MacroInvocationStatement * macroInvocationStatement){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	Factor * factor = calloc(1, sizeof(Factor));
	factor->macroInvocationStatement = macroInvocationStatement;
	factor->type = MACRO_INVOCATION;
	return factor;
}
BoolList * SingleBoolArrayListSemanticAction(Bool boolean) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolList* list = calloc(1, sizeof(BoolList));
	list->booleans = calloc(1, sizeof(BoolNode ));
	list->booleans->boolean = boolean;
	list->last = list->booleans;
	list->count = 1;
	return list;
}
BoolList * AppendBoolArrayListSemanticAction(BoolList *list, Bool boolean) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	BoolNode * node = calloc(1, sizeof(BoolNode));
	node->boolean = boolean;
	list->last->next = node;
	list->last = node;
	list->count++;
	return list;
}
AssignmentStatement * AssignmentArrayExpressionSemanticAction(ArrayAssignment * assignmentExpression) {
	_logSyntacticAnalyzerAction(__FUNCTION__);
	AssignmentStatement * statement = calloc(1, sizeof(AssignmentStatement));
	statement->type = ARRAY_ASSIGNMENT;
	statement->arrayAssignmentExpression = assignmentExpression;
	return statement;
}

ExternalDeclaration * MacroExternalDeclarationSemanticAction(MacroStatement * macroStatement){
	_logSyntacticAnalyzerAction(__FUNCTION__);
	ExternalDeclaration * externalDeclaration = calloc(1, sizeof(ExternalDeclaration));
	externalDeclaration->macroStatement = macroStatement;
	externalDeclaration->type = MACRO_STATEMENT;
	return externalDeclaration;
}
void DeclarationMode() {
	currentCompilerState()->declarationMode = true;
}

// ConditionalExpression *MathConditionalExpressionSemanticAction(MathExpression *math_expression) {
// 	_logSyntacticAnalyzerAction(__FUNCTION__);
// 	ConditionalExpression * condition = calloc(1, sizeof(ConditionalExpression));
// 	condition->math_expression = math_expression;
// 	condition->type= MATH_EXPRESSION;
// 	return condition;
// }