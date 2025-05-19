#include "AbstractSyntaxTree.h"

/* MODULE INTERNAL STATE */

static Logger * _logger = NULL;

void initializeAbstractSyntaxTreeModule() {
	_logger = createLogger("AbstractSyntxTree");
}

void shutdownAbstractSyntaxTreeModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
}

/** PUBLIC FUNCTIONS */


void releaseExpression(Expression * expression) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (expression != NULL) {
        switch (expression->type) {
            case MATH_EXPRESSION:
                releaseMathExpression(expression->math_expression);
                break;
            case BOOLEAN_EXPRESSION:
                releaseBoolExpression(expression->bool_expression);
                break;
            case STRING_EXPRESSION:
                break;
            case STRING_ARRAY_EXPRESSION:
                releaseArrayAccess(expression->array_access);
                break;
        }
        free(expression);
    }
}





void releaseFactor(Factor * factor) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (factor != NULL) {
		switch (factor->type) {
			case CONSTANT:
				releaseConstant(factor->constant);
				break;
			case EXPRESSION:
                releaseMathExpression(factor->expression);
				break;
            case FACTOR_IDENTIFIER:
                break;
            case BOOLEAN_FACTOR:
                break;
            case FUNCTION:
                releaseFunctionStatement(factor->functionStatement);
                break;
            case ARRAY_FACTOR:
                releaseArrayAccess(factor->arrayAccess);
                break;
            case UNARY_CHANGE_FACTOR:
                releaseUnaryChangeOperatorStatement(factor->unaryChangeOperatorStatement);
                break;
		}
		free(factor);
	}
}

void releaseProgram(Program * program) {
	logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
	if (program != NULL) {
        switch (program->type) {
            case EMPTY:
                break;
            case NOT_EMPTY:
                releaseUnit(program->unit);
        }
        free(program);
	}
}

void releaseConstant(Constant * constant){
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (constant != NULL) {
        free(constant);
    }
}
void releaseStatement(Statement *stmt){
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (stmt != NULL) {
        switch (stmt->type) {
            case STATEMENT_MATH_EXPRESSION:
                releaseExpression(stmt->mathExpression);
                break;
            case STATEMENT_FOR:
                releaseForLoop(stmt->forLoop);
                break;
            case STATEMENT_MATCH:
                releaseMatchStatement(stmt->matchStatement);
                break;
            case STATEMENT_WHILE:
                releaseWhileLoop(stmt->whileLoop);
                break;
            case STATEMENT_IF:
                releaseIfStatement(stmt->ifStatement);
                break;
            case STATEMENT_PRINT:
                releasePrintStatement(stmt->printStatement);
                break;
            case STATEMENT_SORT:
                releaseSortStatement(stmt->sort_statement);
                break;
            case STATEMENT_MACRO:
                releaseMacroStatement(stmt->macroStatement);
                break;
            case STATEMENT_FUNCTION:
                releaseFunctionStatement(stmt->functionStatement);
                break;
            case STATEMENT_RETURN:
                releaseReturnStatement(stmt->returnStatement);
                break;
            case STATEMENT_ASSIGNMENT:
                releaseAssignmentStatement(stmt->assignment_statement);
                break;
            case STATEMENT_UNARY_CHANGE_OPERATOR:
                releaseUnaryChangeOperatorStatement(stmt->unaryChangeOperatorStatement);
                break;
            case STATEMENT_VARIABLE:
                releaseVariableStatement(stmt->variableStatement);
                break;
            case STATEMENT_ARRAY:
                releaseArrayStatement(stmt->arrayStatement);
                break;
        }
        free(stmt);
    }

}


void releaseStatementList(StatementList *list){
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (list != NULL) {
        StatementNode *current = list->statements;
        StatementNode *next;
        while (current != NULL) {
            next = current->next;
            releaseStatementNode(current);
            current = next;
        }
        free(list);
    }
}
void releaseForLoop(ForLoop *loop){
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (loop != NULL) {
        releaseAssignmentMathStatement(loop->assignment);
        releaseConstant(loop->endValue);
        releaseStatementBlock(loop->body);
        free(loop);
    }
}

void releaseMatchStatement(MatchStatement *match){
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (match != NULL) {
        releaseCaseList(match->caseList);
        free(match);
    }
}

void releaseCase(Case *c) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (c) {
        releaseStatement(c->body);
        free(c);
    }
}

void releaseCaseList(CaseList * caseList) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!caseList) return;
    releaseCaseNode(caseList->cases);
    free(caseList);
}

void releaseCaseNode(CaseNode *node) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    while (node) {
        CaseNode *next = node->next;
        releaseCase(node->Case);
        free(node);
        node = next;
    }
}

void releaseMathExpression(MathExpression *expr) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!expr) return;
    if (expr->type == FACTOR) {
        releaseFactor(expr->factor);
    } else {
        releaseMathExpression(expr->leftExpression);
        releaseMathExpression(expr->rightExpression);
    }
    free(expr);
}
void releaseIfStatement(IfStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseBoolExpression(stmt->condition);
    releaseStatementBlock(stmt->thenBranch);
    releaseElseStatement(stmt->elseBranch);
    free(stmt);
}

void releaseElseStatement(ElseStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseStatementBlock(stmt->body);
    if (stmt->type == ELSE_IF_STATEMENT)
        releaseIfStatement(stmt->elseIfStatement);
    free(stmt);
}

void releaseWhileLoop(WhileLoop *loop) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!loop) return;
    releaseBoolExpression(loop->condition);
    releaseStatementBlock(loop->body);
    free(loop);
}
void releasePrintStatement(PrintStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (stmt) {
        free(stmt->identifier);
        free(stmt);

    }
}
void releaseSortStatement(SortStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (stmt != NULL) free(stmt);
}

void releaseMacroStatement(MacroStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseStringList(stmt->parameters);
    releaseStatement(stmt->statement);
    free(stmt);
}

void releaseFunctionStatement(FunctionStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseStringList(stmt->parameters);
    free(stmt);
}


void releaseReturnStatement(ReturnStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    switch (stmt->type) {
        case RETURN_EXPRESSION:
            releaseExpression(stmt->expression);
            break;
        case RETURN_FUNCTION_STATEMENT:
            releaseFunctionStatement(stmt->functionStatement);
            break;
        default:
            break;
    }
    free(stmt);
}
void releaseAssignmentStatement(AssignmentStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    switch (stmt->type) {
        case MATH_ASSIGNMENT:
            releaseAssignmentMathStatement(stmt->mathAssignment);
            break;
        case STRING_ASSIGNMENT:
            releaseAssignmentStringStatement(stmt->stringAssignment);
            break;
        case BOOL_ASSIGNMENT:
            releaseAssignmentBoolStatement(stmt->boolAssignment);
            break;
        case ARRAY_ASSIGNMENT:
            releaseArrayStatement(stmt->arrayAssignment);
            break;
    }
    free(stmt);
}
void releaseAssignmentMathStatement(AssignmentMathStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseMathExpression(stmt->mathExpression);
    free(stmt);
}

void releaseAssignmentStringStatement(AssignmentStringStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (stmt) free(stmt);
}
void releaseAssignmentBoolStatement(AssignmentBoolStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseBoolExpression(stmt->expression);
    free(stmt);
}

void releaseUnaryChangeOperatorStatement(UnaryChangeOperatorStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    if (stmt->type == ARRAY)
        releaseArrayAccess(stmt->arrayAccess);
    free(stmt);
}
void releaseVariableStatement(VariableStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseExpression(stmt->expression);
    free(stmt);
}

void releaseArrayStatement(ArrayStatement *stmt) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!stmt) return;
    releaseIntList(stmt->elements);
    free(stmt);
}


void releaseBoolExpression(BoolExpression *expr) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!expr) return;
    switch (expr->type) {
        case COMPARATOR_EXPRESSION:
            releaseMathExpression(expr->comparator_expression.expression1);
            releaseMathExpression(expr->comparator_expression.expression2);
            break;
        case LOGICAL_EXPRESSION:
            releaseBoolExpression(expr->logical_expression.expression1);
            releaseBoolExpression(expr->logical_expression.expression2);
            break;
        case BOOL_FACTOR:
            releaseBoolFactor(expr->boolFactor);
            break;
    }
    free(expr);
}

void releaseBoolFactor(BoolFactor *factor) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!factor) return;
    switch (factor->type) {
        case NOT_EXPRESSION:
        case PARENTHESIS_EXPRESSION:
            releaseBoolExpression(factor->expression);
            break;
        case BOOL_FUNCTION:
            releaseFunctionStatement(factor->functionStatement);
            break;
        case BOOL_ARRAY:
            releaseArrayAccess(factor->arrayAccess);
            break;
        default:
            break;
    }
    free(factor);
}

void releaseStringList(StringList *list) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!list) return;
    releaseStringNode(list->strings);
    free(list);
}

void releaseFunctionDefinition(FunctionDefinition *def) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!def) return;
    releaseStringList(def->parameters);
    releaseStatementBlock(def->body);
    free(def);
}

void releaseStatementBlock(StatementBlock *block) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!block) return;
    releaseStatementList(block->statementList);
    free(block);
}
void releaseArrayAccess(ArrayAccess *access) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!access) return;
    releaseMathExpression(access->index);
    free(access);
}
void releaseIntList(IntList *list) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!list) return;
    releaseIntNode(list->integers);
    free(list);
}


void releaseUnit(Unit *unit) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    while (unit) {
        Unit *next = unit->units;
        releaseExternalDeclaration(unit->externalDeclaration);
        free(unit);
        unit = next;
    }
}
void releaseExternalDeclaration(ExternalDeclaration *decl) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (!decl) return;
    switch (decl->type) {
        case FUNCTION_DEFINITION:
            releaseFunctionDefinition(decl->functionDefinition);
            break;
        case STATEMENT:
            releaseStatement(decl->statement);
            break;
    }
    free(decl);
}

void releaseIntNode(IntNode *node) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    while (node) {
        IntNode *next = node->next;
        free(node);
        node = next;
    }
}

void releaseStringNode(StringNode *node) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    while (node) {
        StringNode *next = node->next;
        free(node);
        node = next;
    }
}


void releaseStatementNode(StatementNode *node) {
    logDebugging(_logger, "Executing destructor: %s", __FUNCTION__);
    if (node != NULL) {
        releaseStatement(node->statement);
        free(node);
    }
}
