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
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__, (void*)expression);
    if (expression != NULL) {
        switch (expression->type) {
            case MATH_EXPRESSION:
                releaseMathExpression(expression->mathExpression);
                break;
            case BOOLEAN_EXPRESSION:
                releaseBoolExpression(expression->boolExpression);
                break;
            case STRING_EXPRESSION:
                if (expression->stringExpression) free(expression->stringExpression);
                break;
        }
        free(expression);
    }
}


void releaseFactor(Factor * factor) {
	logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)factor);
	if (factor != NULL) {
		switch (factor->type) {
			case CONSTANT:
				releaseConstant(factor->constant);
				break;
			case EXPRESSION:
                releaseMathExpression(factor->expression);
				break;
            case FACTOR_IDENTIFIER:
                free(factor->identifier);
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
	logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)program);
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
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)constant);
    if (constant != NULL) {
        free(constant);
    }
}
void releaseStatement(Statement *stmt){
    logDebugging(_logger, "Executing destructor: %s, ptr: %p", __FUNCTION__, (void*)stmt);
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
                releaseSortStatement(stmt->sortStatement);
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
                releaseAssignmentStatement(stmt->assignmentStatement);
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
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)list);
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
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)loop);
    if (loop != NULL) {
        releaseAssignmentMathStatement(loop->assignment);
        releaseConstant(loop->endValue);
        releaseStatementBlock(loop->body);
        free(loop);
    }
}

void releaseMatchStatement(MatchStatement *match){
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)match);
    if (match != NULL) {
        if (match->identifier) free(match->identifier);
        releaseCaseList(match->caseList);
        free(match);
    }
}

void releaseCase(Case *c) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)c);
    if (c) {
        releaseStatement(c->body);
        free(c);
    }
}

void releaseCaseList(CaseList * caseList) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)caseList);
    if (!caseList) return;
    releaseCaseNode(caseList->cases);
    free(caseList);
}

void releaseCaseNode(CaseNode *node) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)node);
    while (node) {
        CaseNode *next = node->next;
        releaseCase(node->Case);
        free(node);
        node = next;
    }
}

void releaseMathExpression(MathExpression *expr) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)expr);
    if (!expr) return;
    if (expr->type == FACTOR_EXPRESSION) {
        releaseFactor(expr->factor);
    } else {
        releaseMathExpression(expr->leftExpression);
        releaseMathExpression(expr->rightExpression);
    }
    free(expr);
}
void releaseIfStatement(IfStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)stmt);
    if (!stmt) return;
    releaseBoolExpression(stmt->condition);
    releaseStatementBlock(stmt->thenBranch);
    releaseElseStatement(stmt->elseBranch);
    free(stmt);
}

void releaseElseStatement(ElseStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)stmt);
    if (!stmt) return;
    releaseStatementBlock(stmt->body);
    if (stmt->type == ELSE_IF_STATEMENT)
        releaseIfStatement(stmt->elseIfStatement);
    free(stmt);
}

void releaseWhileLoop(WhileLoop *loop) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)loop);
    if (!loop) return;
    releaseBoolExpression(loop->condition);
    releaseStatementBlock(loop->body);
    free(loop);
}
void releasePrintStatement(PrintStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)stmt);
    if (stmt) {
        if (stmt->identifier)
            free(stmt->identifier);
        free(stmt);
    }
}
void releaseSortStatement(SortStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)stmt);
    if (stmt == NULL)  return;
    if (stmt->identifier) free(stmt->identifier);
    free(stmt);
}

void releaseMacroStatement(MacroStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__, (void*)stmt);
    if (!stmt) return;
    if (stmt->identifier) free(stmt->identifier);
    releaseStringList(stmt->parameters);
    releaseStatement(stmt->statement);
    free(stmt);
}

void releaseFunctionStatement(FunctionStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
    if (!stmt) return;
    if (stmt->identifier) free(stmt->identifier);
    releaseArgumentList(stmt->parameters);
    free(stmt);
}


void releaseReturnStatement(ReturnStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
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
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
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
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
    if (!stmt) return;
    if (stmt->identifier) free(stmt->identifier);
    releaseMathExpression(stmt->mathExpression);
    free(stmt);
}

void releaseAssignmentStringStatement(AssignmentStringStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
    if (!stmt) return;
    if (stmt->identifier) free(stmt->identifier);
    if (stmt->expression) free(stmt->expression);
    free(stmt);

}
void releaseAssignmentBoolStatement(AssignmentBoolStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
    if (!stmt) return;
    releaseBoolExpression(stmt->expression);
    if(stmt->identifier)free(stmt->identifier);
    free(stmt);
}

void releaseUnaryChangeOperatorStatement(UnaryChangeOperatorStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
    if (!stmt) return;
    if (stmt->identifier) free(stmt->identifier);
    if (stmt->type == ARRAY) {releaseArrayAccess(stmt->arrayAccess);}
    free(stmt);
}
void releaseVariableStatement(VariableStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
    if (!stmt) return;
    if (stmt->identifier) free(stmt->identifier);
    releaseExpression(stmt->expression);
    free(stmt);
}

void releaseArrayStatement(ArrayStatement *stmt) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)stmt);
    if (!stmt) return;
    free(stmt->identifier);
    releaseIntList(stmt->elements);
    free(stmt);
}


void releaseBoolExpression(BoolExpression *expr) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)expr);
    if (!expr) return;
    switch (expr->type) {
        case COMPARATOR_EXPRESSION:
            releaseMathExpression(expr->comparatorExpression.expression1);
            releaseMathExpression(expr->comparatorExpression.expression2);
            break;
        case LOGICAL_EXPRESSION:
            releaseBoolExpression(expr->logicalExpression.expression1);
            releaseBoolExpression(expr->logicalExpression.expression2);
            break;
        case BOOL_FACTOR:
            releaseBoolFactor(expr->boolFactor);
            break;
    }
    free(expr);
}

void releaseBoolFactor(BoolFactor *factor) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)factor);
    if (!factor) return;
    switch (factor->type) {
        case NOT_EXPRESSION:
            releaseBoolExpression(factor->expression);
            break;
        case PARENTHESIS_EXPRESSION:
            releaseBoolExpression(factor->expression);
            break;
        case BOOL_FUNCTION:
            releaseFunctionStatement(factor->functionStatement);
            break;
        case BOOL_ARRAY:
            releaseArrayAccess(factor->arrayAccess);
            break;
        case BOOLEAN_ID:
            if (factor->identifier) free(factor->identifier);
            break;
        default:
            break;
    }
    free(factor);
}

void releaseStringList(StringList *list) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)list);
    if (!list) return;
    releaseStringNode(list->strings);
    free(list);
}

void releaseFunctionDefinition(FunctionDefinition *def) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)def);
    if (!def) return;
    releaseArgumentDefList(def->parameters);
    releaseStatementBlock(def->body);
    free(def->identifier);
    free(def);
}

void releaseStatementBlock(StatementBlock *block) {
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__,  (void*)block);
    if (!block) return;
    releaseStatementList(block->statementList);
    free(block);
}
void releaseArrayAccess(ArrayAccess *access) {
    logDebugging(_logger, "Executing destructor: %s, ptr: %p", __FUNCTION__, (void*)access);
    if (access == NULL) return;
    if (access->identifier) free(access->identifier);
    releaseMathExpression(access->index);
    free(access);
}
void releaseIntList(IntList *list) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)list);
    if (!list) return;
    releaseIntNode(list->integers);
    free(list);
}


void releaseUnit(Unit *unit) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)unit);
    while (unit) {
        Unit *next = unit->units;
        releaseExternalDeclaration(unit->externalDeclaration);
        free(unit);
        unit = next;
    }
}
void releaseExternalDeclaration(ExternalDeclaration *decl) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)decl);
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
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)node);
    while (node) {
        IntNode *next = node->next;
        free(node);
        node = next;
    }
}

void releaseStringNode(StringNode *node) {
    logDebugging(_logger,"Executing destructor: %s ptr: %p", __FUNCTION__,(void*)node);
    while (node) {
        StringNode *next = node->next;
        if (node->string) free(node->string);
        free(node);
        node = next;
    }
}


void releaseStatementNode(StatementNode *node) {
logDebugging(_logger, "Executing destructor: %s, ptr: %p", __FUNCTION__, (void*)node);
    if (node) {
        releaseStatement(node->statement);
        free(node);

    }
}



void releaseArgumentValue(ArgumentValue * argumentValue) {
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__, (void*)argumentValue);
    if (argumentValue != NULL) {
          switch (argumentValue->type) {
            case ARGUMENT_MATH_EXPRESSION:
                releaseMathExpression(argumentValue->mathExpression);
                break;
            case ARGUMENT_BOOL_EXPRESSION:
                releaseBoolExpression(argumentValue->boolExpression);
                break;
            case ARGUMENT_STRING_EXPRESSION:
                if (argumentValue->stringExpression) free(argumentValue->stringExpression);
                break;
            case ARGUMENT_FUNCTION_EXPRESSION:
                releaseFunctionStatement(argumentValue->functionExpression);
                break;
            case ARGUMENT_BOOL_ARRAY_ID:
            case ARGUMENT_STRING_ARRAY_ID:
            case ARGUMENT_INT_ARRAY_ID:
                free(argumentValue->identifier);
                break;
            case ARGUMENT_ARRAY_ACCESS:
                releaseArrayAccess(argumentValue->arrayAccess);
                break;
            case ARGUMENT_UNARY_CHANGE_OPERATOR:
                releaseUnaryChangeOperatorStatement(argumentValue->unaryChangeOperatorStatement);
                break;
    }
        free(argumentValue);
}
}
void releaseArgumentList(ArgumentList * argumentList){
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__, (void*)argumentList);
    if (argumentList != NULL) {
        ArgumentNode *current = argumentList->arguments;
        ArgumentNode *next;
        while (current != NULL) {
            next = current->next;
            releaseArgumentNode(current);
            current = next;
        }
        free(argumentList);
    }
}
void releaseArgumentNode(ArgumentNode * argumentNode){
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__, (void*)argumentNode);
    if (argumentNode != NULL) {
        releaseArgumentValue(argumentNode->argument);
        free(argumentNode);
    }
}

void releaseArgumentDef(ArgumentDef * argumentDef){
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__, (void*)argumentDef);
    if (argumentDef != NULL) {
       free(argumentDef->identifier);
       free(argumentDef);
    }
}
void releaseArgumentDefNode(ArgumentDefNode * argumentDefNode){
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__, (void*)argumentDefNode);
    if (argumentDefNode != NULL) {
        releaseArgumentDef(argumentDefNode->argumentDef);
        free(argumentDefNode);
    }
}
void releaseArgumentDefList(ArgumentDefList * argumentDefList)
{
    logDebugging(_logger, "Executing destructor: %s ptr: %p", __FUNCTION__, (void*)argumentDefList);
    if (argumentDefList != NULL) {
        ArgumentDefNode *current = argumentDefList->arguments;
        ArgumentDefNode *next;
        while (current != NULL) {
            next = current->next;
            releaseArgumentDefNode(current);
            current = next;
        }
        free(argumentDefList);
    }
}