 #include "Generator.h"

 /* MODULE INTERNAL STATE */

 const char _indentationCharacter = ' ';
 const char _indentationSize = 4;
 static Logger * _logger = NULL;

 void initializeGeneratorModule() {
 	_logger = createLogger("Generator");
 }

 void shutdownGeneratorModule() {
 	if (_logger != NULL) {
 		destroyLogger(_logger);
 	}
 }

 /** PRIVATE FUNCTIONS */
static void _generateProgram(Program * program);
static void _generateExpression( Expression * expression);
static void _generateFactor( Factor * factor);
static void _generateConstant( Constant * constant);
static void _generateStatement(Statement * statement);
static void _generateStatementList(StatementList * list);
static void _generateForLoop(ForLoop * loop);
static void _generateMatchStatement(MatchStatement * match);
static void _generateCase(Case * c);
static void _generateCaseList(CaseList * caseList);
static void _generateMathExpression(MathExpression * mathExpression);
static void _generateIfStatement(IfStatement * ifStatement);
static void _generateElseStatement(ElseStatement * elseStatement);
static void _generateWhileLoop(WhileLoop * whileLoop);
static void _generatePrintStatement(PrintStatement * printStatement);
static void _generateSortStatement(SortStatement * sortStatement);
static void _generateMacroStatement(MacroStatement * macroStatement);
static void _generateFunctionStatement(FunctionStatement * functionStatement);
static void _generateReturnStatement(ReturnStatement * returnStatement);
static void _generateAssignmentStatement(AssignmentStatement * assignmentStatement);
static void _generateAssignmentMathStatement(AssignmentMathStatement * assignmentMathStatement);
static void _generateAssignmentStringStatement(AssignmentStringStatement * assignmentStringStatement);
static void _generateAssignmentBoolStatement(AssignmentBoolStatement * assignmentBoolStatement);
static void _generateUnaryChangeOperatorStatement(UnaryChangeOperatorStatement * unaryChangeOperatorStatement);
static void _generateVariableStatement(VariableStatement * variableStatement);
static void _generateArrayStatement(ArrayStatement * arrayStatement);
static void _generateBoolExpression(BoolExpression * boolExpression);
static void _generateBoolFactor(BoolFactor * boolFactor);
static void _generateStringList(StringList * stringList);
static void _generateFunctionDefinition(FunctionDefinition * functionDefinition);
static void _generateStatementBlock(StatementBlock * statementBlock);
static void _generateArrayAccess(ArrayAccess * arrayAccess);
static void _generateIntList(IntList * intList);
static void _generateUnit(Unit * unit);
static void _generateExternalDeclaration(ExternalDeclaration * externalDeclaration);
static void _generateArgumentList(ArgumentList * argumentList);
static void _generateArgumentDefList(ArgumentDefList * argumentDefList);
static void _generateStringExpression(StringExpression * stringExpression);
static void _generateMacroInvocationStatement(MacroInvocationStatement * macroInvocationStatement);
static void _generateArrayAssignment(ArrayAssignment * arrayAssignment);
static void _generateBoolList(BoolList * list);
static void _generateEpilogue(const int value);
static void _generatePrologue();
static void _output(const unsigned int indentationLevel, const char * const format, ...);
static void _generateArgumentValue(ArgumentValue * argumentValue);


static void _generateConstant( Constant * constant){
    if (constant == NULL) return;
        _output(0, "%d", constant->value);
}

static void _generateExpression(Expression * expression){
    if (expression == NULL) return;
    switch (expression->type) {
        case MATH_EXPRESSION:
            _generateMathExpression(expression->mathExpression);
            break;

        case BOOLEAN_EXPRESSION:
            _generateBoolExpression(expression->boolExpression);
            break;

        case STRING_EXPRESSION:
            _generateStringExpression(expression->stringExpression);
            break;
        default:
            logError(_logger, "Unknown expression type: %d", expression->type);
            break;
    }

}
static void _generateStatement(Statement * statement) {
    if (statement == NULL) return;
    logDebugging(_logger, "Generating statement of type %d", statement->type);
    switch (statement->type) {
        case STATEMENT_MATH_EXPRESSION:
            _generateExpression( statement->mathExpression);
            _output(0, ";\n");
            break;

        case STATEMENT_FOR:
            _generateForLoop(statement->forLoop);
            break;

        case STATEMENT_MATCH:
            _generateMatchStatement(statement->matchStatement);
            break;

        case STATEMENT_WHILE:
            _generateWhileLoop(statement->whileLoop);
            break;

        case STATEMENT_IF:
            _generateIfStatement(statement->ifStatement);
            break;

        case STATEMENT_PRINT:
            _generatePrintStatement(statement->printStatement);
            break;

        case STATEMENT_SORT:
            _generateSortStatement(statement->sortStatement);
            break;

        case STATEMENT_MACRO:
            logDebugging(_logger, "Generating macro statement...");
            _generateMacroStatement(statement->macroStatement);
            break;

        case STATEMENT_FUNCTION:
            _generateFunctionStatement(statement->functionStatement);
            break;

        case STATEMENT_RETURN:
            _generateReturnStatement(statement->returnStatement);
            break;

        case STATEMENT_ASSIGNMENT:
            logDebugging(_logger, "Generating assignment statement...");
            _generateAssignmentStatement(statement->assignmentStatement);
            break;

        case STATEMENT_UNARY_CHANGE_OPERATOR:
            _generateUnaryChangeOperatorStatement(statement->unaryChangeOperatorStatement);
            break;

        case STATEMENT_VARIABLE:
            _generateVariableStatement(statement->variableStatement);
            break;

        case STATEMENT_ARRAY:
            _generateArrayStatement(statement->arrayStatement);
            break;

        case STATEMENT_MACRO_INVOCATION:
            _generateMacroInvocationStatement(statement->macroInvocationStatement);
            break;

        default:
            logError(_logger, "Unknown statement type: %d", statement->type);
            break;
    }
}


static void _generateStatementList(StatementList * list) {
    if (list == NULL || list->statements == NULL) return;

    logDebugging(_logger, "Generating statement list...");

    StatementNode * current = list->statements;
    while (current != NULL) {
        _generateStatement(current->statement);
        current = current->next;
    }
}


static void _generateForLoop(ForLoop *forLoop) {
    if (forLoop == NULL || forLoop->assignment == NULL || forLoop->assignment->identifier == NULL) return;

    const char *varName = forLoop->assignment->identifier;

    // for (
    _output(0, "for (");
    _output(0, varName);
    _output(0, " = ");
    _generateMathExpression(forLoop->assignment->mathExpression); // valor inicial
    _output(0, "; ");

    // condición
    _output(0, varName);
    _output(0, " <= ");
    _generateConstant(forLoop->endValue); // valor final
    _output(0, "; ");

    // incremento
    _output(0, varName);
    _output(0, "++) {\n");

    // cuerpo del for
    _generateStatementBlock(forLoop->body);

    _output(0, "}\n");
}



static void _generateMatchStatement(MatchStatement *matchStmt) {
    if (matchStmt == NULL) return;

    _output(0, "switch(");
    _output(0, matchStmt->identifier);
    _output(0, ") {\n");

    _generateCaseList(matchStmt->caseList);

    _output(0, "}\n");
}

static void _generateCase(Case *c) {
    if (c == NULL) return;

    switch (c->type) {
    case INTEGER_CASE:
        _output(1, "case %d:\n", c->matchValue);
        break;

    case STRING_CASE:
        _output(1, "case \"%s\":\n", c->string);
        break;

    case DEFAULT_CASE:
        _output(1, "default:\n");
        break;

    default:
        logError(_logger, "Unknown case type: %d", c->type);
        return;
    }

    _generateStatement(c->body);

    _output(2, "break;\n");
}


static void _generateCaseList(CaseList *caseList) {
    if (caseList == NULL) return;

    CaseNode *node = caseList->cases;
    while (node != NULL) {
        _generateCase(node->Case);
        node = node->next;
    }
}

static void _generateFactor(Factor *factor) {
    if (factor == NULL) return;

    switch (factor->type) {
    case CONSTANT:
            char buffer[32];
        snprintf(buffer, sizeof(buffer), "%d", factor->constant->value);
        _output(0, buffer);
        break;

    case FACTOR_IDENTIFIER:
        _output(0, factor->identifier);
        break;

    case EXPRESSION:
        _output(0, "(");
        _generateMathExpression(factor->expression);
        _output(0, ")");
        break;

    case BOOLEAN_FACTOR:
        _output(0, factor->boolean ? "1" : "0");
        break;

    case FUNCTION:
        _generateFunctionStatement(factor->functionStatement);
        break;

    case ARRAY_FACTOR:
        _generateArrayAccess(factor->arrayAccess);
        break;

    case UNARY_CHANGE_FACTOR:
        _generateUnaryChangeOperatorStatement(factor->unaryChangeOperatorStatement);
        break;

    case MACRO_INVOCATION:
        _generateMacroInvocationStatement(factor->macroInvocationStatement);
        break;

    default:
        _output(0, "/* unknown factor */");
        break;
    }
}



static void _generateMathExpression(MathExpression *expr) {
    if (expr == NULL) return;

    if (expr->type == FACTOR_EXPRESSION) {
        _generateFactor(expr->factor);
    } else if (expr->type == OPERATOR_EXPRESSION) {
        _output(0, "(");
        _generateMathExpression(expr->leftExpression);

        // Operador
        switch (expr->mathType) {
        case ADDITION:
            _output(0, " + ");
            break;
        case SUBTRACTION:
            _output(0, " - ");
            break;
        case MULTIPLICATION:
            _output(0, " * ");
            break;
        case DIVISION:
            _output(0, " / ");
            break;
        case FACTOR:
            _generateFactor(expr->factor);
            break;
        }

        _generateMathExpression(expr->rightExpression);
        _output(0, ")");
    }
}


static void _generateIfStatement(IfStatement *ifStmt) {
    if (ifStmt == NULL || ifStmt->condition == NULL || ifStmt->thenBranch == NULL) return;

    _output(0, "if (");
    _generateBoolExpression(ifStmt->condition);
    _output(0, ") {\n");

    _generateStatementBlock(ifStmt->thenBranch);
    _output(0, "}");

    if (ifStmt->elseBranch != NULL) {
        _generateElseStatement(ifStmt->elseBranch);
    }

    _output(0, "\n");
}

static void _generateElseStatement(ElseStatement *elseStmt) {
    if (elseStmt == NULL) return;

    if (elseStmt->type == ELSE_STATEMENT) {
        _output(0, " else {\n");
        _generateStatementBlock(elseStmt->body);
        _output(0, "}");
    } else if (elseStmt->type == ELSE_IF_STATEMENT && elseStmt->elseIfStatement != NULL) {
        _output(0, " else ");
        _generateIfStatement(elseStmt->elseIfStatement);
    }
}

static void _generateWhileLoop(WhileLoop *whileLoop) {
    if (whileLoop == NULL || whileLoop->condition == NULL || whileLoop->body == NULL) return;

    _output(0, "while (");
    _generateBoolExpression(whileLoop->condition);
    _output(0, ") {\n");

    _generateStatementBlock(whileLoop->body);

    _output(0, "}\n");
}


static void _generatePrintStatement(PrintStatement * printStatement) {
    if (printStatement == NULL) return;

    switch (printStatement->type) {
    case PRINT_MATH_EXPRESSION:
        _output(0, "printf(\"%%d\\n\", ");
        _generateMathExpression(printStatement->mathExpression);
        _output(0, ");\n");
        break;

    case PRINT_IDENTIFIER:
        _output(0, "printf(\"%%s\\n\", ");
        _generateStringExpression(printStatement->identifier);
        _output(0, ");\n");
        break;

    default:
        logError(_logger, "Unknown print statement type: %d", printStatement->type);
        break;
    }
}


static void _generateSortStatement(SortStatement *sortStatement) {
    logDebugging(_logger, "Generating sort statement for array %s", sortStatement->identifier);
    if (sortStatement == NULL) return;

    _output(0, "sortArray(");
    _output(0, sortStatement->identifier);
    _output(0, ", sizeof(");
    _output(0, sortStatement->identifier);
    _output(0, ") / sizeof(");
    _output(0, sortStatement->identifier);
    _output(0, "[0]), ");

    switch (sortStatement->order) {
    case ORDER_ASC:
        _output(0, "ORDER_ASCENDING");
        break;
    case ORDER_DESC:
        _output(0, "ORDER_DESCENDING");
        break;
    default:
        _output(0, "ORDER_ASCENDING");
        break;
    }

    _output(0, ");\n");
}


static void _generateMacroStatement(MacroStatement * macroStatement) {
    if (macroStatement == NULL) return;

    _output(0, "#define %s(", macroStatement->identifier);

    if (macroStatement->parameters != NULL) {
        _generateStringList(macroStatement->parameters);
    }

    _output(0, ") ");

    if (macroStatement->statement != NULL) {
        _generateMathExpression(macroStatement->statement);

    }
        _output(0, "\n");

}

static void _generateFunctionStatement(FunctionStatement * functionStatement) {
    if (functionStatement == NULL) return;

    _output(0, "%s(", functionStatement->identifier);

    if (functionStatement->parameters != NULL) {
        _generateArgumentList(functionStatement->parameters);
    }

    _output(0, ")");
}


static void _generateReturnStatement(ReturnStatement * returnStatement) {
    if (returnStatement == NULL) return;

    switch (returnStatement->type) {
    case RETURN_EMPTY:
        _output(0, "return;\n");
        break;

    case RETURN_EXPRESSION:
        _output(0, "return ");
        _generateExpression(returnStatement->expression);
        _output(0, ";\n");
        break;

    case RETURN_FUNCTION_STATEMENT:
        _output(0, "return ");
        _generateFunctionStatement(returnStatement->functionStatement);
        _output(0, ";\n");
        break;

    default:
        logError(_logger, "Unknown return type: %d", returnStatement->type);
        break;
    }
}


static void _generateAssignmentStatement(AssignmentStatement *stmt) {
    if (stmt == NULL) return;

    switch (stmt->type) {
    case MATH_ASSIGNMENT:
        _output(0, "%s = ", stmt->mathAssignment->identifier);
        _generateMathExpression(stmt->mathAssignment->mathExpression);
        _output(0, ";\n");
        break;

    case BOOL_ASSIGNMENT:
        _output(0, "%s = ", stmt->boolAssignment->identifier);
        _generateBoolExpression(stmt->boolAssignment->expression);
        _output(0, ";\n");
        break;

    case STRING_ASSIGNMENT:
        _output(0, "%s = ", stmt->stringAssignment->identifier);
        _generateStringExpression(stmt->stringAssignment->expression);
        _output(0, ";\n");
        break;

    case ARRAY_ASSIGNMENT:
        _generateArrayAssignment(stmt->arrayAssignmentExpression);
        break;

    case ARRAY_STATEMENT:
        _generateArrayStatement(stmt->arrayAssignment);
        break;

    default:
        _output(0, "/* Tipo de asignación desconocido */\n");
        break;
    }
}


static void _generateAssignmentMathStatement(AssignmentMathStatement *assign) {
    if (assign == NULL) return;
    _output(0, "%s = ", assign->identifier);
    _generateMathExpression(assign->mathExpression);
    _output(0, ";\n");
}

static void _generateAssignmentBoolStatement(AssignmentBoolStatement *assign) {
    if (assign == NULL) return;
    _output(0, "%s = ", assign->identifier);
    _generateBoolExpression(assign->expression);
    _output(0, ";\n");
}

static void _generateAssignmentStringStatement(AssignmentStringStatement *assign) {
    if (assign == NULL) return;
    _output(0, "%s = ", assign->identifier);
    _generateStringExpression(assign->expression);
    _output(0, ";\n");
}

static void _generateUnaryChangeOperatorStatement(UnaryChangeOperatorStatement * stmt) {
    if (stmt == NULL) return;

    const char * op = NULL;
    const char * position = NULL;

    switch (stmt->operator_type) {
    case PRE_INCREMENT:  op = "++"; position = "pre"; break;
    case PRE_DECREMENT:  op = "--"; position = "pre"; break;
    case POST_INCREMENT: op = "++"; position = "post"; break;
    case POST_DECREMENT: op = "--"; position = "post"; break;
    default:
        logError(_logger, "Unknown unary change operator type: %d", stmt->operator_type);
        return;
    }

    if (stmt->type == VARIABLE) {
        if (stmt->operator_type == PRE_INCREMENT || stmt->operator_type == PRE_DECREMENT)
            _output(0, "%s%s;\n", op, stmt->identifier);     // ++x;
        else
            _output(0, "%s%s;\n", stmt->identifier, op);     // x++;
    }
    else if (stmt->type == ARRAY) {
         if (stmt->operator_type == PRE_INCREMENT || stmt->operator_type == PRE_DECREMENT) {
            _output(0, "%s", op);                            // ++
            _generateArrayAccess(stmt->arrayAccess);        // array[i]
            _output(0, ";\n");
        } else {
            _generateArrayAccess(stmt->arrayAccess);        // array[i]
            _output(0, "%s;\n", op);                         // ++;
        }
    }
    else {
        logError(_logger, "Unknown unary change statement type: %d", stmt->type);
    }
}


static void _generateVariableStatement(VariableStatement *stmt) {
    if (stmt == NULL) return;

    switch (stmt->type) {
    case _BOOL:
        _output(0, "bool %s", stmt->identifier);
        if (stmt->expression != NULL) {
            _output(0, " = ");
            _generateExpression(stmt->expression);
        }
        _output(0, ";\n");
        break;

    case _INT:
        _output(0, "int %s", stmt->identifier);
        if (stmt->expression != NULL) {
            _output(0, " = ");
            logDebugging(_logger, "Generating math expression for variable %s", stmt->identifier);
            _generateExpression(stmt->expression);
        }
        _output(0, ";\n");
        break;

    case _STRING:
        _output(0, "const char *%s", stmt->identifier);
        if (stmt->expression != NULL) {
            _output(0, " = ");
            _generateExpression(stmt->expression);
        }
        _output(0, ";\n");
        break;

    default:
        _output(0, "/* Tipo de variable desconocido */\n");
        break;
    }
}


static void _generateArrayStatement(ArrayStatement *stmt) {
    if (stmt == NULL) return;

    switch (stmt->type) {
    case INT_LIST:
        _output(0, "int %s[] = {", stmt->identifier);
        _generateIntList(stmt->elements);
        _output(0, "};\n");
        break;

    case BOOL_LIST:
        _output(0, "bool %s[] = {", stmt->identifier);
        _generateBoolList(stmt->boolElements);
        _output(0, "};\n");
        break;

    case STRING_LIST:
        _output(0, "const char *%s[] = {", stmt->identifier);
        _generateStringList(stmt->stringElements);
        _output(0, "};\n");
        break;

    case INT_SIZE:
        _output(0, "int %s[", stmt->identifier);
        _generateMathExpression(stmt->mathExpression);
        _output(0, "];\n");
        break;

    case BOOL_SIZE:
        _output(0, "bool %s[", stmt->identifier);
        _generateMathExpression(stmt->mathExpression);
        _output(0, "];\n");
        break;

    case STRING_SIZE:
        _output(0, "const char *%s[", stmt->identifier);
        _generateMathExpression(stmt->mathExpression);
        _output(0, "];\n");
        break;

    default:
        _output(0, "/* Tipo de array desconocido */\n");
        break;
    }
}


static void _generateBoolExpression(BoolExpression *expr) {
    if (!expr) return;

    switch (expr->type) {
    case COMPARATOR_EXPRESSION:
        _generateMathExpression(expr->comparatorExpression.expression1);
        switch (expr->comparatorExpression.comparatorType) {
        case EQUAL: _output(0, " == "); break;
        case NOT_EQUAL: _output(0, " != "); break;
        case LESS_THAN: _output(0, " < "); break;
        case LESS_EQUAL: _output(0, " <= "); break;
        case GREATER_THAN: _output(0, " > "); break;
        case GREATER_EQUAL: _output(0, " >= "); break;
        }
        _generateMathExpression(expr->comparatorExpression.expression2);
        break;

    case LOGICAL_EXPRESSION:
        _output(0, "(");
        _generateBoolExpression(expr->logicalExpression.expression1);
        if (expr->logicalExpression.operatorType == LOGICAL_AND)
            _output(0, " && ");
        else if (expr->logicalExpression.operatorType == LOGICAL_OR)
            _output(0, " || ");
        _generateBoolExpression(expr->logicalExpression.expression2);
        _output(0, ")");
        break;

    case BOOL_FACTOR:
        _generateBoolFactor(expr->boolFactor);
        break;
    }
}

static void _generateBoolFactor(BoolFactor * f) {
    if (!f) return;

    switch (f->type) {
    case NOT_EXPRESSION:
        _output(0, "!");
        _generateBoolExpression(f->expression);
        break;

    case PARENTHESIS_EXPRESSION:
        _output(0, "(");
        _generateBoolExpression(f->expression);
        _output(0, ")");
        break;

    case BOOLEAN_ID:
        _output(0, f->identifier);
        break;

    case BOOL_CONSTANT:
        _output(0, f->boolean ? "1" : "0");
        break;

    case BOOL_FUNCTION:
        _generateFunctionStatement(f->functionStatement);
        break;

    case BOOL_ARRAY:
        _generateArrayAccess(f->arrayAccess);
        break;
    }
}


void _generateStringList(StringList *list) {
    if (list == NULL || list->strings == NULL) {
        _output(0, "/* Lista vacía */");
        return;
    }

    StringNode *current = list->strings;
    while (current != NULL) {
        _output(0, "%s", current->string);  // Sin comillas
        if (current->next != NULL) {
            _output(0, ", ");
        }
        current = current->next;
    }
}

static void _generateFunctionDefinition(FunctionDefinition * functionDefinition) {
    // Obtener el tipo como string
    const char * returnType = NULL;
    switch (functionDefinition->type) {
    case _INT: returnType = "int"; break;
    case _VOID: returnType = "void"; break;
    case _STRING: returnType = "char *"; break;
    case _BOOL: returnType = "bool"; break;
    case _INT_ARRAY: returnType = "int *"; break;
    case _STRING_ARRAY: returnType = "char **"; break;
    case _BOOL_ARRAY: returnType = "bool *"; break;
    default: returnType = "/* unknown */"; break;
    }

    _output(0, "%s %s(", returnType, functionDefinition->identifier);

    if (functionDefinition->parameters != NULL) {
        _generateArgumentDefList(functionDefinition->parameters);
    }
    _output(0, ") {\n");
    if (functionDefinition->body != NULL) {
        _generateStatementBlock(functionDefinition->body);
    }

    _output(0, "}\n\n");
}

static void _generateStatementBlock(StatementBlock * statementBlock) {
    if (statementBlock == NULL || statementBlock->statementList == NULL)
        return;

    StatementNode * current = statementBlock->statementList->statements;
    while (current != NULL) {
        _generateStatement(current->statement);
        current = current->next;
    }
}


static void _generateArrayAccess(ArrayAccess * access) {
    if (access == NULL) return;
    _output(0, "%s[", access->identifier);
    _generateMathExpression(access->index);
    _output(0, "]");
}


void _generateIntList(IntList *list) {
    if (list == NULL || list->count == 0) {
        _output(0, "/* Lista vacía */");
        return;
    }

    IntNode *current = list->integers;
    while (current != NULL) {
        _output(0, "%d", current->integer);
        if (current->next != NULL) {
            _output(0, ", ");
        }
        current = current->next;
    }
}



static void _generateUnit(Unit * unit) {
    logDebugging(_logger, "Generating unit...");
    if (unit->type == NODE){
    _generateExternalDeclaration(unit->externalDeclaration);
    _generateUnit(unit->units);
    } else if (unit->type == SINGLE) {
         _generateExternalDeclaration(unit->externalDeclaration);
    } else if(unit->type == NEW_LINE_UNIT) {
        logDebugging(_logger, "Generating new line unit...");
        _output(0, "\n");
        _generateUnit(unit->units);
    }
}
static void _generateProgram(Program * program) {
    logDebugging(_logger, "Generating program...");
    if (program->type == NOT_EMPTY) {
        _generateUnit(program->unit);
    }
}
static void _generateExternalDeclaration(ExternalDeclaration * externalDeclaration) {
    logDebugging(_logger, "Generating external declaration...");
    if (externalDeclaration->type == FUNCTION_DEFINITION) {
        _generateFunctionDefinition(externalDeclaration->functionDefinition);
    } else if (externalDeclaration->type == STATEMENT) {
        _generateStatement(externalDeclaration->statement);
    } else if (externalDeclaration->type == MACRO_STATEMENT) {
        _generateMacroStatement(externalDeclaration->macroStatement);
    }
}

static void _generateArgumentValue(ArgumentValue * argumentValue) {
    if (argumentValue == NULL) return;

    switch (argumentValue->type) {
    case ARGUMENT_MATH_EXPRESSION:
        _generateMathExpression(argumentValue->mathExpression);
        break;

    case ARGUMENT_BOOL_EXPRESSION:
        _generateBoolExpression(argumentValue->boolExpression);
        break;

    case ARGUMENT_STRING_EXPRESSION:
        _generateStringExpression(argumentValue->stringExpression);
        break;

    case ARGUMENT_FUNCTION_EXPRESSION:
        _generateFunctionStatement(argumentValue->functionExpression);
        break;

    case ARGUMENT_BOOL_ARRAY_ID:
    case ARGUMENT_STRING_ARRAY_ID:
    case ARGUMENT_INT_ARRAY_ID:
        _output(0, "%s", argumentValue->identifier);
        break;

    case ARGUMENT_ARRAY_ACCESS:
        _generateArrayAccess(argumentValue->arrayAccess);
        break;

    case ARGUMENT_UNARY_CHANGE_OPERATOR:
        _generateUnaryChangeOperatorStatement(argumentValue->unaryChangeOperatorStatement);
        break;

    default:
        logError(_logger, "Unknown argument value type: %d", argumentValue->type);
        break;
    }
}


static void _generateArgumentList(ArgumentList * argumentList) {
    if (argumentList == NULL) return;

    ArgumentNode * current = argumentList->arguments;
    while (current != NULL) {
        _generateArgumentValue(current->argument);

        if (current->next != NULL)
            _output(0, ", ");

        current = current->next;
    }
}


static void _generateArgumentDefList(ArgumentDefList * argumentDefList) {
    ArgumentDefNode * current = argumentDefList->arguments;
    while (current != NULL) {
        ArgumentDef * arg = current->argumentDef;
        const char * typeStr = NULL;
        switch (arg->type) {
        case _INT: typeStr = "int"; break;
        case _BOOL: typeStr = "bool"; break;
        case _STRING: typeStr = "char *"; break;
        case _INT_ARRAY: typeStr = "int *"; break;
        case _STRING_ARRAY: typeStr = "char **"; break;
        case _BOOL_ARRAY: typeStr = "bool *"; break;
        default: typeStr = "/* unknown */"; break;
        }

        _output(0, "%s %s", typeStr, arg->identifier);

        if (current->next != NULL)
            _output(0, ", ");
        current = current->next;
    }
}

static void _generateStringExpression(StringExpression *expr) {
    if (expr == NULL) return;

    switch (expr->type) {
    case STRING_IDENTIFIER_EXPRESSION:
        // Una variable string
            _output(0, expr->identifier);
        break;

    case STRING_VALUE_EXPRESSION:
            _output(0, "\"");
        _output(0, expr->string);
        _output(0, "\"");
        break;

    case STRING_EXPRESSION_ARRAY:
        // Si es arr[i]
            _generateArrayAccess(expr->arrayAccess);
        break;

    case STRING_EXPRESSION_FUNCTION:
            _generateFunctionStatement(expr->functionStatement);
        break;

    default:
        _output(0, "/* unknown string expr */");
        break;
    }
}


static void _generateMacroInvocationStatement(MacroInvocationStatement * macroInvocationStatement) {
    if (macroInvocationStatement == NULL) return;

    _output(0, "%s(", macroInvocationStatement->identifier);

    _generateArgumentList(macroInvocationStatement->arguments);

    _output(0, ");\n");
}


static void _generateArrayAssignment(ArrayAssignment *assign) {
    if (assign == NULL) return;

    // Generar acceso al array (ejemplo: arr[5])
    _generateArrayAccess(assign->arrayAccess);

    _output(0, " = ");

    // Según el tipo de asignación, generar el valor correspondiente
    switch (assign->type) {
    case ARRAY_MATH_EXPRESSION:
        _generateMathExpression(assign->mathExpression);
        break;

    case ARRAY_BOOL_EXPRESSION:
        _generateBoolExpression(assign->boolExpression);
        break;

    case ARRAY_STRING_EXPRESSION:
        _generateStringExpression(assign->stringExpression);
        break;

    default:
        _output(0, "/* tipo desconocido */");
        break;
    }

    _output(0, ";\n");
}


void _generateBoolList(BoolList *list) {
    if (list == NULL || list->count == 0) {
        _output(0, "/* Lista vacía */");
        return;
    }

    BoolNode *current = list->booleans;
    while (current != NULL) {
        _output(0, current->boolean ? "true" : "false");
        if (current->next != NULL) {
            _output(0, ", ");
        }
        current = current->next;
    }
}





static void _generatePrologue(){
    _output(0, "#include <stdlib.h>\n\n");
    _output(0, "#include <stdio.h>\n\n");
    _output(0, "typedef enum {\n");
    _output(1, "ORDER_ASCENDING,\n");
    _output(1, "ORDER_DESCENDING\n");
    _output(0, "} Order;\n\n");

    _output(0, "int compareAsc(const void *a, const void *b) {\n");
    _output(1, "int intA = *(const int*)a;\n");
    _output(1, "int intB = *(const int*)b;\n");
    _output(1, "return (intA > intB) - (intA < intB);\n");
    _output(0, "}\n\n");

    _output(0, "int compareDesc(const void *a, const void *b) {\n");
    _output(1, "int intA = *(const int*)a;\n");
    _output(1, "int intB = *(const int*)b;\n");
    _output(1, "return (intB > intA) - (intB < intA);\n");
    _output(0, "}\n\n");

    _output(0, "void sortArray(int *array, int length, Order order) {\n");
    _output(1, "if (array == NULL || length <= 0) return;\n");
    _output(1, "if (order == ORDER_ASCENDING) {\n");
    _output(2, "qsort(array, length, sizeof(int), compareAsc);\n");
    _output(1, "} else {\n");
    _output(2, "qsort(array, length, sizeof(int), compareDesc);\n");
    _output(1, "}\n");
    _output(0, "}\n\n");
}

 static char * _indentation(const unsigned int level) {
 	return indentation(_indentationCharacter, level, _indentationSize);
 }

 /**
  * Outputs a formatted string to standard output. The "fflush" instruction
  * allows to see the output even close to a failure, because it drops the
  * buffering.
  */

static FILE *outputFile = NULL;

static void openOutputFile() {
    if (outputFile == NULL) {
        const char *outputPath = getOutputPath();
        outputFile = fopen(outputPath, "w");
        if (!outputFile) {
            fprintf(stderr, "No se pudo abrir %s, usando stdout\n", outputPath);
            outputFile = stdout;
        }
    }
}
static void _output(const unsigned int indentationLevel, const char * const format, ...) {
    openOutputFile();

    va_list arguments;
    va_start(arguments, format);
    char * indentation = _indentation(indentationLevel);
    char * effectiveFormat = concatenate(2, indentation, format);
    vfprintf(outputFile, effectiveFormat, arguments);
    fflush(outputFile);
    free(effectiveFormat);
    free(indentation);
    va_end(arguments);
}

/** PUBLIC FUNCTIONS */

void generate(CompilerState * compilerState) {
    logDebugging(_logger, "Generating final output...");
    _generatePrologue();
    _generateProgram(compilerState->abstractSyntaxtTree);
    // 	_generateEpilogue(compilerState->value);
    logDebugging(_logger, "Generation is done.");
}