#include "FlexActions.h"

/* MODULE INTERNAL STATE */

static Logger * _logger = NULL;
static boolean _logIgnoredLexemes = true;

void initializeFlexActionsModule() {
	_logIgnoredLexemes = getBooleanOrDefault("LOG_IGNORED_LEXEMES", _logIgnoredLexemes);
	_logger = createLogger("FlexActions");
}

void shutdownFlexActionsModule() {
	if (_logger != NULL) {
		destroyLogger(_logger);
	}
}

/* PRIVATE FUNCTIONS */

static void _logLexicalAnalyzerContext(const char * functionName, LexicalAnalyzerContext * lexicalAnalyzerContext);

/**
 * Logs a lexical-analyzer context in DEBUGGING level.
 */
static void _logLexicalAnalyzerContext(const char * functionName, LexicalAnalyzerContext * lexicalAnalyzerContext) {
	char * escapedLexeme = escape(lexicalAnalyzerContext->lexeme);
	logDebugging(_logger, "%s: %s (context = %d, length = %d, line = %d)",
		functionName,
		escapedLexeme,
		lexicalAnalyzerContext->currentContext,
		lexicalAnalyzerContext->length,
		lexicalAnalyzerContext->line);
	free(escapedLexeme);
}

/* PUBLIC FUNCTIONS */
typedef struct {
	unsigned int stack[255];
	int last;
} IndentStack;

IndentStack indentStack = {{0}, 0};

Token IndentationLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);

	if (lexicalAnalyzerContext->length > indentStack.stack[indentStack.last]) {
		indentStack.stack[++indentStack.last] = lexicalAnalyzerContext->length;
		lexicalAnalyzerContext->semanticValue->token = INDENT;
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
		return INDENT;
	}
	if ( lexicalAnalyzerContext->length < indentStack.stack[indentStack.last]) {
		indentStack.last--;
		lexicalAnalyzerContext->semanticValue->token = DEDENT;
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
		return DEDENT;
	}
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return IGNORE;
}

void BeginMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	if (_logIgnoredLexemes) {
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	}
}

void EndMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	if (_logIgnoredLexemes) {
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	}
}
void BeginSingleLineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	if (_logIgnoredLexemes) {
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	}
}

void EndSingleLineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	if (_logIgnoredLexemes) {
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	}
}

Token BeginStringLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	if (_logIgnoredLexemes) {
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	}

}

Token EndStringLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	if (_logIgnoredLexemes) {
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	}
}

void IgnoredLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	if (_logIgnoredLexemes) {
		_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
        destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	}
}

Token ArithmeticOperatorLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

Token IntegerLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->integer = atoi(lexicalAnalyzerContext->lexeme);
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return INTEGER;
}
Token StringLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->string =strdup(lexicalAnalyzerContext->lexeme);
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return STRING;
}
Token ParenthesisLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

Token BracketLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}


Token IdentifierLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->string = strdup(lexicalAnalyzerContext->lexeme);
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return GENERIC_ID;
}

Token TypeLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

Token AddOneLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = ADD_ONE;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return ADD_ONE;
}

Token MinusOneLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = MINUS_ONE;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return MINUS_ONE;
}

Token ConditionalLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}
Token LogicalOperatorLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}


Token UnknownLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return UNKNOWN;

}
Token MatchLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

Token AssignmentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = ASSIGNMENT;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return ASSIGNMENT;
}
Token ReturnLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = RETURN;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return RETURN;
}

Token ForLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = FOR;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return FOR;
}

Token WhileLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = WHILE;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return WHILE;
}

Token IfLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = IF;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return IF;
}


Token ElseLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = ELSE;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return ELSE;
}

Token PrintLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext){
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = PRINT;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return PRINT;
}
Token MacroLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext){
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = MACRO;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return MACRO;
}
Token SortLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext){
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = SORT;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return SORT;
}
Token DefaultLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext){
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = DEFAULT;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return DEFAULT;
}
Token ToLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext){
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = TO;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return TO;
}
Token BooleanLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = token;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}
Token CommaLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext){
    _logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
    lexicalAnalyzerContext->semanticValue->token = COMMA;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
    return COMMA;
}
Token NewLineLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = NEW_LINE;
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return NEW_LINE;
}
Token TypedIdentifierLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->string = strdup(lexicalAnalyzerContext->lexeme);
    destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return token;
}

Token AscLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = ASC;
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return ASC;
}
Token DescLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext) {
	_logLexicalAnalyzerContext(__FUNCTION__, lexicalAnalyzerContext);
	lexicalAnalyzerContext->semanticValue->token = DESC;
	destroyLexicalAnalyzerContext(lexicalAnalyzerContext);
	return DESC;
}
