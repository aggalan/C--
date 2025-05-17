#ifndef FLEX_ACTIONS_HEADER
#define FLEX_ACTIONS_HEADER

#include "../../shared/Environment.h"
#include "../../shared/Logger.h"
#include "../../shared/String.h"
#include "../../shared/Type.h"
#include "../syntactic-analysis/AbstractSyntaxTree.h"
#include "../syntactic-analysis/BisonParser.h"
#include "LexicalAnalyzerContext.h"
#include <stdio.h>
#include <stdlib.h>

/** Initialize module's internal state. */
void initializeFlexActionsModule();

/** Shutdown module's internal state. */
void shutdownFlexActionsModule();

/**
 * Flex lexeme processing actions.
 */
void BeginSingleLineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndMultilineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void BeginSingleLineLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void EndSingleLineCommentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token BeginStringLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token EndStringLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
void IgnoredLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token ArithmeticOperatorLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);
Token IntegerLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token StringLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token ParenthesisLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);
Token BracketLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);

Token UnknownLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token IdentifierLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token TypeLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);

Token MatchLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext,Token token);
Token ReturnLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token PrintLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token MacroLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token SortLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token DefaultLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token ToLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token ForLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token WhileLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token IfLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token ElseLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token AddOneLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token MinusOneLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token AssignmentLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token IndentationLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
Token ConditionalLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);

Token LogicalOperatorLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);
Token CommaLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);

Token BooleanLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext, Token token);

Token NewLineLexemeAction(LexicalAnalyzerContext * lexicalAnalyzerContext);
#endif
