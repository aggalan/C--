%{

#include "FlexActions.h"

%}

/**
 * Enable start condition manipulation functions.
 */
%option stack

/**
 * Flex contexts (a.k.a. start conditions).
 *
 * @see https://westes.github.io/flex/manual/Start-Conditions.html
 */
%x MULTILINE_COMMENT
%x STRING
/**
 * Reusable patterns.
 *
 * @see https://westes.github.io/flex/manual/Matching.html
 * @see https://westes.github.io/flex/manual/Patterns.html
 */

%%

"/*"								{ BEGIN(MULTILINE_COMMENT); BeginMultilineCommentLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>"*/"				{ EndMultilineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL); }
<MULTILINE_COMMENT>[[:space:]]+		{ IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>[^*]+			{ IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<MULTILINE_COMMENT>.				{ IgnoredLexemeAction(createLexicalAnalyzerContext()); }


"\""                                { BEGIN(STRING); BeginStringLexemeAction(createLexicalAnalyzerContext()); }
<STRING>"\""                        { EndStringLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL);}
<STRING>"[^\"]+"                    { StringLexemeAction(createLexicalAnalyzerContext());}

"++"                                { AddOneLexemeAction(createLexicalAnalyzerContext()); }
"--"                                { MinusOneLexemeAction(createLexicalAnalyzerContext()); }

">="                                { ConditionalLexemeAction(createLexicalAnalyzerContext(), GREATER_OR_EQUAL);}
"<="                                { ConditionalLexemeAction(createLexicalAnalyzerContext(), SMALLER_OR_EQUAL);}
"=="                                { ConditionalLexemeAction(createLexicalAnalyzerContext(), CONDITIONAL_EQUAL);}
"!="                                { ConditionalLexemeAction(createLexicalAnalyzerContext(), DIFFERENT);}
"<"                                 { ConditionalLexemeAction(createLexicalAnalyzerContext(), SMALLER);}
">"                                 { ConditionalLexemeAction(createLexicalAnalyzerContext(), GREATER);}

"-"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), SUB); }
"*"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), MUL); }
"/"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), DIV); }
"+"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), ADD); }

"="                                 { return AssignmentOperatorLexemeAction(createLexicalAnalyzerContext()); }

"("									{ return ParenthesisLexemeAction(createLexicalAnalyzerContext(), OPEN_PARENTHESIS); }
")"									{ return ParenthesisLexemeAction(createLexicalAnalyzerContext(), CLOSE_PARENTHESIS); }
"["                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), OPEN_BRACKETS); }
"]"                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), CLOSE_BRACKETS); }

"INT"                               { return TypeLexemeAction(createLexicalAnalyzerContext(), INT);}
"STRING"                            { return TypeLexemeAction(createLexicalAnalyzerContext(), STRING_TYPE);}
"BOOL"                              { return TypeLexemeAction(createLexicalAnalyzerContext(), BOOL);}

"FOR"                               { return ForLexemeAction(createLexicalAnalyzerContext()); }
"WHILE"                             { return WhileLexemeAction(createLexicalAnalyzerContext()); }

"IF"                                { return IfLexemeAction(createLexicalAnalyzerContext()); }
"ELSE"                              { return ElseLexemeAction(createLexicalAnalyzerContext()); }

"MATCH"							    { return MatchLexemeAction(createLexicalAnalyzerContext()); }

[[:alpha:]][[:alnum:]]+             { return IdentifierLexemeAction(createLexicalAnalyzerContext()); }

[[:digit:]]+						{ return IntegerLexemeAction(createLexicalAnalyzerContext()); }

[[:space:]]+						{ IgnoredLexemeAction(createLexicalAnalyzerContext()); }
.									{ return UnknownLexemeAction(createLexicalAnalyzerContext()); }


%%

#include "FlexExport.h"
