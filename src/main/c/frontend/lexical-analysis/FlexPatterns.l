%{

#include "FlexActions.h"

%}

/**
 * Enable start condition manipulation functions.
 */
%option stack
%option bol

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
<STRING>"[^\"]+"                    { return StringLexemeAction(createLexicalAnalyzerContext());}

"^[ ]*"                            { return IndentationLexemeAction(createLexemeAction()); }

"++"                                { return AddOneLexemeAction(createLexicalAnalyzerContext()); }
"--"                                { return MinusOneLexemeAction(createLexicalAnalyzerContext()); }

">="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), GREATER_OR_EQUAL);}
"<="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), SMALLER_OR_EQUAL);}
"=="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), CONDITIONAL_EQUAL);}
"!="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), DIFFERENT);}
"<"                                 { return ConditionalLexemeAction(createLexicalAnalyzerContext(), SMALLER);}
">"                                 { return ConditionalLexemeAction(createLexicalAnalyzerContext(), GREATER);}

"AND"                               { return LogicalOperatorLexemeAction(createLexicalAnalyzerContext(), AND); }
"OR"                                { return LogicalOperatorLexemeAction(createLexicalAnalyzerContext(), OR); }
"NOT"                               { return LogicalOperatorLexemeAction(createLexicalAnalyzerContext(), NOT); }

"-"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), SUB); }
"*"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), MUL); }
"/"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), DIV); }
"+"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), ADD); }

"="                                 { return AssignmentLexemeAction(createLexicalAnalyzerContext()); }

"("									{ return ParenthesisLexemeAction(createLexicalAnalyzerContext(), OPEN_PARENTHESIS); }
")"									{ return ParenthesisLexemeAction(createLexicalAnalyzerContext(), CLOSE_PARENTHESIS); }
"["                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), OPEN_BRACKETS); }
"]"                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), CLOSE_BRACKETS); }

"INT"                               { return TypeLexemeAction(createLexicalAnalyzerContext(), INT);}
"STRING"                            { return TypeLexemeAction(createLexicalAnalyzerContext(), STRING_TYPE);}
"BOOL"                              { return TypeLexemeAction(createLexicalAnalyzerContext(), BOOL);}
"VOID"                              { return TypeLexemeAction(createLexicalAnalyzerContext(), VOID);}

"FOR"                               { return ForLexemeAction(createLexicalAnalyzerContext()); }
"WHILE"                             { return WhileLexemeAction(createLexicalAnalyzerContext()); }

"IF"                                { return IfLexemeAction(createLexicalAnalyzerContext()); }
"ELSE"                              { return ElseLexemeAction(createLexicalAnalyzerContext()); }

"MATCH"							    { return MatchLexemeAction(createLexicalAnalyzerContext(),MATCH); }

"RETURN"						    { return ReturnLexemeAction(createLexicalAnalyzerContext()); }

[[:alpha:]][[:alnum:]]+             { return IdentifierLexemeAction(createLexicalAnalyzerContext()); }

[[:digit:]]+						{ return IntegerLexemeAction(createLexicalAnalyzerContext()); }

[[:space:]]+						{ IgnoredLexemeAction(createLexicalAnalyzerContext()); }
.									{ return UnknownLexemeAction(createLexicalAnalyzerContext()); }

"->"								{ return MatchLexemeAction(createLexicalAnalyzerContext(),ARROW); }
%%

#include "FlexExport.h"
