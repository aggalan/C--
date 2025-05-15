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
%x SINGLE_LINE_COMMENT
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

\"                          { BEGIN(STRING); return BeginStringLexemeAction(createLexicalAnalyzerContext()); }
<STRING>\"                  {  BEGIN(INITIAL); return EndStringLexemeAction(createLexicalAnalyzerContext()); }
<STRING>[^\\"\n]+           { return StringLexemeAction(createLexicalAnalyzerContext()); }

"//"                                { BEGIN(SINGLE_LINE_COMMENT); BeginSingleLineCommentLexemeAction(createLexicalAnalyzerContext()); }
"#"                                 { BEGIN(SINGLE_LINE_COMMENT); BeginSingleLineCommentLexemeAction(createLexicalAnalyzerContext());}
<SINGLE_LINE_COMMENT>[^\n]*       { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<SINGLE_LINE_COMMENT>\n           { EndSingleLineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL);}

\r\n[ ]*                            { Token token = IndentationLexemeAction(createLexicalAnalyzerContext()); if(token != IGNORE) return token; }
\n[ ]*                            { Token token = IndentationLexemeAction(createLexicalAnalyzerContext()); if(token != IGNORE) return token; }

"++"                                { return AddOneLexemeAction(createLexicalAnalyzerContext()); }
"--"                                { return MinusOneLexemeAction(createLexicalAnalyzerContext()); }

">="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), GTE);}
"<="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), LTE);}
"=="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), EQ);}
"!="                                { return ConditionalLexemeAction(createLexicalAnalyzerContext(), NEQ);}
"<"                                 { return ConditionalLexemeAction(createLexicalAnalyzerContext(), LT);}
">"                                 { return ConditionalLexemeAction(createLexicalAnalyzerContext(), GT);}

"AND"                               { return LogicalOperatorLexemeAction(createLexicalAnalyzerContext(), AND); }
"OR"                                { return LogicalOperatorLexemeAction(createLexicalAnalyzerContext(), OR); }
"NOT"                               { return LogicalOperatorLexemeAction(createLexicalAnalyzerContext(), NOT); }

"-"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), SUB); }
"*"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), MUL); }
"/"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), DIV); }
"+"									{ return ArithmeticOperatorLexemeAction(createLexicalAnalyzerContext(), ADD); }

"->"								{ return MatchLexemeAction(createLexicalAnalyzerContext(),ARROW); }

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
"TO"                                { return ToLexemeAction(createLexicalAnalyzerContext()); }
"MATCH"							    { return MatchLexemeAction(createLexicalAnalyzerContext(),MATCH); }
"PRINT"						        { return PrintLexemeAction(createLexicalAnalyzerContext()); }
"MACRO"                             { return MacroLexemeAction(createLexicalAnalyzerContext()); }
"RETURN"						    { return ReturnLexemeAction(createLexicalAnalyzerContext()); }
"SORT"                              { return SortLexemeAction(createLexicalAnalyzerContext()); }
"DEFAULT"                           { return DefaultLexemeAction(createLexicalAnalyzerContext()); }

[[:alpha:]][[:alnum:]]*             { return IdentifierLexemeAction(createLexicalAnalyzerContext()); }

[[:digit:]]+						{ return IntegerLexemeAction(createLexicalAnalyzerContext()); }

[[:space:]]+						{ IgnoredLexemeAction(createLexicalAnalyzerContext()); }
.									{ return UnknownLexemeAction(createLexicalAnalyzerContext()); }

%%

#include "FlexExport.h"
