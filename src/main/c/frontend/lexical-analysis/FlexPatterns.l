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

\"                          { BEGIN(STRING);  BeginStringLexemeAction(createLexicalAnalyzerContext()); }
<STRING>\"                  {  BEGIN(INITIAL);  EndStringLexemeAction(createLexicalAnalyzerContext()); }
<STRING>[^\\"\n]+           { return StringLexemeAction(createLexicalAnalyzerContext()); }

"//"                                { BEGIN(SINGLE_LINE_COMMENT); BeginSingleLineCommentLexemeAction(createLexicalAnalyzerContext()); }
"#"                                 { BEGIN(SINGLE_LINE_COMMENT); BeginSingleLineCommentLexemeAction(createLexicalAnalyzerContext());}
<SINGLE_LINE_COMMENT>[^\n]*       { IgnoredLexemeAction(createLexicalAnalyzerContext()); }
<SINGLE_LINE_COMMENT>\n           { EndSingleLineCommentLexemeAction(createLexicalAnalyzerContext()); BEGIN(INITIAL);}

\n[[:space:]]*			            { return NewLineLexemeAction(createLexicalAnalyzerContext()); }
"}"[[:space:]]*\n                  { return BracketLexemeAction(createLexicalAnalyzerContext(), CLOSE_BRACE); }

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

"true"                              { return BooleanLexemeAction(createLexicalAnalyzerContext(), TRUE); }
"false"                             { return BooleanLexemeAction(createLexicalAnalyzerContext(), FALSE); }
"TRUE"                              { return BooleanLexemeAction(createLexicalAnalyzerContext(), TRUE); }
"FALSE"                             { return BooleanLexemeAction(createLexicalAnalyzerContext(), FALSE); }

"->"								{ return MatchLexemeAction(createLexicalAnalyzerContext(),ARROW); }

"="                                 { return AssignmentLexemeAction(createLexicalAnalyzerContext()); }

"("									{ return ParenthesisLexemeAction(createLexicalAnalyzerContext(), OPEN_PARENTHESIS); }
")"									{ return ParenthesisLexemeAction(createLexicalAnalyzerContext(), CLOSE_PARENTHESIS); }
"["                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), OPEN_BRACKETS); }
"]"                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), CLOSE_BRACKETS); }
"}"                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), CLOSE_BRACE); }
"{"                                 { return BracketLexemeAction(createLexicalAnalyzerContext(), OPEN_BRACE); }
"INT"                               { return TypeLexemeAction(createLexicalAnalyzerContext(), INT);}
"STRING"                            { return TypeLexemeAction(createLexicalAnalyzerContext(), STRING_TYPE);}
"BOOL"                              { return TypeLexemeAction(createLexicalAnalyzerContext(), BOOL);}
"VOID"                              { return TypeLexemeAction(createLexicalAnalyzerContext(), VOID);}

"FOR"                               { return ForLexemeAction(createLexicalAnalyzerContext()); }
"WHILE"                             { return WhileLexemeAction(createLexicalAnalyzerContext()); }
","                                  {return CommaLexemeAction(createLexicalAnalyzerContext());}
"IF"                                { return IfLexemeAction(createLexicalAnalyzerContext()); }
"ELSE"                              { return ElseLexemeAction(createLexicalAnalyzerContext()); }
"TO"                                { return ToLexemeAction(createLexicalAnalyzerContext()); }
"MATCH"							    { return MatchLexemeAction(createLexicalAnalyzerContext(),MATCH); }
"PRINT"						        { return PrintLexemeAction(createLexicalAnalyzerContext()); }
"MACRO"                             { return MacroLexemeAction(createLexicalAnalyzerContext()); }
"RETURN"						    { return ReturnLexemeAction(createLexicalAnalyzerContext()); }
"SORT"                              { return SortLexemeAction(createLexicalAnalyzerContext()); }
"ASC"                               { return AscLexemeAction(createLexicalAnalyzerContext()); }
"DESC"                              { return DescLexemeAction(createLexicalAnalyzerContext()); }
"DEFAULT"                           { return DefaultLexemeAction(createLexicalAnalyzerContext()); }

"int"[[:alnum:]]*		            { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), INT_ID); }
"string"[[:alnum:]]*	            { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), STRING_ID); }
"bool"[[:alnum:]]*	                { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), BOOL_ID); }
"arr_int"[[:alnum:]]*	            { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), INT_ARRAY_ID); }
"arr_string"[[:alnum:]]*	        { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), STRING_ARRAY_ID); }
"arr_bool"[[:alnum:]]*	        { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), BOOL_ARRAY_ID); }
"func_int"[[:alnum:]]*	            { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), INT_FUNCTION_ID); }
"func_string"[[:alnum:]]*	        { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), STRING_FUNCTION_ID); }
"func_bool"[[:alnum:]]*	        { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), BOOL_FUNCTION_ID); }
"macro_"[[:alnum:]]*	            { return TypedIdentifierLexemeAction(createLexicalAnalyzerContext(), MACRO_ID); }
[[:alpha:]][[:alnum:]]*             { return IdentifierLexemeAction(createLexicalAnalyzerContext()); }

[[:digit:]]+						{ return IntegerLexemeAction(createLexicalAnalyzerContext()); }

[ \t\r]+						{ IgnoredLexemeAction(createLexicalAnalyzerContext()); }
.									{ return UnknownLexemeAction(createLexicalAnalyzerContext()); }

%%

#include "FlexExport.h"
