#ifndef ABSTRACT_SYNTAX_TREE_HEADER
#define ABSTRACT_SYNTAX_TREE_HEADER

#include "../../shared/Logger.h"
#include <stdlib.h>

/** Initialize module's internal state. */
void initializeAbstractSyntaxTreeModule();

/** Shutdown module's internal state. */
void shutdownAbstractSyntaxTreeModule();

/**
 * This typedefs allows self-referencing types.
 */

typedef enum MathExpressionType MathExpressionType;
typedef enum FactorType FactorType;
typedef enum OperatorType OperatorType;
typedef enum ComparatorType ComparatorType;
typedef enum ConditionalType ConditionalType;
typedef struct Constant Constant;
typedef struct MathExpression MathExpression;
typedef struct Factor Factor;
typedef struct Program Program;
typedef char * String;
typedef struct Statement Statement;
typedef struct StatementList StatementList;
typedef struct ForLoop ForLoop;
typedef struct MatchStatement MatchStatement;
typedef struct Case Case;
typedef struct CaseList CaseList;
typedef struct AssignmentMathExpression AssignmentMathExpression;
typedef struct WhileLoop WhileLoop;
typedef struct IfStatement IfStatement;
typedef struct PrintStatement PrintStatement;
typedef struct ConditionalExpression ConditionalExpression;
typedef struct AssignmentExpression AssignmentExpression;
typedef struct BoolExpression BoolExpression;
typedef struct NotConditionalExpression NotConditionalExpression;
typedef struct ParenthesizedConditionalExpression ParenthesizedConditionalExpression;
typedef struct MacroStatement MacroStatement;
typedef struct StringList StringList;
typedef struct Condition Condition;
typedef struct ComparisonExpression ComparisonExpression;
/**
 * Node types for the Abstract Syntax Tree (AST).
 */

struct PrintStatement {
	String identifier;
};



enum ComparatorType {
	EQUAL,
	NOT_EQUAL,
	LESS_THAN,
	LESS_EQUAL,
	GREATER_THAN,
	GREATER_EQUAL
};

enum OperatorType {
	LOGICAL_AND,
	LOGICAL_OR
};
enum ConditionalType {
	CONDITIONAL_NOT,
	PARENTHESIS,
	BOOLEAN,
	OPERATOR,
	CONDITIONAL_IDENTIFIER
};
struct ComparisonExpression {
	MathExpression * math_expression1;
	MathExpression * math_expression2;
	ComparatorType type;
	BoolExpression * boolExpression;
};

struct BoolExpression {
	BoolExpression * boolExpression1;
	BoolExpression * boolExpression2;
	ConditionalType type;
	ComparisonExpression * comparisonExpression;
	MathExpression * math_expression;

};

struct StringList {
    String *strings;
    int count;
};
struct MacroStatement {
	String identifier;
	StringList * parameters;
	Statement *  statement;
};
struct ConditionalExpression {
	BoolExpression * boolExpression;
	String identifier;
};


struct IfStatement {
	ConditionalExpression *condition;
	StatementList *thenBranch;
	StatementList *elseBranch;
};

struct WhileLoop {
	ConditionalExpression *condition;
	StatementList *body;
};


struct Case {
	int matchValue;
	StatementList *body;
};

struct CaseList {
	Case **cases;
	int count;
};

struct MatchStatement {
	String identifier;
	CaseList *caseList;
};

struct Statement {
	enum {
		STATEMENT_MathExpression,
		STATEMENT_FOR,
		STATEMENT_MATCH,
		STATEMENT_WHILE,
		STATEMENT_IF,
    	STATEMENT_PRINT,
    	STATEMENT_MACRO
	} type;
	union {
		MathExpression *mathExpression;
		ForLoop *forLoop;
		MatchStatement *matchStatement;
		WhileLoop *whileLoop;
		IfStatement *ifStatement;
        PrintStatement *printStatement;
        MacroStatement *macroStatement;
	};
};

struct StatementList {
	Statement **statements;
	int count;
};

struct ForLoop {
	AssignmentMathExpression * assignment;
	Constant * endValue;
	StatementList *body;
};

struct Program {
	StatementList *statements;
};

enum MathExpressionType {
	ADDITION,
	DIVISION,
	FACTOR,
	MULTIPLICATION,
	SUBTRACTION
};

struct AssignmentMathExpression {
	String identifier;
	MathExpression *mathExpression;
};



enum FactorType {
	CONSTANT,
	EXPRESSION,
	VARIABLE
};

struct Constant {
	int value;
};

struct Factor {
	union {
		Constant * constant;
		MathExpression * mathExpression;
		String identifier;
	};
	FactorType type;
};

struct MathExpression {
	union {
		Factor * factor;
		struct {
			MathExpression * leftExpression;
			MathExpression * rightExpression;
		};
	};
	MathExpressionType type;
};


/**
 * Node recursive destructors.
 */
void releaseConstant(Constant * constant);
void releaseExpression(MathExpression * MathExpression);
void releaseFactor(Factor * factor);
void releaseProgram(Program * program);
void releaseStatement(Statement *stmt);
void releaseStatementList(StatementList *list);
void releaseForLoop(ForLoop *loop);
void releaseMatchStatement(MatchStatement *match);
void releaseCase(Case *caseNode);
void releaseCaseList(CaseList *caseList);

#endif
