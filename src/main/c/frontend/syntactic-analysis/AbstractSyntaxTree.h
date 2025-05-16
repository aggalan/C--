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
typedef enum BooleanOperator BooleanOperator;
typedef enum ComparatorType ComparatorType;
typedef enum BooleanType BooleanType;
typedef enum BoolConstant BoolConstant;
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
typedef struct BoolExpression BoolExpression;
typedef struct AssignmentExpression AssignmentExpression;
typedef struct BoolExpression BoolExpression;
typedef struct NotConditionalExpression NotConditionalExpression;
typedef struct ParenthesizedConditionalExpression ParenthesizedConditionalExpression;
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
	GREATER_EQUAL,
};

enum BooleanOperator {
	LOGICAL_AND,
	LOGICAL_OR,
  LOGICAL_NOT,
};

enum BooleanType {
	MATH_BINARY,
	BOOL_BINARY,
	BOOL_UNARY,
  BOOL_CONSTANT,
};

enum BoolConstant { LOGICAL_TRUE, LOGICAL_FALSE };

struct BoolExpression {
	BooleanType type;
	union {
		struct {
			MathExpression * math_exp_1;
			MathExpression * math_exp_2;
			ComparatorType comparatorType;
		};
		struct {
			BoolExpression * bool_exp_1;
			BoolExpression * bool_exp_2;
			BooleanOperator operatorTypeBinary;
		};
		struct {
			BoolExpression * bool_exp;
			BooleanOperator operatorTypeUnary;
		};
    BoolConstant constant;
	} ;
};


struct IfStatement {
	BoolExpression *condition;
	StatementList *thenBranch;
	StatementList *elseBranch;
};

struct WhileLoop {
	BoolExpression *condition;
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
    	STATEMENT_PRINT
	} type;
	union {
		MathExpression *mathExpression;
		ForLoop *forLoop;
		MatchStatement *matchStatement;
		WhileLoop *whileLoop;
		IfStatement *ifStatement;
        PrintStatement *printStatement;
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
	FACTOR_IDENTIFIER
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
