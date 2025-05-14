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
typedef struct ConditionalMathExpression ConditionalMathExpression;
typedef struct WhileLoop WhileLoop;
typedef struct IfStatement IfStatement;

/**
 * Node types for the Abstract Syntax Tree (AST).
 */

struct IfStatement {
	MathExpression *condition;
	StatementList *thenBranch;
	StatementList *elseBranch;
};

struct WhileLoop {
	MathExpression *condition;
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
		STATEMENT_IF
	} type;
	union {
		MathExpression *mathExpression;
		ForLoop *forLoop;
		MatchStatement *matchStatement;
		WhileLoop *whileLoop;
		IfStatement *ifStatement;
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
	EXPRESSION
};

struct Constant {
	int value;
};

struct Factor {
	union {
		Constant * constant;
		MathExpression * mathExpression;
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
