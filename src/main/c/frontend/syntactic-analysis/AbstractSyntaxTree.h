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
typedef enum StatementType StatementType;
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
typedef struct SortStatement SortStatement;
typedef struct MacroStatement MacroStatement;
typedef struct StringList StringList;
typedef struct FunctionStatement FunctionStatement;
typedef struct ReturnStatement ReturnStatement;
typedef struct FunctionDefinition FunctionDefinition;
/**
 * Node types for the Abstract Syntax Tree (AST).
 */

struct FunctionDefinition {
    String identifier;
    StringList * parameters;
    StatementList * body;
};

struct PrintStatement {
	String identifier;
};

struct SortStatement {
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

struct FunctionStatement {
    StringList * stringList;
    String identifier;
};

struct ReturnStatement {
    Expression expression;
    FunctionStatement * functionStatement
};

struct BoolExpression {
	union {
		struct {
			MathExpression * math_expression1;
			MathExpression * math_expression2;
			ComparatorType comparatorType;
		};
		String identifier;
	};
	enum {
		BOOL_IDENTIFIER,
		MATH_DERIVED
	} type;

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
	union {
		struct {
			ConditionalExpression * condition1;
			ConditionalExpression * condition2;
			OperatorType operatorType;
		};
		ConditionalExpression * conditional_expression;
		ConditionalExpression * parenthesized_conditional_expression;
		BoolExpression * bool_expression;
		String identifier;
	} ;

	ConditionalType type;

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

enum StatementType {
	STATEMENT_MATH_EXPRESSION,
	STATEMENT_FOR,
	STATEMENT_MATCH,
	STATEMENT_WHILE,
	STATEMENT_IF,
	STATEMENT_PRINT,
	STATEMENT_SORT,
    STATEMENT_MACRO,
    STATEMENT_FUNCTION,
    STATEMENT_RETURN
} ;

struct Statement {
	StatementType type;
	union {
		MathExpression *mathExpression;
		ForLoop *forLoop;
		MatchStatement *matchStatement;
		WhileLoop *whileLoop;
		IfStatement *ifStatement;
        PrintStatement *printStatement;
		SortStatement * sort_statement;
        MacroStatement *macroStatement;
        FunctionStatement * functionStatement;
        ReturnStatement * returnStatement;
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
    FunctionDefinition * functionDefinition;
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
