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
typedef struct Factor Factor;
typedef struct Program Program;
typedef char * String;
typedef int Bool;
typedef struct Statement Statement;
typedef struct StatementList StatementList;
typedef struct ForLoop ForLoop;
typedef struct MatchStatement MatchStatement;
typedef struct Case Case;
typedef struct CaseList CaseList;
typedef struct AssignmentConditionalExpression AssignmentConditionalExpression;
typedef struct AssignmentStringExpression AssignmentStringExpression;
typedef struct WhileLoop WhileLoop;
typedef struct IfStatement IfStatement;
typedef struct PrintStatement PrintStatement;
typedef struct AssignmentStatement AssignmentStatement;
typedef struct Expression Expression;
typedef struct NotConditionalExpression NotConditionalExpression;
typedef struct ParenthesizedConditionalExpression ParenthesizedConditionalExpression;
typedef struct SortStatement SortStatement;
typedef struct MacroStatement MacroStatement;
typedef struct StringList StringList;
/**
 * Node types for the Abstract Syntax Tree (AST).
 */

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


struct IfStatement {
	Expression *condition;
	StatementList *thenBranch;
	StatementList *elseBranch;
};

struct WhileLoop {
	Expression *condition;
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
	STATEMENT_ASSIGNMENT,
    STATEMENT_MACRO
} ;

struct Statement {
	StatementType type;
	union {
		Expression *mathExpression;
		ForLoop *forLoop;
		MatchStatement *matchStatement;
		WhileLoop *whileLoop;
		IfStatement *ifStatement;
        PrintStatement *printStatement;
		SortStatement * sort_statement;
		AssignmentStatement * assignment_statement;
        MacroStatement *macroStatement;
	};
};
struct AssignmentStatement {
	Expression * expression;
	String identifier;
};

struct StatementList {
	Statement **statements;
	int count;
};

struct ForLoop {
	AssignmentStatement * assignment;
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
	Expression *mathExpression;
};
struct AssignmentStringExpression {
	String identifier;
	String expression;
};

struct AssignmentConditionalExpression {
	String identifier;
	Expression *condition;
};



enum FactorType {
	CONSTANT,
	EXPRESSION,
	FACTOR_IDENTIFIER,
	BOOLEAN_FACTOR
};

struct Constant {
	int value;
};
struct Expression {
	enum {
		CONDITIONAL_EXPRESSION,
		MATH_EXPRESSION,
		STRING_EXPRESSION
	} type;
	union {
		struct {
			union {
				Factor * factor;
				struct {
					Expression * leftExpression;
					Expression * rightExpression;
				};
			};
			MathExpressionType mathType;
		} math_expression;
		String string_expression;
		struct {
			union {
				struct {
					Expression * condition1;
					Expression * condition2;
					OperatorType operatorType;
				};
				Expression * conditional_expression;
				Expression * parenthesized_conditional_expression;
				struct {
					union {
						struct {
							Expression * math_expression1;
							Expression * math_expression2;
							ComparatorType comparatorType;
						};
					};
				} bool_expression;
			} ;

			ConditionalType type;
		} conditional_expression;
};
};

struct Factor {
	union {
		Constant * constant;
		Expression * expression;
		String identifier;
		Bool boolean;
	};
	FactorType type;
};


/**
 * Node recursive destructors.
 */
void releaseConstant(Constant * constant);
void releaseExpression(Expression * MathExpression);
void releaseFactor(Factor * factor);
void releaseProgram(Program * program);
void releaseStatement(Statement *stmt);
void releaseStatementList(StatementList *list);
void releaseForLoop(ForLoop *loop);
void releaseMatchStatement(MatchStatement *match);
void releaseCase(Case *caseNode);
void releaseCaseList(CaseList *caseList);

#endif
