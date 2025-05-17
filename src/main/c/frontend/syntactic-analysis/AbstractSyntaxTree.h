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

typedef enum Type Type;
typedef enum MathExpressionType MathExpressionType;
typedef enum FactorType FactorType;
typedef enum OperatorType OperatorType;
typedef enum ComparatorType ComparatorType;
typedef enum ConditionalType ConditionalType;
typedef enum StatementType StatementType;
typedef enum ElseType ElseType;
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
typedef struct FunctionStatement FunctionStatement;
typedef struct ReturnStatement ReturnStatement;
typedef struct FunctionDefinition FunctionDefinition;
typedef struct Unit Unit;
typedef struct ExternalDeclaration ExternalDeclaration;
typedef struct ElseStatement ElseStatement;
typedef struct UnaryChangeOperatorStatement UnaryChangeOperatorStatement;
typedef struct StatementBlock StatementBlock;
typedef struct VariableStatement VariableStatement;
typedef struct ArrayStatement ArrayStatement;
typedef struct IntList IntList;
typedef struct StringNode StringNode;
typedef struct IntNode IntNode;
typedef struct StatementNode StatementNode;
typedef struct CaseNode CaseNode;
/**
 * Node types for the Abstract Syntax Tree (AST).
 */


enum Type {
	_INT,
	_STRING,
	_BOOL,
	_VOID
};

struct ArrayStatement {
	String identifier;
	IntList * elements;
};

struct IntNode {
    int integer;
    IntNode *next;
};

struct IntList {
    IntNode *integers;
    IntNode *last;
	int count;
};

struct VariableStatement {
	String identifier;
	Type type;
	Expression * expression;
};

struct FunctionDefinition {
    String identifier;
	Type type;
    StringList * parameters;
    StatementBlock * body;
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
    StringList * parameters;
    String identifier;
};

struct ReturnStatement {
	union {
		Expression * expression;
		FunctionStatement * functionStatement;
	};
	enum {
		RETURN_EXPRESSION,
		RETURN_FUNCTION_STATEMENT,
		RETURN_EMPTY
	}type;
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
struct StringNode {
    String string;
     StringNode *next;
};

struct StatementNode {
    Statement * statement;
    StatementNode *next;
};

struct StringList {
    StringNode *strings;
    StringNode *last;
	int count;
};

struct MacroStatement {
	String identifier;
	StringList * parameters;
	Statement *  statement;
};


struct IfStatement {
	Expression *condition;
	StatementBlock *thenBranch;
	ElseStatement *elseBranch;
};

struct WhileLoop {
	Expression *condition;
	StatementBlock *body;
};


struct Case {
	int matchValue;
	Statement *body;
    String string;
    enum {
        DEFAULT_CASE,
        INTEGER_CASE,
        STRING_CASE
    } type;
};

struct CaseNode {
    Case * Case;
    CaseNode *next;
};
struct CaseList {
	CaseNode *cases;
    CaseNode *last;
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
    STATEMENT_RETURN,
	STATEMENT_ASSIGNMENT,
	STATEMENT_UNARY_CHANGE_OPERATOR,
	STATEMENT_VARIABLE,
	STATEMENT_ARRAY,
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
        FunctionStatement * functionStatement;
        ReturnStatement * returnStatement;
		VariableStatement * variableStatement;
		UnaryChangeOperatorStatement * unaryChangeOperatorStatement;
		ArrayStatement * arrayStatement;
	};
};
struct AssignmentStatement {
	Expression * expression;
	String identifier;
};

struct StatementList {
	StatementNode * statements;
    StatementNode * last;
	int count;
};

struct ForLoop {
	AssignmentStatement * assignment;
	Constant * endValue;
	StatementBlock *body;
};

enum ElseType {
    ELSE_STATEMENT,
    ELSE_IF_STATEMENT,
};

struct ElseStatement {
    StatementBlock *body;
    ElseType type;
    IfStatement * elseIfStatement;
};

struct UnaryChangeOperatorStatement {
	String identifier;
	enum {
		PRE_INCREMENT,
		PRE_DECREMENT,
		POST_INCREMENT,
		POST_DECREMENT
	} type;
};

struct Program {
	Unit * unit;
	enum {
		EMPTY,
		NOT_EMPTY
	} type;
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
	BOOLEAN_FACTOR,
	FUNCTION,
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
		FunctionStatement * functionStatement;
	};
	FactorType type;
};
struct StatementBlock {
	StatementList * statementList;
};

struct Unit {
	Unit *units;
	ExternalDeclaration *externalDeclaration;
	enum {
		SINGLE,
		NODE,
		NEW_LINE_UNIT
	}type;
};
struct ExternalDeclaration {
	FunctionDefinition * functionDefinition;
	Statement * statement;
	enum {
		FUNCTION_DEFINITION,
		STATEMENT
	} type;
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
