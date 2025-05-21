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
typedef enum MathOperatorType MathOperatorType;
typedef enum FactorType FactorType;
typedef enum OperatorType OperatorType;
typedef enum ComparatorType ComparatorType;
typedef enum ConditionalType ConditionalType;
typedef enum StatementType StatementType;
typedef enum ElseType ElseType;
typedef struct Constant Constant;
typedef struct Factor Factor;
typedef struct BoolFactor BoolFactor;
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
typedef struct AssignmentMathStatement AssignmentMathStatement;
typedef struct AssignmentStringStatement AssignmentStringStatement;
typedef struct AssignmentBoolStatement AssignmentBoolStatement;
typedef struct Expression Expression;
typedef struct NotConditionalExpression NotConditionalExpression;
typedef struct ParenthesizedConditionalExpression ParenthesizedConditionalExpression;
typedef struct BoolExpression BoolExpression;
typedef struct MathExpression MathExpression;
typedef struct StringExpression StringExpression;
typedef struct Argument Argument;
typedef struct ArgumentList ArgumentList;
typedef struct ArgumentValue ArgumentValue;
typedef struct ArgumentNode ArgumentNode;
typedef struct ArgumentDef ArgumentDef;
typedef struct ArgumentDefNode ArgumentDefNode;
typedef struct ArgumentDefList ArgumentDefList;
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
typedef struct ArrayAccess ArrayAccess;
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
	_VOID,
	_INT_ARRAY,
	_STRING_ARRAY,
	_BOOL_ARRAY,
};

struct ArrayStatement {
	String identifier;
	IntList * elements;
};
struct ArrayAccess {
	String identifier;
	MathExpression * index;
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
    ArgumentDefList * parameters;
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
    ArgumentList * parameters;
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
struct StringExpression {
	union {
		String identifier;
		String string;
		ArrayAccess * arrayAccess;
	};
	enum {
		STRING_IDENTIFIER_EXPRESSION,
		STRING_VALUE_EXPRESSION,
		STRING_EXPRESSION_ARRAY,
	}type;
};

struct ArgumentValue {
	union {
		String identifier;
		MathExpression * mathExpression;
		BoolExpression * boolExpression;
		StringExpression * stringExpression;
		ArrayAccess * arrayAccess;
		FunctionStatement * functionExpression;
		UnaryChangeOperatorStatement * unaryChangeOperatorStatement;
	};
	enum {
		ARGUMENT_MATH_EXPRESSION,
		ARGUMENT_BOOL_EXPRESSION,
		ARGUMENT_STRING_EXPRESSION,
		ARGUMENT_FUNCTION_EXPRESSION,
		ARGUMENT_BOOL_ARRAY_ID,
		ARGUMENT_STRING_ARRAY_ID,
		ARGUMENT_INT_ARRAY_ID,
		ARGUMENT_ARRAY_ACCESS,
		ARGUMENT_UNARY_CHANGE_OPERATOR,
	} type;
};

struct ArgumentNode {
    ArgumentValue * argument ;
	ArgumentNode *next;
};

struct StatementNode {
    Statement * statement;
    StatementNode *next;
};

struct ArgumentDef {
	String identifier;
	enum {
		ARGUMENT_MATH,
		ARGUMENT_BOOL,
		ARGUMENT_STRING,
		ARGUMENT_INT_ARRAY,
		ARGUMENT_STRING_ARRAY,
		ARGUMENT_BOOL_ARRAY,
	} type;
};

struct ArgumentDefNode {
	ArgumentDef * argumentDef;
	 ArgumentDefNode *next;
};
struct ArgumentDefList {
	ArgumentDefNode * arguments;
	ArgumentDefNode *last;
	int count;
};


struct StringNode {
	String string;
	StringNode *next;
};

struct StringList {
	StringNode *strings;
	StringNode *last;
};

struct ArgumentList {
    ArgumentNode *arguments;
    ArgumentNode *last;
	int count;
};

struct MacroStatement {
	String identifier;
	StringList * parameters;
	Statement *  statement;
};


struct IfStatement {
	BoolExpression *condition;
	StatementBlock *thenBranch;
	ElseStatement *elseBranch;
};

struct WhileLoop {
	BoolExpression *condition;
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
		SortStatement * sortStatement;
		AssignmentStatement * assignmentStatement;
        MacroStatement *macroStatement;
        FunctionStatement * functionStatement;
        ReturnStatement * returnStatement;
		VariableStatement * variableStatement;
		UnaryChangeOperatorStatement * unaryChangeOperatorStatement;
		ArrayStatement * arrayStatement;
	};
};
struct AssignmentStatement {
	enum {
		MATH_ASSIGNMENT,
		STRING_ASSIGNMENT,
		BOOL_ASSIGNMENT,
		ARRAY_ASSIGNMENT
	}type;
	union {
		AssignmentMathStatement * mathAssignment;
		AssignmentStringStatement * stringAssignment;
		AssignmentBoolStatement * boolAssignment;
		ArrayStatement * arrayAssignment;
	};
};
struct AssignmentMathStatement {
	MathExpression * mathExpression;
	String identifier;
};
struct AssignmentBoolStatement {
	BoolExpression * expression;
	String identifier;
};
struct AssignmentStringStatement {
	String expression;
	String identifier;
};

struct StatementList {
	StatementNode * statements;
    StatementNode * last;
	int count;
};

struct ForLoop {
	AssignmentMathStatement * assignment;
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
	enum {
		VARIABLE,
		ARRAY
	} type;

	union {
		String identifier;
		ArrayAccess * arrayAccess;
	};

	enum {
		PRE_INCREMENT,
		PRE_DECREMENT,
		POST_INCREMENT,
		POST_DECREMENT
	} operator_type;
};

struct Program {
	Unit * unit;
	enum {
		EMPTY,
		NOT_EMPTY
	} type;
};

enum MathOperatorType {
	ADDITION,
	DIVISION,
	FACTOR,
	MULTIPLICATION,
	SUBTRACTION
};
enum MathExpressionType {
	FACTOR_EXPRESSION,
	OPERATOR_EXPRESSION,
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
	ARRAY_FACTOR,
	UNARY_CHANGE_FACTOR,
};

struct Constant {
	int value;
};
struct MathExpression {
	MathExpressionType type;
	union {
			Factor * factor;
			struct {
				MathExpression * leftExpression;
				MathExpression * rightExpression;
				MathOperatorType mathType;
			};
	};

};
struct BoolExpression {
	union {
		struct {
			MathExpression * expression1;
			MathExpression * expression2;
			ComparatorType comparatorType;
		}comparatorExpression;
		struct {
			BoolExpression * expression1;
			BoolExpression * expression2;
			OperatorType operatorType;
		}logicalExpression;
		BoolFactor * boolFactor;
	};
	enum {
		COMPARATOR_EXPRESSION,
		LOGICAL_EXPRESSION,
		BOOL_FACTOR
	} type;
};
struct BoolFactor {
	union {
		BoolExpression * expression;
		Bool boolean;
		String identifier;
		FunctionStatement * functionStatement;
		ArrayAccess * arrayAccess;
	};
	enum {
		NOT_EXPRESSION,
		PARENTHESIS_EXPRESSION,
		BOOLEAN_ID,
		BOOL_CONSTANT,
		BOOL_FUNCTION,
		BOOL_ARRAY
	} type;
};

struct Expression {
	enum {
		BOOLEAN_EXPRESSION,
		MATH_EXPRESSION,
		STRING_EXPRESSION,
	} type;
	union {
		MathExpression * mathExpression;
		StringExpression * stringExpression;
		BoolExpression * boolExpression;
};
};

struct Factor {
	union {
		Constant * constant;
		MathExpression * expression;
		String identifier;
		Bool boolean;
		FunctionStatement * functionStatement;
		ArrayAccess * arrayAccess;
		UnaryChangeOperatorStatement * unaryChangeOperatorStatement;
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
void releaseCase(Case *c);
void releaseCaseList(CaseList *caseList);
/**
 * Additional node recursive destructors.
 */
 void releaseMathExpression(MathExpression * mathExpression);
void releaseIfStatement(IfStatement * ifStatement);
void releaseElseStatement(ElseStatement * elseStatement);
void releaseWhileLoop(WhileLoop * whileLoop);
void releasePrintStatement(PrintStatement * printStatement);
void releaseSortStatement(SortStatement * sortStatement);
void releaseMacroStatement(MacroStatement * macroStatement);
void releaseFunctionStatement(FunctionStatement * functionStatement);
void releaseReturnStatement(ReturnStatement * returnStatement);
void releaseAssignmentStatement(AssignmentStatement * assignmentStatement);
void releaseAssignmentMathStatement(AssignmentMathStatement * assignmentMathStatement);
void releaseAssignmentStringStatement(AssignmentStringStatement * assignmentStringStatement);
void releaseAssignmentBoolStatement(AssignmentBoolStatement * assignmentBoolStatement);
void releaseUnaryChangeOperatorStatement(UnaryChangeOperatorStatement * unaryChangeOperatorStatement);
void releaseVariableStatement(VariableStatement * variableStatement);
void releaseArrayStatement(ArrayStatement * arrayStatement);
void releaseBoolExpression(BoolExpression * boolExpression);
void releaseBoolFactor(BoolFactor * boolFactor);
void releaseStringList(StringList * stringList);
void releaseFunctionDefinition(FunctionDefinition * functionDefinition);
void releaseStatementBlock(StatementBlock * statementBlock);
void releaseArrayAccess(ArrayAccess * arrayAccess);
void releaseIntList(IntList * intList);
void releaseUnit(Unit * unit);
void releaseExternalDeclaration(ExternalDeclaration * externalDeclaration);
void releaseIntNode(IntNode * node);
void releaseStringNode(StringNode * node);
void releaseStatementNode(StatementNode * node);
void releaseCaseNode(CaseNode * node);
void releaseArgumentValue(ArgumentValue * argumentValue);
void releaseArgumentList(ArgumentList * argumentList);
void releaseArgumentNode(ArgumentNode * argumentNode);
void releaseArgumentDef(ArgumentDef * argumentDef);
void releaseArgumentDefNode(ArgumentDefNode * argumentDefNode);
void releaseArgumentDefList(ArgumentDefList * argumentDefList);



#endif
