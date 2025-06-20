#ifndef COMPILER_STATE_HEADER
#define COMPILER_STATE_HEADER

#include "ScopeStack.h"
#include "Type.h"
#include "SymbolTable.h"
/**
 * The general status of a compilation.
*/
typedef enum {
	SUCCEED = 0,
	FAILED = 1
} CompilationStatus;

/**
 * The global state of the compiler. Should transport every data structure
 * needed across the different phases of a compilation.
 */
typedef struct {
	// The root node of the AST.
	void * abstractSyntaxtTree;

	// A flag that indicates the current state of the compilation so far.
	boolean succeed;
	boolean hasError;
	// TODO: Add an stack to handle nested scopes.

	ScopeStack * scopeStack;
	SymbolTable * symbolTable;
	// TODO: Add configuration.
	// TODO: ...
	ReturnList * returnList;
	boolean declarationMode;
	// The computed value of the entire program (only for the calculator).
	int value;
} CompilerState;

#endif
