#ifndef SCOPE_STACK_HEADER
#define SCOPE_STACK_HEADER
#define MAX_STACK_SIZE 1024
#include "Type.h"

typedef struct {
    Scope * scopes;
    int size;
    Scope lastScope;
} ScopeStack;

ScopeStack* createScopeStack();
void pushScope(ScopeStack *stack);
Scope popScope(ScopeStack *stack);
Scope peekScope(ScopeStack *stack);
void clearScope(ScopeStack *stack);
void freeScopeStack(ScopeStack *stack);

#endif