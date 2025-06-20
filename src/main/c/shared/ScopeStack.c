#include "ScopeStack.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "ScopeStack.h"

ScopeStack* createScopeStack() {
    ScopeStack *stack = malloc(sizeof(ScopeStack));
    stack->size = 1;
    stack->lastScope = 0; // Comienza con el scope 0
    stack->scopes = calloc(MAX_STACK_SIZE,sizeof(Scope) );// Si bien ya inserta el valor 0 inicial, lo voy a hacer explicito
    stack->scopes[0] = 0; // El primer scope es 0
    return stack;
}

void pushScope(ScopeStack *stack) {
    if (stack->size >= MAX_STACK_SIZE) {
        fprintf(stderr, "Scope stack overflow\n");
        return;
    }

    const Scope next = stack->lastScope + 1;
    stack->scopes[stack->size++] = next;
    stack->lastScope = next;
}

Scope popScope(ScopeStack *stack) {
    if (stack->size <= 0) {
        fprintf(stderr, "Scope stack underflow\n");
        return -1; // O un valor que indique error
    }
    const Scope lastScope = stack->scopes[--stack->size ];
    return lastScope;

}

Scope peekScope(ScopeStack *stack) {
    if (stack->size <= 0) {
        return -1; // O un valor que indique error
    }
    return stack->scopes[stack->size - 1];

}

void freeScopeStack(ScopeStack *stack) {
    if (stack) {
        free(stack->scopes);
        free(stack);
    }
}
