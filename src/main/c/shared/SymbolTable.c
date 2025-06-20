#include "SymbolTable.h"

#include "ScopeStack.h"
#include "../frontend/syntactic-analysis/SyntacticAnalyzer.h"

SymbolTable* createSymbolTable() {
    SymbolTable *table = malloc(sizeof(SymbolTable));
    table->count = 0;
    return table;
}

ReturnList* createReturnList() {
    ReturnList *list = malloc(sizeof(ReturnList));
    list->head = NULL;
    list->count = 0;
    return list;
}
ReturnNode* createReturnNode(Type type) {
    ReturnNode *node = malloc(sizeof(ReturnNode));
    node->type = type;
    return node;
}
ReturnNode* addReturnNode(ReturnList *list, Type type) {
    if (list->count >= MAX_SYMBOLS) {
        currentCompilerState()->succeed = false;
        return NULL;
    }

    ReturnNode *newNode = createReturnNode(type);
    newNode->next = list->head; // Inserta al principio
    list->head = newNode;
    list->count++;
    return newNode;
}
int isReturnListEmpty(const ReturnList *list) {
    return list->count == 0;
}
void destroyReturnList(ReturnList *list) {
    if (!list) return;

    ReturnNode *current = list->head;
    while (current) {
        ReturnNode *next = current->next;
        free(current);
        current = next;
    }

    list->head = NULL;
}


Symbol* createSymbol(const char *name, const Type *types, const int typeCount) {
    Symbol *symbol = malloc(sizeof(Symbol));
    symbol->name = strdup(name); // duplica el string
    symbol->types = malloc(sizeof(Type) * typeCount);
    symbol->scope = peekScope(currentCompilerState()->scopeStack) ; // Asigna el scope actual
    for (int i = 0; i < typeCount; i++) {
        symbol->types[i] = types[i]; // asumimos que los Type* ya están en heap
    }
    symbol->typeCount = typeCount;
    return symbol;
}

void addSymbol(SymbolTable *table, const char *name, const Type *types, const int typeCount,int function) {
    if (table->count >= MAX_SYMBOLS) {
        // Manejo simple de error
        currentCompilerState()->succeed= false;
        return;
    }
    Symbol *existing = NULL;
    for (int i = 0; i < table->count; i++) {
        if (strcmp(table->symbols[i]->name, name) == 0
            && table->symbols[i]->scope == peekScope(currentCompilerState()->scopeStack)) {
            existing = table->symbols[i];
            break;
        }
    }
    if (existing != NULL) {
        // Ya existe, podrías actualizar o emitir error
        fprintf(stderr, "Symbol %s already defined\n", name);
        currentCompilerState()->succeed = false; // Marca el estado de compilación como fallido
        return;
    }

    Symbol *newSymbol = createSymbol(name, types, typeCount);
    if (function == 1) {
        table->currentFunction = newSymbol;
        newSymbol->isFunction = 1;
    }
    if (function == 2) {
        newSymbol->isFunction = 2; // Macro
    }
    table->symbols[table->count++] = newSymbol;
}

Symbol* findSymbol(SymbolTable *table, const char *name) {
    for (int i = table->count -1 ; i >= 0; i--) {
        if ( table->symbols[i]->scope <= peekScope(currentCompilerState()->scopeStack) && // Verifica el scope
            strcmp(table->symbols[i]->name, name) == 0) {
            for (int j=currentCompilerState()->scopeStack->size -1 ; j >= 0 ; j--) {
                if (table->symbols[i]->scope == currentCompilerState()->scopeStack->scopes[j]) {
                    // Si el scope coincide, devuelve el símbolo
                    return table->symbols[i];
                }
            }
        }
    }
    return NULL;
}
void freeSymbolTable(SymbolTable *table) {
    if (!table) return;

    for (int i = 0; i < table->count; i++) {
        Symbol *symbol = table->symbols[i];
        if (symbol) {
            free(symbol->name);          // strdup
            free(symbol->types);         // malloc de types
            free(symbol);                // el propio símbolo
        }
    }

    free(table); // finalmente la tabla
}
