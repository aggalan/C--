#include "SymbolTable.h"

#include "ScopeStack.h"
#include "../frontend/syntactic-analysis/SyntacticAnalyzer.h"
static Logger * _logger = NULL;

void initializeTableActionsModule() {
    _logger = createLogger("Table");
}

void shutdownTableActionsModule() {
    if (_logger != NULL) {
        destroyLogger(_logger);
    }
}

SymbolTable* createSymbolTable() {
    SymbolTable *table = malloc(sizeof(SymbolTable));
    table->count = 0;
    return table;
}


Symbol* createSymbol(const char *name, const Type *types, const int typeCount, SymbolType symbolType) {
    Symbol *symbol = malloc(sizeof(Symbol));
    symbol->name = strdup(name); // duplica el string
    symbol->types = malloc(sizeof(Type) * typeCount);
    symbol->scope = symbolType == VARIABLE_SYMBOL ? peekScope(currentCompilerState()->scopeStack) : 0;
    for (int i = 0; i < typeCount; i++) {
        symbol->types[i] = types[i]; // asumimos que los Type* ya están en heap
    }
    symbol->typeCount = typeCount;
    symbol->isFunction = symbolType;

    return symbol;
}

void addSymbol(SymbolTable *table, const char *name, const Type *types, const int typeCount,SymbolType symbolType) {
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

    Symbol *newSymbol = createSymbol(name, types, typeCount, symbolType);
    if (symbolType == FUNCTION_SYMBOL) {
        table->currentFunction = newSymbol;
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
