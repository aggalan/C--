#include "SymbolTable.h"

#include "../frontend/syntactic-analysis/SyntacticAnalyzer.h"

SymbolTable* createSymbolTable() {
    SymbolTable *table = malloc(sizeof(SymbolTable));
    table->count = 0;
    return table;
}

Symbol* createSymbol(const char *name, const Type *types, int typeCount) {
    Symbol *symbol = malloc(sizeof(Symbol));
    symbol->name = strdup(name); // duplica el string
    symbol->types = malloc(sizeof(Type) * typeCount);
    for (int i = 0; i < typeCount; i++) {
        symbol->types[i] = types[i]; // asumimos que los Type* ya están en heap
    }
    symbol->typeCount = typeCount;
    return symbol;
}

void addSymbol(SymbolTable *table, const char *name, const Type *types, const int typeCount) {
    if (table->count >= MAX_SYMBOLS) {
        // Manejo simple de error
        currentCompilerState()->succeed= false;
        return;
    }

    Symbol *existing = NULL;
    for (int i = 0; i < table->count; i++) {
        if (strcmp(table->symbols[i]->name, name) == 0) {
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
    table->symbols[table->count++] = newSymbol;
}

Symbol* findSymbol(SymbolTable *table, const char *name) {
    for (int i = 0; i < table->count; i++) {
        if (strcmp(table->symbols[i]->name, name) == 0) {
            return table->symbols[i];
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
