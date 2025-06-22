#ifndef SYMBOL_TABLE_HEADER
#define SYMBOL_TABLE_HEADER

#include "Type.h"

#define MAX_SYMBOLS 1024

void initializeTableActionsModule();

void shutdownTableActionsModule();
typedef struct {
    char *name;          // Nombre del símbolo
    Type *types;        // Arreglo de tipos (En funciones, primer valor es el tipo de retorno)
    int typeCount;       // Cantidad de tipos
    Scope scope;         // Alcance del símbolo (local, global, etc.)
    SymbolType symbolType;      // Función, variable, macro.
} Symbol;

typedef struct {
    Symbol *symbols[MAX_SYMBOLS];
    Symbol * currentFunction;
    int count;
} SymbolTable;

SymbolTable* createSymbolTable();
Symbol* createSymbol(const char *name, const Type *types, int typeCount, SymbolType symbolType);
void addSymbol(SymbolTable *table, const char *name, const Type *types, const int typeCount,SymbolType symbolType);
Symbol* findSymbol(SymbolTable *table, const char *name);
void freeSymbolTable(SymbolTable *table);


#endif
