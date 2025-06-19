#ifndef SYMBOL_TABLE_HEADER
#define SYMBOL_TABLE_HEADER

#include "Type.h"

#define MAX_SYMBOLS 1024


typedef struct {
    char *name;          // Nombre del símbolo
    Type *types;        // Arreglo de tipos (ver arriba)
    int typeCount;       // Cantidad de tipos
    Scope scope;         // Alcance del símbolo (local, global, etc.)
} Symbol;

typedef struct {
    Symbol *symbols[MAX_SYMBOLS];
    int count;
} SymbolTable;

SymbolTable* createSymbolTable();
Symbol* createSymbol(const char *name, const Type *types, int typeCount);
void addSymbol(SymbolTable *table, const char *name, const Type *types, int typeCount);
Symbol* findSymbol(SymbolTable *table, const char *name);
void freeSymbolTable(SymbolTable *table);


#endif
