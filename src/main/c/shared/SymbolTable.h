#ifndef SYMBOL_TABLE_HEADER
#define SYMBOL_TABLE_HEADER

#include <stdlib.h>
#include <string.h>

#include "Type.h"

#define MAX_SYMBOLS 1024


typedef struct {
    char *name;          // Nombre del símbolo
    Type *types;        // Arreglo de tipos (ver arriba)
    int typeCount;       // Cantidad de tipos
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
