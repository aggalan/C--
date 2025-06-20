#ifndef SYMBOL_TABLE_HEADER
#define SYMBOL_TABLE_HEADER

#include "Type.h"

#define MAX_SYMBOLS 1024

void initializeTableActionsModule();

void shutdownTableActionsModule();
typedef struct {
    char *name;          // Nombre del símbolo
    Type *types;        // Arreglo de tipos (ver arriba)---Si es una variable, array de largo 1, si es una función, el primero es el tipo de retorno y el resto los parámetros
    int typeCount;       // Cantidad de tipos
    Scope scope;         // Alcance del símbolo (local, global, etc.)
    int isFunction;      // Cantidad de tipos
} Symbol;

typedef struct {
    Symbol *symbols[MAX_SYMBOLS];
    Symbol * currentFunction;
    int count;
} SymbolTable;
typedef struct ReturnNode {
    Type type;
     struct ReturnNode *next;
} ReturnNode;
typedef struct {
    ReturnNode *head; // Puntero al primer nodo de la lista
    int count;
} ReturnList;
ReturnList* createReturnList();
ReturnNode* createReturnNode(Type type);
ReturnNode* addReturnNode(ReturnList *list, Type type);
int isReturnListEmpty(const ReturnList *list);
void destroyReturnList(ReturnList *list);
SymbolTable* createSymbolTable();
Symbol* createSymbol(const char *name, const Type *types, int typeCount);
void addSymbol(SymbolTable *table, const char *name, const Type *types, const int typeCount,int function);
Symbol* findSymbol(SymbolTable *table, const char *name);
void freeSymbolTable(SymbolTable *table);


#endif
