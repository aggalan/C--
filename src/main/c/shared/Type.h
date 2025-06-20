#ifndef TYPE_HEADER
#define TYPE_HEADER

typedef enum {
	false = 0,
	true = 1
} boolean;

typedef enum {
	_INT,
	_STRING,
	_BOOL,
	_VOID,
	_INT_ARRAY,
	_STRING_ARRAY,
	_BOOL_ARRAY,
	_MACRO
}Type ;
typedef int Token;
typedef int Scope;

#endif
