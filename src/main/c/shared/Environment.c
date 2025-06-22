#include "Environment.h"
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
/* PUBLIC FUNCTIONS */

const boolean getBooleanOrDefault(const char * name, const boolean defaultValue) {
	const char * value = getStringOrDefault(name, NULL);
	if (value == NULL) {
		return defaultValue;
	}
	else if (strcmp(value, "true") == 0) {
		return true;
	}
	else {
		return false;
	}
}

const char * getStringOrDefault(const char * name, const char * defaultValue) {
	const char * value = getenv(name);
	if (value == NULL) {
		return defaultValue;
	}
	else {
		return value;
	}
}


const char* getOutputPath(void) {
	return getStringOrDefault("OUTPUT_PATH", "./salida/salida.c");
}