#include "uthash.h"
typedef struct variable Variable;

int getVarType(char* identifier);
void addVar(char* identifier, int type);

Variable* symbolTable = NULL;

struct variable{
    UT_hash_handle hh;
    int type; //0 INT, 1 BOOL
    char* identifier;
};

void addVar(char* identifier, int type){
    Variable* v = (Variable*) malloc(sizeof(Variable));
    v->identifier = identifier;
    v->type = type;
    HASH_ADD_KEYPTR(hh, symbolTable, v->identifier, strlen(v->identifier), v);
}

int getVarType(char* identifier){
    Variable* v;
    HASH_FIND_STR(symbolTable, identifier, v);
    if(v) return v->type;
    return -1;
}