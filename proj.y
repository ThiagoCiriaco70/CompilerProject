%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashfuncs.h"
#include "uthash.h"

extern int yylex(void);
char errorStr[1000] = "";
int errorFlag = 0;
int errorNum = 1;
int yyerror();
%}

%code requires{

    #include "structs.h"
    

    void gencode(void* decl, void* stmtseq);
    void genDecls(int indent, DeclNode* declN);
    void pIndent(int inden);
    void genType(TypeNode* typeN);
    void genStmtseq(int indent, StmtSeqNode* stmtSeqNo);
    void genStmt(int indent, StmtNode* stmtNo);
    void genAssmt(int indent, AssnmtNode* assmtN);
    void genExpr(ExpressionNode* exprN);
    void genSExp(SimpExpressionNode* sexprN);
    void genTerm(TermNode* tNode);
    void genFactor(FactorNode* facN);
    void genIf(int indent, IfstmtNode* ifstmtN);
    void genElse(int indent, ElseNode* elseN);
    void genWhile(int indent, WhileStmtNode* whileN);
    void genWrInt(int indent, WrIntNode* wrIntN);

    void error(char* message);
    
    int getType(void* tNode);
    int getExprType(void* expNode);
    int getFactorType(void* facNode);
    int getTermType(void* termNode);
    int getSExprType(void* sExprNode);

}

//Symbols
%union
{
    char *sval;
    int ival;
    Node* nodeptr;
};

%token <ival> NUM
%token <sval> BOOLLIT
%token <sval> IDENTIFIER
%token <sval> OP2
%token <sval> OP3
%token <sval> OP4
%token VAR
%token WRITEINT
%token READINT
%token IF
%token THEN
%token ELSE
%token BEGINN
%token END
%token WHILE
%token DO
%token PROGRAM
%token INT
%token AS
%token BOOL
%token LP
%token RP
%token ASGN
%token SC

%type<nodeptr> Program
%type<nodeptr> Declarations
%type<nodeptr> Type
%type<nodeptr> StatementSequence
%type<nodeptr> Statement
%type<nodeptr> Assignment
%type<nodeptr> IfStatement
%type<nodeptr> WhileStatement
%type<nodeptr> ElseClause
%type<nodeptr> WriteInt
%type<nodeptr> Expression
%type<nodeptr> SimpleExpression
%type<nodeptr> Term
%type<nodeptr> Factor

%parse-param {Node* program}

%start Program
%%
Program:
    PROGRAM Declarations BEGINN 
    StatementSequence END   
        {
            ProgramNode* sptr = malloc(sizeof(ProgramNode));
            sptr->decl = $2;
            sptr->stmtseq = $4;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = ProgramN;
            ptr->nodePtr = sptr;
            $$ = ptr;
            program = $$;
            gencode($2, $4);
        }
    ;

Declarations:
    %empty  
        {
            DeclNode* sptr = malloc(sizeof(DeclNode));
            sptr->subtype = 0;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = declN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | VAR IDENTIFIER AS Type SC Declarations
        {
            DeclNode* sptr = malloc(sizeof(DeclNode));
            sptr->subtype = 1;
            sptr->identifier = $2;
            sptr->typeN = $4;
            sptr->decls = $6;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = declN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

Type:
    INT 
        {   
            TypeNode* sptr = malloc(sizeof(TypeNode));
            sptr->subtype = 0;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = typeN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | BOOL 
        {
            TypeNode* sptr = malloc(sizeof(TypeNode));
            sptr->subtype = 1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = typeN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

StatementSequence:
    %empty 
        {
            StmtSeqNode* sptr = malloc(sizeof(StmtSeqNode));
            sptr->subtype = 0;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = stmtSeqN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | Statement SC StatementSequence
        {
            StmtSeqNode* sptr = malloc(sizeof(StmtSeqNode));
            sptr->subtype = 1;
            sptr->stmt = $1;
            sptr->stmtseq = $3;

            Node* ptr = malloc(sizeof(Node));
            ptr->type = stmtSeqN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

Statement:
    Assignment
        {
            StmtNode* sptr = malloc(sizeof(StmtNode));
            sptr->subtype = 0;
            sptr->assmt = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = stmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | IfStatement
        {
            StmtNode* sptr = malloc(sizeof(StmtNode));
            sptr->subtype = 1;
            sptr->ifstmt = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = stmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | WhileStatement
        {
            StmtNode* sptr = malloc(sizeof(StmtNode));
            sptr->subtype = 2;
            sptr->whileStmt = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = stmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | WriteInt
        {
            StmtNode* sptr = malloc(sizeof(StmtNode));
            sptr->subtype = 3;
            sptr->wrInt = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = stmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

Assignment:
    IDENTIFIER ASGN Expression
        {
            AssnmtNode* sptr = malloc(sizeof(AssnmtNode));
            sptr->identifier = $1;
            sptr->expr = $3;
            sptr->subtype = 0;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = assnmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | IDENTIFIER ASGN READINT
        {
            AssnmtNode* sptr = malloc(sizeof(AssnmtNode));
            sptr->identifier = $1;
            sptr->subtype = 1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = assnmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

IfStatement:
    IF Expression THEN StatementSequence ElseClause END
        {
            IfstmtNode* sptr = malloc(sizeof(IfstmtNode));
            sptr->expr = $2;
            sptr->stmtseq = $4;
            sptr->elsestmt = $5;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = ifstmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

ElseClause:
    %empty
        {
            ElseNode* sptr = malloc(sizeof(ElseNode));
            sptr->subtype = 0;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = elseN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | ELSE StatementSequence
        {
            ElseNode* sptr = malloc(sizeof(ElseNode));
            sptr->subtype = 1;
            sptr->stmtseq = $2;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = elseN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

WhileStatement:
    WHILE Expression DO StatementSequence END
        {
            WhileStmtNode* sptr = malloc(sizeof(WhileStmtNode));
            sptr->expr = $2;
            sptr->stmtseq = $4;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = whileStmtN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

WriteInt:
    WRITEINT Expression 
        {
            WrIntNode* sptr = malloc(sizeof(WrIntNode));
            sptr->expr = $2;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = wrIntN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

Expression:
    SimpleExpression
        {
            ExpressionNode* sptr = malloc(sizeof(ExpressionNode));
            sptr->subtype = 0;
            sptr->sExp1 = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = expressionN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | SimpleExpression OP4 SimpleExpression
        {
            ExpressionNode* sptr = malloc(sizeof(ExpressionNode));
            sptr->subtype = 1;
            sptr->sExp1 = $1;
            sptr->op4 = $2;
            sptr->sExp2 = $3;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = expressionN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

SimpleExpression:
    Term OP3 Term
        {
            SimpExpressionNode* sptr = malloc(sizeof(SimpExpressionNode));
            sptr->subtype = 0;
            sptr->term1 = $1;
            sptr->op3 = $2;
            sptr->term2 = $3;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = simpExpressionN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | Term
        {
            SimpExpressionNode* sptr = malloc(sizeof(SimpExpressionNode));
            sptr->subtype = 1;
            sptr->term1 = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = simpExpressionN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

Term:
    Factor OP2 Factor 
        {
            TermNode* sptr = malloc(sizeof(TermNode));
            sptr->subtype = 0;
            sptr->fac1 = $1;
            sptr->op2 = $2;
            sptr->fac2 = $3;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = termN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | Factor
        {
            TermNode* sptr = malloc(sizeof(TermNode));
            sptr->subtype = 1;
            sptr->fac1 = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = termN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;

Factor:
    IDENTIFIER
        {
            FactorNode* sptr = malloc(sizeof(FactorNode));
            sptr->subtype = 0;
            sptr->identifier = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = factorN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | NUM
        {
            FactorNode* sptr = malloc(sizeof(FactorNode));
            sptr->subtype = 1;
            sptr->num = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = factorN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | BOOLLIT
        {
            FactorNode* sptr = malloc(sizeof(FactorNode));
            sptr->subtype = 2;
            sptr->boollit = $1;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = factorN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    | LP Expression RP
        {
            FactorNode* sptr = malloc(sizeof(FactorNode));
            sptr->subtype = 3;
            sptr->expr = $2;
            Node* ptr = malloc(sizeof(Node));
            ptr->type = factorN;
            ptr->nodePtr = sptr;
            $$ = ptr;
        }
    ;
%%

int yyerror(){
    printf("\nInvalid Syntax\n");
    exit(-1);
}
int main(void) {

    /*#ifdef YYDEBUG
        yydebug = 1;
    #endif*/

    Node* programPtr;
    yyparse(programPtr);
    //printf("SUCCESS\n");

    //check hash to see if working
    /*Variable *v, *tmp = NULL;
    HASH_ITER(hh, symbolTable, v, tmp) {
        printf("%i", getVarType(v->identifier));
    }*/

    if(errorFlag){
        printf("%s", errorStr);
        printf("\n");
    }
}

void pIndent(int indent){
    for(int i = 0; i < indent; i++){
        printf("   ");
    }
}

void gencode(void* decl, void* stmtseq){

    int indent = 0;
    Node* node2 = (Node*)decl;
    DeclNode* declN = (DeclNode*) node2->nodePtr;

    Node* node4 = (Node*)stmtseq;
    StmtSeqNode* stmtSeqN = (StmtSeqNode*) node4->nodePtr;

    printf("#include<stdio.h>\n#include<stdlib.h>\nint main(void) {\n");
    
    genDecls(indent + 1, declN);
    genStmtseq(indent + 1, stmtSeqN);

    printf("\n}\n");
}

void genDecls(int indent, DeclNode* declN){

    int subtype = declN->subtype;

    if(subtype == 0) { //EMPTY DECL
        return;
    } else {
        char* identifier = declN->identifier;
        Node* node1 = declN->typeN;
        TypeNode* typeN = (TypeNode*) node1->nodePtr;
        Node* node2 = declN->decls;
        DeclNode* decl2N = (DeclNode*) node2->nodePtr;

        int type = typeN->subtype;
        char* iden_copy = malloc(strlen(identifier) + 1);
        strcpy(iden_copy, identifier);
        if(getVarType(identifier) == -1){
            addVar(identifier, type);
        } else {
            error("Error: Variable already defined. >> ");
            strcat(errorStr, iden_copy);
        }

        pIndent(indent);
        genType(typeN);
        printf(" %s", iden_copy);
        printf(";\n");

        

        genDecls(indent, decl2N);
        free(iden_copy);
    }
}

void genType(TypeNode* typeN){

    int subtype = typeN->subtype;

    if(subtype == 1){
        printf("bool");
    } else { 
        printf("int");
    }
}

int getType(void* t) {
    Node* nP = (Node*) t;
    TypeNode* tN = (TypeNode*) nP;
    return tN->subtype;
}

int getExprType(void* expNode){

    Node* eN = (Node*) expNode;
    ExpressionNode* exprN = (ExpressionNode*) eN->nodePtr;
    int subtype = exprN->subtype;
    if(subtype == 0){
        return getSExprType(exprN->sExp1);
    } else {
        int simp1type = getSExprType(exprN->sExp1);
        int simp2type = getSExprType(exprN->sExp2);
        if(simp1type != simp2type){
            error("Error: Comparing values of types bool and int when evaluating expression.");
            return 1;
        }
        if(simp1type == 1 && simp2type == 1){
            char* op4 = exprN->op4;
            if (!((strcmp(op4, "=") == 0) || (strcmp(op4, "!=") == 0))){
                error("Error: Comparing values of types bool with operator ");
                strcat(errorStr, op4);
                return 1;
            }
        }
        return 1; //1 meaning boolean
    }
}

int getFactorType(void* facNode){
    Node* fN = (Node*) facNode;
    FactorNode* fNode = (FactorNode*) fN->nodePtr;
    int subtype = fNode->subtype;
    if(subtype == 0){
        return getVarType(fNode->identifier); 
    } else if(subtype == 1){
        return 0; //0 meaning number
    } else if(subtype == 2){
        return 1; //1 meaning boolean
    } else {
        return getExprType(fNode->expr);
    }
}

int getTermType(void* termNode){
    Node* tN = (Node*) termNode;
    TermNode* termN = (TermNode*) tN->nodePtr;
    int subtype = termN->subtype;
    if(subtype == 0){
        return 0; //0 meaning number
    } else {
        return getFactorType(termN->fac1);
    }
}

int getSExprType(void* sExprNode){
    Node* seN = (Node*) sExprNode;
    SimpExpressionNode* sexpN = (SimpExpressionNode*) seN->nodePtr;
    int subtype = sexpN->subtype;
    
    if(subtype == 0){
        return 0; //0 meaning number
    } else {
        return getTermType(sexpN->term1);
    }
}

void genStmtseq(int indent, StmtSeqNode* stmtSeqNo){
    int subtype = stmtSeqNo->subtype;
    if(subtype == 0){
        return;
    } else {
        Node* node1 = stmtSeqNo->stmt;
        Node* node3 = stmtSeqNo->stmtseq;
        StmtNode* stmtNo = (StmtNode*) node1->nodePtr;
        StmtSeqNode* stmtseq2 = (StmtSeqNode*) node3->nodePtr;
        genStmt(indent, stmtNo);
        genStmtseq(indent, stmtseq2);
    }
}

void genStmt(int indent, StmtNode* stmtNo){
    int subtype = stmtNo->subtype;
    if(subtype == 0){ //assignment
        Node* node1 = stmtNo->assmt;
        AssnmtNode* assmtN = (AssnmtNode*) node1->nodePtr;
        genAssmt(indent, assmtN);

    } else if(subtype == 1){ //ifstmt
        Node* node1 = stmtNo->ifstmt;
        IfstmtNode* ifstmtN = (IfstmtNode*) node1->nodePtr;
        genIf(indent, ifstmtN);

    } else if(subtype == 2){ //whilestmt
        Node* node1 = stmtNo->whileStmt;
        WhileStmtNode* whileN = (WhileStmtNode*) node1->nodePtr;
        genWhile(indent, whileN);

    } else if(subtype ==3){ //writeint
        Node* node1 = stmtNo->wrInt;
        WrIntNode* wrIntN = (WrIntNode*) node1->nodePtr;
        genWrInt(indent, wrIntN);
    }
}

void genAssmt(int indent, AssnmtNode* assmtN){
    
    int subtype = assmtN->subtype;
    char* identifier = assmtN->identifier;
    char* iden_copy = malloc(strlen(identifier) + 1);
    strcpy(iden_copy, identifier);
    int varType = getVarType(identifier);
    
    if(varType == -1){
        error("Error: Variable not defined. >> ");
        strcat(errorStr, iden_copy);
    }
    
    if(subtype == 0){
        Node* node1 = assmtN->expr;
        ExpressionNode* exN = (ExpressionNode*) node1->nodePtr;
        int expType = getExprType(node1);

        pIndent(indent);
        printf("%s = ", iden_copy);
        genExpr(exN);
        printf(";\n");
        
        if(varType == 1 && expType == 0){
            error("Error: Type mismatch in assignment with boolean variable and non-boolean expression.");
        }
        
        if(varType == 0 && expType == 1){
            error("Error: Type mismatch in assignment with integer variable and boolean expression.");
        }
        
    } else if(subtype == 1){
        pIndent(indent);
        printf("scanf(\"%%d\", &%s);\n", iden_copy);
    }
    free(iden_copy);
}

void genExpr(ExpressionNode* exprN){
    int subtype = exprN->subtype;
    Node* node1 = exprN->sExp1;
    SimpExpressionNode* sExprN1 = (SimpExpressionNode*) node1->nodePtr;
    
    if(subtype == 0){
        genSExp(sExprN1);
    } else if(subtype == 1){
        Node* node2 = exprN->sExp2;
        SimpExpressionNode* sExprN2 = (SimpExpressionNode*) node2->nodePtr;
        const char* op4 = exprN->op4;
        const char* newop4 = op4;

        if(strcmp(op4, "=") == 0){
            newop4 = "==";
        }

        printf("(");
        genSExp(sExprN1);
        printf(" %s ", newop4);
        genSExp(sExprN2);
        printf(")");
    }
}

void genSExp(SimpExpressionNode* sexprN){
    int subtype = sexprN->subtype;
    Node* node1 = sexprN->term1;
    TermNode* term1 = (TermNode*) node1->nodePtr;
    
    if(subtype == 1){
        genTerm(term1);
    } else if(subtype == 0){
        Node* node2 = sexprN->term2;
        TermNode* term2 = (TermNode*) node2->nodePtr;
        char* op3 = sexprN->op3;

        printf("(");
        genTerm(term1);
        printf(" %s ", op3);
        genTerm(term2);
        printf(")");
    }
}

void genTerm(TermNode* tNode){
    int subtype = tNode->subtype;
    Node* node1 = tNode->fac1;
    FactorNode* fac1 = (FactorNode*) node1->nodePtr;
    
    if(subtype == 1){
        genFactor(fac1);
    } else if(subtype == 0){
        Node* node2 = tNode->fac2;
        FactorNode* fac2 = (FactorNode*) node2->nodePtr;
        const char* op2 = tNode->op2;
        const char* newop2 = op2;

        if(strcmp(op2, "div") == 0){
            newop2 = "/";
        } else if(strcmp(op2, "mod") == 0){
            newop2 = "%";
        }


        printf("(");
        genFactor(fac1);
        printf(" %s ", newop2);
        genFactor(fac2);
        printf(")");
    }
}

void genFactor(FactorNode* facN){
    int subtype = facN->subtype;
    
    if(subtype == 0){ //identifier
        char* identifier = facN->identifier;
        char* iden_copy = malloc(strlen(identifier) + 1);
        strcpy(iden_copy, identifier);
        if(getVarType(identifier) == -1){
            error("Error: Variable not defined. >> ");
            strcat(errorStr, facN->identifier);
        }
        printf("%s",iden_copy);
        free(iden_copy);
    } else if(subtype == 1){ //num
        printf("%i",facN->num);
    } else if (subtype == 2){ //boollit
        printf("%s",facN->boollit);
    } else if (subtype == 3){ //(expr)
        Node* node1 = facN->expr;
        ExpressionNode* expr = (ExpressionNode*) node1->nodePtr;
        printf("(");
        genExpr(expr);
        printf(")");
    }
}

void genIf(int indent, IfstmtNode* ifstmtN){
    Node* node1 = ifstmtN->expr;
    Node* node2 = ifstmtN->stmtseq;
    Node* node3 = ifstmtN->elsestmt;
    ExpressionNode* exprN = (ExpressionNode*) node1->nodePtr;
    StmtSeqNode* stmtseqN = (StmtSeqNode*) node2->nodePtr;
    ElseNode* elseN = (ElseNode*) node3->nodePtr;

    if(getExprType(node1) != 1){
        error("Error: If statement expression is not a boolean value.");
    }

    pIndent(indent);
    printf("if(");
    genExpr(exprN);
    printf(") {\n");
    genStmtseq(indent+1, stmtseqN);
    pIndent(indent);
    printf("}");
    genElse(indent, elseN);
}

void genElse(int indent, ElseNode* elseN){
    int subtype = elseN->subtype;
    if(subtype == 1){
        Node* node1 = elseN->stmtseq;
        StmtSeqNode* stmtseqN = (StmtSeqNode*) node1->nodePtr;
        printf(" else {\n");
        genStmtseq(indent+1, stmtseqN);
        pIndent(indent);
        printf("}\n");
    } else {
        printf("\n");
    }
}

void genWhile(int indent, WhileStmtNode* whileN){
    Node* node1 = whileN->expr;
    Node* node2 = whileN->stmtseq;
    ExpressionNode* exprN = (ExpressionNode*) node1->nodePtr;
    StmtSeqNode* stmtseqN = (StmtSeqNode*) node2->nodePtr;

    if(getExprType(node1) != 1){
        error("Error: While statement expression is not a boolean value.");
    }

    pIndent(indent);
    printf("while(");
    genExpr(exprN);
    printf(") {\n");
    genStmtseq(indent+1, stmtseqN);
    pIndent(indent);
    printf("}\n");
}

void genWrInt(int indent, WrIntNode* wrIntN){
    Node* node1 = wrIntN->expr;
    ExpressionNode* exprN = (ExpressionNode*) node1->nodePtr;

    if(getExprType(node1) != 0){
        error("Error: WriteInt statement expression is not an integer value.");
    }

    pIndent(indent);
    printf("printf(\"%%i\\n\", ");
    genExpr(exprN);
    printf(");\n"); 
}

void error(char* message){
    printf("[%i]", errorNum);
    errorFlag = 1;

    strcat(errorStr, "\n[");
    char* errorNumStr = (char*) malloc(64);
    sprintf(errorNumStr, "%i", errorNum);
    strcat(errorStr, errorNumStr);
    strcat(errorStr, "]");
    strcat(errorStr, message);

    errorNum++; 
}


int yywrap(){

}