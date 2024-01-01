%{
#include <stdio.h>
#include<stdlib.h>
extern int yylex(void);
int yyerror(char *);
%}

//Symbols
%union
{
    char *sval;
    int ival;
};
%token <sval> VAR
%token <sval> NUM
%token <sval> BOOLLIT
%token <sval> IDENTIFIER
%token <sval> OP2
%token <sval> OP3
%token <sval> OP4
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

%start Program
%%
Program:
    PROGRAM Declarations BEGINN 
    StatementSequence END
    ;

Declarations:
    %empty
    | VAR IDENTIFIER AS Type SC Declarations
    ;

Type:
    INT
    | BOOL
    ;

StatementSequence:
    %empty
    | Statement SC StatementSequence
    ;

Statement:
    Assignment
    | IfStatement
    | WhileStatement
    | WriteInt
    ;

Assignment:
    IDENTIFIER ASGN Expression
    | IDENTIFIER ASGN READINT
    ;

IfStatement:
    IF Expression THEN StatementSequence ElseClause END
    ;

ElseClause:
    %empty
    | ELSE StatementSequence
    ;

WhileStatement:
    WHILE Expression DO StatementSequence END
    ;

WriteInt:
    WRITEINT Expression
    ;

Expression:
    SimpleExpression
    | SimpleExpression OP4 SimpleExpression
    ;

SimpleExpression:
    Term OP3 Term
    | Term
    ;

Term:
    Factor OP2 Factor
    | Factor
    ;

Factor:
    IDENTIFIER
    | NUM
    | BOOLLIT
    | LP Expression RP
    ;
%%

int yyerror(char *s){
    printf("\nInvalid Syntax : %s\n",s);
    exit(-1);
}
int main(void) {
    yyparse();
    printf("SUCCESS\n");
}

int yywrap(){

}