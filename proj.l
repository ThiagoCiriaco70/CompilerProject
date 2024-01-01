/*declarations*/
%{
#include<stdio.h>
#include<string.h>
#include"trimmer.h"
#include"proj.tab.h"
%}

/*rules*/
%%

([ \n\t\r]+)|(\%.*) {} /*whitespace*/

writeInt[ \n\t\r]  {
                    return(WRITEINT);
                    }

readInt[ \n\t\r] {return(READINT);}

if[ \n\t\r] {return(IF);}

then[ \n\t\r] {return(THEN);}

else[ \n\t\r] {return(ELSE);}

begin[ \n\t\r] {return(BEGINN);}

end[ \n\t\r] {return(END);}

while[ \n\t\r] {return(WHILE);}

do[ \n\t\r] {return(DO);}

program[ \n\t\r] {return(PROGRAM);}

var[ \n\t\r] {
                yylval.sval = malloc(strlen(yytext));
                strncpy(yylval.sval, trimString(yytext), strlen(yytext));
                return(VAR);
                }

as[ \n\t\r] {return(AS);}

int[ \n\t\r] {return(INT);}

bool[ \n\t\r] {return(BOOL);}

([1-9][0-9]*)|0[ \n\t\r] {
                yylval.ival = atoi(trimString(yytext));
                return(NUM);
                }

(false|true)[ \n\t\r] {yylval.sval = malloc(strlen(yytext));
                strncpy(yylval.sval, trimString(yytext), strlen(yytext));
                return(BOOLLIT);}

([A-Z][A-Z0-9]*)[ \n\t\r] {yylval.sval = malloc(strlen(yytext));
                strncpy(yylval.sval, trimString(yytext), strlen(yytext));
                return(IDENTIFIER);}

(\()[ \n\t\r] {return(LP);}

(\))[ \n\t\r] {return(RP);}

(:=)[ \n\t\r] {return(ASGN);}

(;)[ \n\t\r] {return(SC);}

(\*|div|mod)[ \n\t\r] {yylval.sval = malloc(strlen(yytext));
                strncpy(yylval.sval, trimString(yytext), strlen(yytext));
                return(OP2);}

([\+\-])[ \n\t\r] {yylval.sval = malloc(strlen(yytext));
                strncpy(yylval.sval, trimString(yytext), strlen(yytext));
                return(OP3);}

(>=|<=|!=|=|<|>)[ \n\t\r] {yylval.sval = malloc(strlen(yytext));
                strncpy(yylval.sval, trimString(yytext), strlen(yytext));
                return(OP4);}
%%




