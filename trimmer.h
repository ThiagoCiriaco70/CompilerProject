#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/* helper function borrowed from Lasha Khintibidze*/
/* https://www.delftstack.com/howto/c/trim-string-in-c/ */
/* using this to get rid of newlines and space in the lexemes */

char *trimString(char *str)
{
    char *end;

    while(isspace((unsigned char)*str)) str++;

    if(*str == 0)
        return str;

    end = str + strlen(str) - 1;
    while(end > str && isspace((unsigned char)*end)) end--;

    end[1] = '\0';

    return str;
}