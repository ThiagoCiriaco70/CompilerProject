Includes lexer, parser, and compiler. These insturctions are for compiling the custom language code into runnable C code. 
Compiler will throw and explain compile time errors if present.

Make sure all files are in the same directory.
To compile:

    $gcc trimmer.h uthash.h; lex proj.l; bison proj.y -d;   gcc lex.yy.c proj.tab.c -o compiler



To run and generate C file (if code has errors, it will say in the file):

    $./compiler >> output.c

    OR, with txt file

    $cat <txtfile> | ./compiler >> output.c


Compile c code:

    $gcc output.c

Run c code:
   
    $./a.out
    

    
    
    
