Note: Trimmer contains a helper method that helps remove whitespace from my tokens.


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
    

    
    
    
