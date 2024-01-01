Note: Trimmer contains a helper method that helps remove whitespace from my tokens.

To compile:

    $gcc trimmer.h
    $lex proj.l
    $bison proj.y -d
    $gcc lex.yy.c proj.tab.c

To run:

    $./a.out

    OR, with txt file

    $cat <txtfile> | ./a.out


Example run with valid input file in.txt:

    $cat in.txt | ./a.out
    SUCCESS
    

    
    
    
