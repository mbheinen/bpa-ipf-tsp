C    @(#)getchr.f	20.1 1/4/99
        subroutine getchr (n, b, ia)
 
        character b*(*), format*4
        double precision r8

C       * GETCHR   MOVES REAL DATA INTO CHARACTER VARIABLE
 
C       * CURRENTLY USES WRITE TO SIMULATE DECODE
C       * NOTE THAT VARIABLES ARE SWITCHED FROM DECODE ORDER
 
C       *  IA IS THE INTEGER * 4 DATA PASSED
C       *  B IS THE CHARACTER DATA RETURNED (NO MORE THAT A4 LONG)
C       *  N IS THE NUMBER OF CHARACTERS TO BE DECODED
 
        write (format,2) n
   2    format('(a',i1,')')  
        write (b,format) ia
        return

        end
