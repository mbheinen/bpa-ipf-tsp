C    @(#)getchr_8.f	20.1 1/4/99
        subroutine getchr_8 (n, b, r8)
 
        character b*(*), format*4
        double precision r8

C       * GETCHR_8 MOVES REAL * 8 DATA INTO CHARACTER VARIABLE
 
C       * CURRENTLY USES WRITE TO SIMULATE DECODE
C       * NOTE THAT VARIABLES ARE SWITCHED FROM DECODE ORDER
 
C       *  R8 IS THE REAL * 8 DATA PASSED
C       *  B IS THE CHARACTER DATA RETURNED (NO MORE THAT A4 LONG)
C       *  N IS THE NUMBER OF CHARACTERS TO BE DECODED
 
        write (format,2) n
   2    format('(a',i1,')')  
        write (b, format) r8
 
        return
        end
