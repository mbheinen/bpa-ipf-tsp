C    @(#)putchr_8.f	20.1 1/4/99
        subroutine putchr_8 (n, b, r8)
 
        character b*(*), format*4
        double precision r8

C       * PUTCHR_8 MOVES CHARACTER DATA INTO REAL * 8 STORAGE
 
C       * CURRENTLY USES READ/WRITE TO SIMULATE ENCODE
C       * NOTE THAT VARIABLES ARE SWITCHED FROM ENCODE ORDER
C
C       *  B IS THE CHARACTER DATA PASSED  (NO MORE THAT A4 LONG)
C       *  R8 IS THE REAL * 8 DATA RETURNED
C       *  N IS THE NUMBER OF CHARACTERS TO BE ENCODED
 
 
        write (format,2) n
   2    format('(a',i1,')')  
        read (b, format) r8
 
        return
        end
