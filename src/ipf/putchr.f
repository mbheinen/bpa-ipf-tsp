C    @(#)putchr.f	20.1 1/4/99
        subroutine putchr (n, b, ia)
 
        character b*(*), format*4
 
C       * PUTCHR   MOVES CHARACTER DATA INTO INTEGER * 4 STORAGE
 
C       * CURRENTLY USES READ/WRITE TO SIMULATE ENCODE
C       * NOTE THAT VARIABLES ARE SWITCHED FROM ENCODE ORDER
C 
C       *  B IS THE CHARACTER DATA PASSED  (NO MORE THAT A4 LONG)
C       *  IA IS THE INTEGER DATA RETURNED
C       *  N IS THE NUMBER OF CHARACTERS TO BE ENCODED
 
        write (format,2) n
   2    format('(a',i1,')')
        read (b, format) ia
 
        return
 
        end
