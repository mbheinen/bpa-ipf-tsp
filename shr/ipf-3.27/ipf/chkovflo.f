C    @(#)chkovflo.f	20.3 2/13/96
C****************************************************************
C
C   	File: chkovflo.f
C
C   	Purpose: This function decodes real "x" into ascii elements
C                components "array(1)", "array(2)", ..., etc. More
C                than one element will be required if the original
C                "x" cannot be coded using format "(w.d)". The sum
C                of these decoded elements will equal the original "x".
C                                                                      
C       Input parameters:
C
C             x        - the real variable to be encoded (fw.d)
C             w        - the width of the format (fw.d)
C             d        - the number of digits to the right of the
C                        decimal point in the format (fw.d)
C
C       Output parameters:
C
c             num      - the number of encoded words that are required
C                        to encode x into format (fw.d) using explicit
C                        exponiation.
c             array    - a character array of length num which contains
C                        encoded x to fit collectively into x.  
C
C                        if num = 1, the original format was sufficient.
C
C   	Author: Walt Powell            Date: 7 September 1994
C   	Called by: ext_bus.f
C
C****************************************************************
	integer function chkovflo (x, w, d, num, array)
       	integer w, d, num
        character array(*)*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'

        logical finished
        character fmt*10, code*10, tempc*20, ljstfy*20
        integer size, fx, ix, digits

        chkovflo = 0
        if (abs(x) .le. 1.0) then
          num = 1
          array(num) = code (x, w, d)
        else if (w - d .lt. 2) then
          num = 1
          array(num) = code (x, w, d)
        else
          num = 0
          total = 0.0           ! "total" is accumulated encoded value
          fx = w - d                        
          if (x-total .lt. 0.0) fx = fx - 1 ! fx is size of mantissa 
          digits = w - 2
          if (x .lt. 0 .and. digits .gt. 0) digits = digits - 1
          if (abs(x-total) .gt. 1.0) then
            size = alog10 (abs (x-total)) + 1
          else 
            size = 0
          endif
          do while (size .gt. fx)
            num = num + 1
            ix = int (x-total + sign (0.5, x-total))
            write (tempc, '(i10)') ix
            array(num) = ljstfy(tempc)
            write (array(num)(w-1:), 110) size-digits+d
  110       format ('E', i1)
            write (fmt, 120) w, d
  120       format (bn, '(f', i1, '.', i1, ')')
            read (array(num), fmt=fmt) temp
            total = total + temp
            if (abs(x-total) .gt. 1.0) then
              size = alog10 (abs (x-total)) + 1
            else
              size = 0
            endif
          enddo                
          if (size .gt. 0) then
            num = num + 1
            array(num) = code (x-total, w, d)
          endif
       endif

       total = 0.0
       write (fmt, 120) w, d
       do i = 1, num
          read (array(i), fmt=fmt) temp
          total = total + temp
c         write (*, 150) array(i), temp, total
c 150     format (t2, 'Encoded X = ', a, t22, 'X = ', f10.2, 
c    &            t38, 'Total = ', f10.2)
       enddo
       if (total .gt. 0.0 .and. abs (x/total) .gt. 1.0e-3) then
          write ( errbuf(1), 160) total, x
  160     format (' Encoding error - Total = ', e12.5, ' Original = ',
     &              e12.5)
          call prterx ('W', 1)
       endif
       return
       end
