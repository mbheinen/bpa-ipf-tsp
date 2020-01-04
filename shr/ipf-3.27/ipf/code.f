C    @(#)code.f	20.3 2/13/96
c
C     character function CODE -- used for float to ascii conversion -
C     gets as much precision as possible from an implied decimal field
C
C     This function has a sister function called "reformat" in
C     the "gui" library (reformat.c) that should be kept
C     functionally the same as this routine
C      
C     The fortran function CODEX is similar to this function, but
C     forces a decimal point.  It is used exclusively for
C     base kv conversion.  It has a sister function in the "gui"
C     library called "convert" (convert.c)
C      

      character * (*) function code (x, width, dec_field)
      integer width, dec_field
     
      include 'ipfinc/prt.inc'

      common /code_ovfg/ code_ovfg
      character code_ovfg*3
c
c     Set code_ovfg = 'ON' to replace '****' with '+XEX' values 
c
      character xc * 21, fmt * 12, tempc * 10, ljstfy * 21
      integer free, digits
C    
C     Check validiy of format specifications
C    
      if (width .le. 0 .or. width .gt. 10 .or. 
     &    dec_field .gt. width .or. dec_field .lt. 0) then
         write (errbuf(1),1000) width, dec_field
 1000    format(' Format (programming error)',
     &          ' Format:','f',i2,'.',i1)
         call prterx ('E',1)
         code = '**********'
         return
      endif
C    
C     Skip under/overflow tests for zero's
C    
      if (x .eq. 0.0) then
         code = ' '
         return
      endif
C    
c     encode word in maximum f21.10 (ten digits on either
c     side of decimal point.
C    
      write (xc,1100) x
 1100 format(f21.10)
C    
c     left-justify encoded word
C    
      xc = ljstfy (xc)
C    
c     eliminate leading "0"
C    
      if (xc(1:1) .eq. '-' .and. xc(2:2) .eq. '0') then
         xc(2:) = xc(3:)
      else if (xc(1:1) .eq. '0') then
         xc(1:) = xc(2:)
      endif
c    
c     locate decimal point
c    
      digits = index (xc,'.') - 1
c    
c     compute available characters for dec_field
c    
      free = width - dec_field - digits
c    
c     three conditions exist for encoding "e"
c    
c     1.  word fits perfectly into width.dec_field.  encode word in
c         width+1.dec_field and squeeze out decimal point.
c    
      if (free .eq. 0 .or. free .eq. 1) then
         write (fmt,1300) width + 2, dec_field
 1300    format ('(F',i2,'.',i2,')')
         write (xc,fmt) x
         if (xc(1:1) .eq. ' ') then
            xc = xc(2:)
            digits = index (xc,'.')
            xc(digits:) = xc(digits+1:)
            if (digits .eq. 2 .and. xc(1:1) .eq. '0') xc(1:1) = ' '
         else if (xc(1:1) .eq. '-') then
            if (xc(2:2) .eq. '0') xc(2:) = xc(3:)
            digits = index (xc,'.')
            xc(digits:) = xc(digits+1:)
            if (digits .eq. 3 .and. xc(2:2) .eq. '0') xc(2:2) = ' '
         endif
c    
c     2. leading blanks appear in encoded word.  accuracy of
c        encoding can be improved by shifting decimal point
c        left.
c    
      else if (free .gt. 1) then
         if (abs (x) .ge. 1.0) then
            write (fmt, 1300) width+1, width-digits-1
            write (xc, fmt, err=1302) x
            goto 1304
 1302       if (width-digits-2 .lt. 0) then
               write (fmt, 1300) width, 0
            else
               write (fmt, 1300) width, width-digits-2
            endif
            write (xc, fmt) x
 1304       continue
            if (xc(1:1) .eq. ' ') then
               xc = xc(2:)
            endif
         else
            write (fmt, 1300) width+2, width-digits-1
            write (xc, fmt, err=1306) x
            goto 1308
 1306       if (width-digits-2 .lt. 0) then
               write (fmt,1300) width+1, 0
            else
               write (fmt,1300) width+1, width-digits-2
            endif
            write (xc, fmt) x
 1308       continue
            if (xc(1:1) .eq. ' ') then
               xc = xc(2:)
            endif
c    
c           eliminate leading "0"
c    
            if (xc(1:1) .eq. '-' .and. xc(2:2) .eq. '0') then
               xc(2:) = xc(3:)
            else if (xc(1:1) .eq. '0') then
               xc(1:) = xc(2:)
            endif
         endif
c    
c     3. overflow occurs in present format.  check if shifting
c        the decimal point to the right suffices.
c    
      else if (free .lt. 0) then
         if (width-1 .lt. digits) then
            if (code_ovfg .eq. 'ON') then
c
c              Option set in calling program to avoid '****' using 
c              E formats. 
c              Variables:
c
c              free = number of integer digits

               if (x .gt. 0.0) then
                  free = width - 2
               else
                  free = width - 3
               endif                 
               ilog = alog10 (abs(x))
               ix = x * 10.0 ** (free-ilog)  
               write (xc, '(i10)') ix
               xc = ljstfy (xc)
               write (xc(width-1:), 1400) ilog-free+1
 1400          format ('E', i1)
            else
               xc = '*************'
            endif
            last = lastch(xc)
            write (errbuf(1), 1410) x, width, dec_field, xc(1:last)
 1410       format(' F-Field overflow - Quantity: ', 
     &             e12.5, ' Format: ', 'F', i1, '.' ,i1,
     &             ' Encoded value: ', a)
            call prterx ('W', 1)
         else
            write (fmt, 1300) width+1, width-digits-1
            write (xc, fmt) x
            if (xc(1:1) .eq. ' ') then
               xc = xc(2:)
            endif
         endif
      endif
      code = xc
      return
      end
