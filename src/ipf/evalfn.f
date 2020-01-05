C    @(#)evalfn.f	20.7 5/27/98
      real function evalfn (string, ndef, defstr, defval, defind,
     1                      error, mode)
      character string*(*), defstr(*)*(*)
      integer defind(*), error, mode
      real defval(*)
C
C     This function evaluates symbolic expressions such as
C
C     2.0 * B1.BS * (B1.PG + B1.QG) ** 2
C
c     MODE defines the source: 0 (default) = base case
c                              1           = alternate base case
c
      include 'ipfinc/parametr.inc'
      include 'ipfinc/prt.inc'
 
      character capital * 132, symbol * 12, fn_code * 4, 
     1          word(30) * 30, newstr * 132, cfmt * 7, tempc * 30
      integer o1, s1, s2, type, itor
      real rtoi
      equivalence (itor, rtoi)
      complex zc
 
      error = 0
      newstr = capital (string)
C
C     Search for symbols
C
      lc = len (string)
      call xscan (newstr(1:lc), word(2), nwrd)
C
C     Append level 0 parenthesis
C
      word(1) = '('
      word(nwrd+2) = ')' 
      nwrd = nwrd + 2
      do 140 iw = 2, nwrd-1
         i1 = index ('ABCDEFGHIJKLMNOPQRSTUVWXYZ',word(iw)(1:1))
         if (i1 .gt. 0) then
C
C             Determine whether symbol is ordinary or indexed.
C
           jc = lastch (word(iw))
           if (index (word(iw), '.') .ne. 0) then
              i2 = index (word(iw), '.')
              symbol = word(iw)(1:i2-1)
              fn_code = word(iw)(i2+1:jc)
           else
              symbol = word(iw)(1:jc)
              fn_code = ' '
           endif
C
C          Find symbol value
C
           lc = lastch (symbol)
           do j = 1, ndef
              if (defstr(j) .eq. symbol(1:lc)) then
                 if (fn_code .ne. ' ' .and. defind(j) .ne. 0) then
c
c                   real-to-integer conversion averted
c
                    rtoi = defval(j)
                    ind = itor
                    type = defind(j)
                    if (mode. eq. 0) then
                       value = getval (ind, type, fn_code, error)
                    else
                       value = xgetval (ind, type, fn_code, error)
                    endif
                    if (error .ne. 0) then
                       write (errbuf(1), 102) symbol(1:lc)
     1                    // '.' // fn_code
  102                  format (' indexed SYMBOL (', a, 
     &                         ') is not installed.')
                       last = min0 (80, len(string))
                       write (errbuf(2), 104) string(1:last)
  104                  format (' Text (', a, ')')
                       call prterx ('W', 2)
                       write (word(iw), 110) 0.0
                       error = 1
                       go to 140
                    endif
                 else if (defind(j) .ne. 0) then
                    write (errbuf(1), 106) symbol(1:lc)
     1                                     // '.' // fn_code
  106               format (' SYMBOL (', a, 
     &                      ') has "INDEX" missing in >LET command.')
                    last = min0 (80, len(string))
                    write (errbuf(2), 104) string(1:last)
                    call prterx ('W', 2)
                    write (word(iw), 110) 0.0
                    error = 1
                    go to 140
                 else if (fn_code .ne. ' ') then
                    write (errbuf(1), 108) symbol(1:lc)
  108               format (' indexed SYMBOL (', a, 
     1                      ') is missing "TYPE" (e.g. .PL)')
                    last = min0 (80, len(string))
                    write (errbuf(2), 104) string(1:last)
                    call prterx ('W', 2)
                    write (word(iw), 110) 0.0
                    go to 140
                 else
                    value = defval(j)
                 endif
                 write (word(iw), 110) value
  110            format (e15.8)
                 go to 140
              endif
           enddo
C
C          Test for special functions
C
           if (symbol(1:lc) .eq. 'SIN' .or.
     &         symbol(1:lc) .eq. 'COS' .or.
     &         symbol(1:lc) .eq. 'TAN' .or.
     &         symbol(1:lc) .eq. 'ARCSIN' .or.
     &         symbol(1:lc) .eq. 'ARCCOS' .or.
     &         symbol(1:lc) .eq. 'ARCTAN' .or.
     &         symbol(1:lc) .eq. 'ABS') then
           else
              write (errbuf(1), 130) symbol(1:lc)
  130         format (' Symbol (', a, ') is not defined')
              last = min0 (80, len(string))
              write (errbuf(2), 104) string(1:last)
              call prterx ('W', 2)
              write (word(iw), 110) 0.0
              error = 1
           endif
        endif
  140 continue
      if (error .ne. 0) go to 900
C
C     Evaluate level of innermost parenthesis
C
  160 maxlev = 0
      s1 = 0
      s2 = 0
      level = 0
      do iw = 1, nwrd
         if (word(iw) .eq. '(') then
            level = level + 1
            if (level .ge. maxlev) then
               maxlev = level
               s1 = iw
            endif
         else if (word(iw) .eq. ')') then
            if (level .eq. maxlev) then
               s2 = iw
            endif
            level = level - 1
         endif
      enddo
 
      if ((s1 .gt. 0 .and. s2 .eq. 0) .or.
     &    (s1 .eq. 0 .and. s2 .gt. 0)) then
         write (errbuf(1), 171) string
  171    format (' Unbalanced parenthesis in string (', a, ')')
         call prterx ('W', 2)
         error = 1
         go to 900
      endif
      if (maxlev .gt. 0) then
C
C        Find highest operator within string
C
         o1 = 0
         do iw = s1+1, s2-1
            if (word(iw) .eq. '**' .or. word(iw) .eq. '^') o1 = iw
         enddo
         if (o1 .eq. 0) then
            iw = s1+1
            do while (iw .le. s2-1 .and. o1 .eq. 0)
               jc = lastch (word(iw))
               if (word(iw)(1:jc) .eq. '/') then
                  o1 = iw
               endif
               iw = iw + 1
            enddo
            if (o1 .eq. 0) then
               iw = s1+1
               do while (iw .le. s2-1 .and. o1 .eq. 0)
                  jc = lastch (word(iw))
                  if (word(iw)(1:jc) .eq. '*') then
                     o1 = iw
                  endif
                  iw = iw + 1
               enddo
               if (o1 .eq. 0) then
                  iw = s1+1
                  do while (iw .le. s2-1 .and. o1 .eq. 0)
                     jc = lastch (word(iw))
                     if (index ('+-', word(iw)(1:jc)) .ne. 0) then
                        o1 = iw
                     endif
                     iw = iw + 1
                  enddo
                  if (o1 .eq. 0) then
                     iw = s1+1
                     do while (iw .le. s2-1 .and. o1 .eq. 0)
                        jc = lastch (word(iw))
                        if (index ('><', word(iw)(1:jc)) .ne. 0) then
                           o1 = iw
                        endif
                        iw = iw + 1
                     enddo
                  endif
               endif
            endif
         endif
         if (o1 .gt. 0) then
            if (o1 .gt. 2) then
C
C              Decode left operand
C
               xl = ftn_atof(word(o1-1))
            else
               jc = lastch (word(o1))
               write (errbuf(1), 206) word(o1)(1:jc)
  206          format (' Operator (', a, ') is missing left operand')
               call prterx ('W', 1)
               error = 1
               go to 900
            endif
C
C           Find right operand.
C
  208       if (o1 .lt. nwrd - 1) then
C
C              Decode right operand
C
               xr = ftn_atof(word(o1+1))
            else
               jc = lastch (word(o1))
               write (errbuf(1), 214) word(o1)(1:jc)
  214          format (' Operator (', a, ') is missing right operand')
               call prterx ('W', 1)
               error = 1
               go to 900
            endif
C
C           Perform operation.
C
  216       if (word(o1) .eq. '**' .or. word(o1) .eq. '^') then
               if (xl .ne. 0.0) then
                  zc = cmplx (xr,0.0) * clog (cmplx(xl,0.0))
                  zc = cexp (zc)
                  x = real (zc)
               else
                  x = 0.0
               endif
            else if (word(o1) .eq. '*') then
               x = xl * xr
            else if (word(o1) .eq. '/') then
               if (xr .ne. 0.0) then
                  x = xl / xr
               else
                  jc = lastch (word(o1+1))
                  write (errbuf(1), 220) word(o1+1)(1:jc)
  220             format (' Error division by zero (', a, ').')
                  call prterx ('W', 1)
                  error = 1
               endif
            else if (word(o1) .eq. '+') then
               x = xl + xr
            else if (word(o1) .eq. '-') then
               x = xl - xr
            else if (word(o1) .eq. '>') then
               x = dim (xl, xr)
            else if (word(o1) .eq. '<') then
               x = -dim (xl, xr)
            else
               jc = lastch (word(o1))
               write (errbuf(1), 230) word(o1)(1:jc)
  230          format (' Unknown operator (', a, ')')
               call prterx ('W', 1)
               error = 0
               go to 900
            endif
            write (word(o1-1), 240) x
  240       format (e15.8)
            do i = o1, nwrd-2
               word(i) = word(i+2)
            enddo
            nwrd = nwrd - 2
            go to 160
         else
 
C           No operator: remove inner level of parenthesis.
C
            do i = s2, nwrd-1
               word(i) = word(i+1)
            enddo
            nwrd = nwrd - 1
            do i = s1, nwrd-1
               word(i) = word(i+1)
            enddo
            nwrd = nwrd - 1
C
C           Determine if intrinsic function.
C
            if (s1 .gt. 1) then
               symbol = word(s1-1)
               lc = lastch(symbol)
               if (symbol(1:lc) .eq. 'SIN') then
                  if (word(s1) .ne. ' ') then
                     do while (lastch (word(s1)) .lt. 10)
                        tempc = ' ' // word(s1)
                        word(s1) = tempc
                     enddo
                  endif
	 	  x = ftn_atof(word(s1))
                  write (word(s1-1), 240) sin (x)
                  do i = s1, nwrd
                     word(i) = word(i+1)
                  enddo
                  nwrd = nwrd - 1
               else if (symbol(1:lc) .eq. 'COS') then
                  if (word(s1) .ne. ' ') then
                     do while (lastch (word(s1)) .lt. 10)
                        tempc = ' ' // word(s1)
                        word(s1) = tempc
                     enddo
                  endif
	 	  x = ftn_atof(word(s1))
                  write (word(s1-1), 240) cos (x)
                  do i = s1, nwrd
                     word(i) = word(i+1)
                  enddo
                  nwrd = nwrd - 1
               else if (symbol(1:lc) .eq. 'TAN') then
                  if (word(s1) .ne. ' ') then
                     do while (lastch (word(s1)) .lt. 10)
                        tempc = ' ' // word(s1)
                        word(s1) = tempc
                     enddo
                  endif
	 	  x = ftn_atof(word(s1))
                  write (word(s1-1), 240) tan (x)
                  do i = s1, nwrd
                     word(i) = word(i+1)
                  enddo
                  nwrd = nwrd - 1
               else if (symbol(1:lc) .eq. 'ARCSIN') then
                  if (word(s1) .ne. ' ') then
                     do while (lastch (word(s1)) .lt. 10)
                        tempc = ' ' // word(s1)
                        word(s1) = tempc
                     enddo
                  endif
	 	  x = ftn_atof(word(s1))
                  write (word(s1-1), 240) asin (x)
                  do i = s1, nwrd
                     word(i) = word(i+1)
                  enddo
                  nwrd = nwrd - 1
               else if (symbol(1:lc) .eq. 'ARCCOS') then
                  if (word(s1) .ne. ' ') then
                     do while (lastch (word(s1)) .lt. 10)
                        tempc = ' ' // word(s1)
                        word(s1) = tempc
                     enddo
                  endif
	 	  x = ftn_atof(word(s1))
                  write (word(s1-1), 240) acos (x)
                  do i = s1, nwrd
                     word(i) = word(i+1)
                  enddo
                  nwrd = nwrd - 1
               else if (symbol(1:lc) .eq. 'ARCTAN') then
                  if (word(s1) .ne. ' ') then
                     do while (lastch (word(s1)) .lt. 10)
                        tempc = ' ' // word(s1)
                        word(s1) = tempc
                     enddo
                  endif
	 	  x = ftn_atof(word(s1))
                  write (word(s1-1), 240) atan (x)
                  do i = s1, nwrd
                     word(i) = word(i+1)
                  enddo
                  nwrd = nwrd - 1
               else if (symbol(1:lc) .eq. 'ABS') then
                  if (word(s1) .ne. ' ') then
                     do while (lastch (word(s1)) .lt. 10)
                        tempc = ' ' // word(s1)
                        word(s1) = tempc
                     enddo
                  endif
	 	  x = ftn_atof(word(s1))
                  write (word(s1-1), 240) abs (x)
                  do i = s1, nwrd
                     word(i) = word(i+1)
                  enddo
                  nwrd = nwrd - 1
               endif
            endif
            go to 160
         endif
 
      else
C
C        Evaluation complete. Decode results.
C
	 evalfn = ftn_atof(word(1))
      endif
 
  900 continue
      return
      end
