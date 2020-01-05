C    @(#)subdef.f	20.4 5/27/98
        character * (*) function subdef (text, numdef, defstr, defval,
     1                                   defind, nchr, ndefch, defchr)
        integer defind(*), ndefch(*)
        character defstr (*)*(*), text *(*), defchr (*)*(*)
        dimension defval(*)
C
C       Match 'strings' in DEFSTR and COMMNT
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'

        integer error, type, itor
        real rtoi
        equivalence (rtoi, itor)
        character tempx * 132, tempc * 132, fn_code * 4, 
     1            symbol * 18, format * 18, valuec * 18, capital * 18
 
        tempx = text
  100   if (index (tempx,'$') .ne. 0) then
C
C          SYMBOL is delimited with a " ".
C
           i1 = index (tempx(1:),'$')
           j1 = index (tempx(i1+1:),' ') + i1
           if (j1 .eq. i1) j1 = lastch (tempx) + 1
           if (i1 + 1 .gt. j1 - 1 ) then
              write (errbuf(1), 120)
  120         format (' SYMBOL cannot be deciphered from TEXT ')
              tempc = ' '
              if (i1 .le. 80) tempc(i1:i1) = '$'
              write (errbuf(2), 130) tempc(1:80)
  130         format ('      (', a, ')')
              write (errbuf(3), 140) tempx(1:80)
  140         format (' Text (', a, ')')
              call prterx ('W', 3)
C
C             Overwrite output with "--"
C
              i = index (format, '.')
              if (i .gt. 0) then
                 tempc = format
                 format = '(A' // tempc(i-1:i-1) // ')'
              else
                 tempc = format
                 format = '(A' // tempc(3:3) // ')'
              endif
              write (valuec, format, err=220) '--'
              l = lastch (valuec)
              tempx(i1:i1+l-1) = valuec
              go to 100
           else
              symbol = capital (tempx(i1+1:j1-1))
              fn_code = ' '
              format = '(F6.0)'
C
C             Check for indexed symbol (imbedded CODE) and
C             imbedded format specifications.
C
              lc = lastch (symbol)
              i2 = index (symbol, '.')
              j2 = index (symbol, '/')
              if (i2 .gt. 0 .and. j2 .eq. 0) then
                 fn_code = symbol(i2+1:lc)
                 symbol = symbol(1:i2-1)
                 lc = lastch (symbol)
              else if (i2 .gt. 0 .and. j2 .gt. i2) then
                 format = '(' // symbol(j2+1:lc) // ')'
                 fn_code = symbol(i2+1:j2-1)
                 symbol = symbol(1:i2-1)
                 lc = lastch (symbol)
              else if (j2 .gt. 0) then
                 format = '(' // symbol(j2+1:lc) // ')'
                 symbol = symbol(1:j2-1)
                 lc = lastch (symbol)
              endif
 
              do 230 j = 1,numdef
                 if (defstr(j) .eq. symbol(1:lc)) then
                    tempx(i1:j1) = ' '
C
C                   Encode according to user-specified format
C
                    if (fn_code .ne. ' ' .and. defind(j) .ne. 0) then
c
c                      real-to-integer conversion averted
c
                       rtoi = defval(j)
                       ind = itor
                       type = defind(j)
                       value = getval (ind, type, fn_code, error)
                       if (error .ne. 0) then
                          write (errbuf(1), 142) symbol(1:lc)
     1                                        // '.' // fn_code
  142                     format (' indexed SYMBOL (', a,
     1                            ', ) is not installed.')
                          write (errbuf(2), 160) tempx(1:80)
                          call prterx ('W', 2)
C
C                         Overwrite output with "--"
C
                          i = index (format, '.')
                          if (i .gt. 0) then
                             tempc = format
                             format = '(A' // tempc(i-1:i-1) // ')'
                          else
                             tempc = format
                             format = '(A' // tempc(3:3) // ')'
                          endif
                          write (valuec, format, err=220) '--'
                          l = lastch (valuec)
                          tempx(i1:i1+l-1) = valuec
                          go to 100
                       else
                          write (valuec, format, err=220) value
                          l = lastch (valuec)
                       endif
                    else if (defind(j) .ne. 0) then
                       write (errbuf(1), 150) symbol(1:lc)
     1                                        // '.' // fn_code
  150                  format (' SYMBOL (', a,
     1                         ', ) is has "INDEX" missing',
     2                         ' in > DEFINE command.')
                       write (errbuf(2), 160) tempx(1:80)
  160                  format (' Text (', a, ')')
                       call prterx ('W', 2)
C
C                      Overwrite output with "--"
C
                       i = index (format, '.')
                       if (i .gt. 0) then
                          tempc = format
                          format = '(A' // tempc(i-1:i-1) // ')'
                       else
                          tempc = format
                          format = '(A' // tempc(3:3) // ')'
                       endif
                       write (valuec, format, err=220) '--'
                       l = lastch (valuec)
                       tempx(i1:i1+l-1) = valuec
                       go to 100
                    else if (fn_code .ne. ' ') then
                       write (errbuf(1), 170) symbol(1:lc)
  170                  format (' indexed SYMBOL (', a,
     1                         ', ) is missing "TYPE" (e.g. .PL)')
                       write (errbuf(2), 160) tempx(1:80)
                       call prterx ('W', 2)
C
C                      Overwrite output with "--"
C
                       i = index (format, '.')
                       if (i .gt. 0) then
                          tempc = format
                          format = '(A' // tempc(i-1:i-1) // ')'
                       else
                          tempc = format
                          format = '(A' // tempc(3:3) // ')'
                       endif
                       write (valuec, format, err=220) '--'
                       l = lastch (valuec)
                       tempx(i1:i1+l-1) = valuec
                       go to 100
                    else if (ndefch(j) .eq. 0) then
                       write (valuec, format, err=220) defval(j)
                       l = lastch (valuec)
                    else
                       if (index (format, 'A') .eq. 0) then
                          write (errbuf(1), 172) symbol(1:lc), format
  172                     format (' Character symbol (', a,
     1                            ' ) has improper format ', a)
                          write (errbuf(2), 160) tempx(1:80)
                          call prterx ('W', 2)
                          valuec = '******************'
                          l = lastch (valuec)
                       else
                          ii = index (format, 'A')
                          jj = index (format, ')')
                          read (format(ii+1:jj-1), 173) n
  173                     format (i2)
                          if (n .gt. len(valuec)) then
                             write (format(ii+1:jj-1), 173) len(valuec)
                          endif
                          write (valuec, format, err=220)
     1                       defchr(ndefch(j))
                          l = lastch (valuec)
                       endif
                    endif
C
C                   Eliminate trailing decimal point for Fx.0
C                   format specifications.
C
                    if (l .gt. 0) then
                       if (valuec(l:l) .eq. '.') then
                          valuec(l:l) = ' '
                          l = l - 1
                       endif
                    endif
                    tempx(i1:i1+l-1) = valuec
                    go to 100
 
                 endif
  230         continue
 
              write (errbuf(1), 240) symbol(1:lc)
  240         format (' String ',a,' in TEXT is not defined')
              call prterx ('W', 1)
C
C             Overwrite output with "--"
C
              i = index (format, '.')
              if (i .gt. 0) then
                 tempc = format
                 format = '(A' // tempc(i-1:i-1) // ')'
              else
                 tempc = format
                 format = '(A' // tempc(3:3) // ')'
              endif
              write (valuec, format, err=220) '--'
              l = lastch (valuec)
              tempx(i1:i1+l-1) = valuec
              go to 100
           endif
        endif
        go to 900

  220   if (fn_code .ne. ' ' .and. defind(j) .ne. 0) then
           write (errbuf(1), 222) defstr(j), format, value
  222      format ('Format specification error for SYMBOL (',
     1              a, ') Format ', a, ' Value (', f8.1, ')')
           call prterx ('W', 1)
        else
           write (errbuf(1), 222) defstr(j), format, defval(j)
           call prterx ('W', 1)
        endif
C
C       Overwrite output with "--"
C
        valuec = '--'
        l = lastch (valuec)
        tempx(i1:i1+l-1) = valuec
        go to 100

  900   subdef = tempx
        return
        end
