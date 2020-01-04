C    @(#)outdbg.f	20.4 5/27/98
      subroutine outdbg

C     This module is intended for debugging use with symbolic debugger.  
c     It can be invoked only by setting ITERM = 1 using the debugger.
C     Its purpose is to restrict the number of outage studies so that 
c     interactive debugging may require less time.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/apcom.inc'
      include 'ipfinc/intbus.inc'

      common /trmdbg/ iterm
C
C     This variable is a VAX symbolic-debugger-set variable to 
c     interactively display certain debug statements, which are 
c     activated by setting ITERM = 1 using the DEPOSIT command.
C
      dimension temp(7), ktemp(7)
      character decide*1, value*9

      jout = 0
      do while (.true.)
        write (*, 10000)
10000   format (' Enter outage range (n:m, 0=Save, -1=Cancel)')
        read (*, 10010) value
10010   format (a)
        if (index (value, ':') .eq. 0) then
           n1 = rval (value)
           n2 = n1
        else
           ix = index (value, ':')
           n1 = rval (value(1:ix-1))
           n2 = rval (value(ix+1:))
        endif
        if (n1 .eq. 0) goto 100
        if (n1 .lt. 0) goto 110
        if (n1 .gt. nout) then
           write (*, 10020) n, nout
10020      format (' Illegal entry ', i4, ' Maximum is ', i4)
        else
          do n = n1, n2
            k1 = klnc(1, n)
            k2 = klnc(2, n)
            id = mod (klnc(4, n), 1000) 
            write (*, 10030) n, intbus(k1), intbas(k1), intbus(k2),
     &         intbas(k2), char(id)
10030       format (1x, i4, ' outage ', a, f6.1, 1x, a, f6.1, 1x, a, 
     &              ' : Select? (Y or N) ')
            read (*, 10040) decide
10040       format (a)
            if (decide .eq. 'Y' .or. decide .eq. 'y') then
              jout = jout + 1
              do j = 1, 4
                ktemp(j) = klnc(j, n)
              enddo
              do j = 1, 7
                temp(j) = clnc(j, n)
              enddo
              do i = n, jout + 1, -1
                do j = 1, 4
                  klnc(j, i) = klnc(j, i-1)
                enddo
                do j = 1, 7
                  clnc(j, i) = clnc(j, i-1)
                enddo
              enddo
              do j = 1, 4
                klnc(j, jout) = ktemp(j)
              enddo
              do j = 1, 7
                clnc(j, jout) = temp(j)
              enddo
            endif
          enddo
        endif
      enddo
  100 nout = jout
  110 return
      end
