C    %W% %G%
      function nambas(base)

      include 'tspinc/params.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/reread.inc'

      logical debug
c     -
c     -     Begin     Begin     Begin     Begin     Begin     Begin
      
      nambas = 0
      ipass = 0                                                         !dem

C     CHECK FOR BLANK BASE
      if (base .eq. 0.0) goto 100

C     DO A BINARY SEARCH AMONG NUMERICALLY ORDERED BASES
      if (debug) then                                                   !dem
        call dbgwrf ('NAMBAS - looking for kv = ',base)                 !dem
      endif                                                             !dem
      if (debug) then                                                   !dem
        do la = 1,ibxyz                                                 !dem
          call dbgwri ('    LA         = ',la)                          !dem
          call dbgwrf ('    BASEKV(LA) = ',basekv(la))                  !dem
        enddo                                                           !dem
      endif                                                             !dem
      i1 = 1
      i2 = ibxyz
      do while (i1 .le. i2 .and. nambas .eq. 0)
        nam = (i1+i2)/2
        ipass = ipass + 1                                               !dem
        if (debug) then                                                 !dem
          call dbgwrf ('  looking for kv = ',base)                      !dem
          call dbgwri ('  pass # ',ipass)                               !dem
          call dbgwri ('    I1 /left/   = ',i1)                         !dem
          call dbgwrf ('    BASEKV(I1)  = ',basekv(i1))                 !dem
          call dbgwri ('    NAM /midd/  = ',nam)                        !dem
          call dbgwrf ('    BASEKV(NAM) = ',basekv(nam))                !dem
          call dbgwri ('    I2 /right/  = ',i2)                         !dem
          call dbgwrf ('    BASEKV(I2)  = ',basekv(i2))                 !dem
        endif                                                           !dem
        if (basekv(nam) .lt. base) then
          i1 = nam + 1
        else if (basekv(nam) .gt. base) then
          i2 = nam - 1
        else
          nambas = nam
        endif
      enddo
  100 if (nambas .eq. 0) then
        write (errbuf(1), 10000) base
10000   format('NAMBAS - The card below was ignored because the base ', !dem
     1    'kv (', f6.1, ') was not used in the base power flow.')       !dem
        write (errbuf(2), 10010) buffer
10010   format (a)                                                      !dem
        call prterr ('E',2)                                             !dem
        iabort=1

c       force a program abort here                                      !dem
        if (debug) then                                                 !dem
          call erexit
        endif                                                           !dem
      endif
      return
      end

