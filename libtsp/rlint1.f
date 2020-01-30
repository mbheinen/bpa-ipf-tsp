C    %W% %G%
      subroutine rlint1
C     
C     This subroutine cleans up the default distance relay BYPASS.
C     It converts external power flow numbers to internal swing numbers,
C     arranges buses belonging to branch in low to high order,
C     arranges branches in ascending order, and
c     removes redudant branches.  It is called by INITL1.
C     
      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/busnum.inc'

      integer mbypss(6000)

      if (iswbps .ne. 0) then
        if (nbypas .gt. 6000) then
          write (errbuf(1), 10000) nbypas
          call prterr('E', 1)
10000     format ('0',
     &     'THE BYPASS( ) TABLE CAN ACCOMMODATE 6000 ENTRIES.',
     &     ' YOU HAVE (', i4, ').')
          iabort = 1
        else
          do i = 1, nbypas
            i1 = kbpass(1, i)
            j1 = kbpass(2, i)
            i1 = indx2n(i1)
            j1 = indx2n(j1)
            if (i1 .gt. j1) then
              mbypss(i) = j1*10000 + i1
            else
              mbypss(i) = i1*10000 + j1
            endif
          enddo
          lim = nbypas
          do while (.true.)
            kdone = 1
            lim = lim - 1
            if (lim .eq. 0) goto 100
            do i = 1, lim
              j = mbypss(i)
              k = mbypss(i+1)
              if (j .gt. k) then
                mbypss(i) = k
                mbypss(i+1) = j
                kdone = 2
              endif
            enddo
            if (kdone .eq. 1) goto 100
          enddo
  100     continue
          kend = nbypas
          kmove = nbypas - 1
  110     continue
          do i = 1, kmove
            if (mbypss(i) .eq. mbypss(i+1)) goto 120
          enddo
          goto 130
  120     k = i
          do j = k, kmove
            mbypss(j) = mbypss(j+1)
          enddo
          kmove = kmove - 1
          goto 110
  130     nbypas = kmove + 1
C         
C         WE WILL NOW UNPACK
C         
          do i = 1, nbypas
            j1 = mod(mbypss(i), 10000)
            i1 = (mbypss(i)-j1)/10000
            kbpass(1, i) = i1
            kbpass(2, i) = j1
          enddo
        endif
      endif
      idfltd = ibypas + nbypas
      return
      end
