C    %W% %G%
      subroutine rlread
C     
C     THIS SUBROUTINE DECODES THE RL CARDS FOR THE DEFAULT DISTANC
C     RELAY AND FORMS INITIAL TABLES.  IT IS CALLED BY INPUT1.
C     
      include 'tspinc/params.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/dfalt.inc'
      include 'tspinc/in1n.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/bypass.inc'

      dimension temp(45), ktemp(45)
      equivalence (temp, ktemp)
      character*8 bus1c, bus2c, bus3c, bus4c
      character*1 colmn4
C     -     begin     begin     begin     begin     begin     begin
C     
C     CHECK SUBTYPE OF RL CARD
C     
      read (buffer, 10000) colmn4
10000 format (3x, a1)
      write (outbuf, 10010) buffer
10010 format ('0', a)
      call prtout(1)
      if (colmn4 .eq. 'V') then
C       
C       DECODING RL V CARD
C       
        read (buffer, 10020) (temp(k), k = 1, 12)
10020   format (bz, 9x, 4(f5.4, 2f5.0))
        do i = 1, 12, 3
          if (temp(i) .ne. 0.0) then
            if (.not. (temp(i+1) .eq. 0.0 .and. temp(i+2) .eq. 0.0))
     &       then
              if (temp(i+2) .eq. 0.0) temp(i+2) = temp(i+1)
              if (temp(i+1) .gt. temp(i+2)) then
                tem = temp(i+1)
                temp(i+1) = temp(i+2)
                temp(i+2) = tem
              endif
              nrlv = nrlv + 1
              reappx(nrlv, 1) = temp(i)
              reappx(nrlv, 2) = temp(i+1)
              reappx(nrlv, 3) = temp(i+2)
            endif
          endif
        enddo
      elseif (colmn4 .eq. 'D') then
C       
C       DECODING RL D CARD
C       
        read (buffer, 10030) bus1c, base1, bus2c, base2, bus3c, base3,
     &   bus4c, base4
10030   format (bz, 9x, a8, f4.0, 1x, a8, f4.0, 5x, a8, f4.0, 1x, a8,
     &   f4.0)
        if (.not. (base1 .eq. 0.0 .and. base2 .eq. 0.0)) then
          if (base1 .eq. 0.0 .or. base2 .eq. 0.0) then
            write (errbuf(1), 10040) buffer
            write (errbuf(2), 10050)
            call prterr('E', 2)
10040       format ('0', a)
10050       format (' CHECK BUS1-BASE1 % BUS2-BASE2')
            iabort = 1
          else
            kb1 = nambas(base1)
            kb2 = nambas(base2)
            nbypas = nbypas + 1
            kbpass(1, nbypas) = inam(bus1c, kb1)
            kbpass(2, nbypas) = inam(bus2c, kb2)
          endif
        endif
        if (.not. (base3 .eq. 0.0 .and. base4 .eq. 0.0)) then
          if (base3 .eq. 0.0 .or. base4 .eq. 0.0) then
            write (errbuf(1), 10060) buffer
            write (errbuf(2), 10070)
            call prterr('E', 2)
10060       format ('0', a)
10070       format (' CHECK BUS3-BASE3 % BUS4-BASE4')
            iabort = 1
          else
            kb3 = nambas(base3)
            kb4 = nambas(base4)
            nbypas = nbypas + 1
            kbpass(1, nbypas) = inam(bus3c, kb3)
            kbpass(2, nbypas) = inam(bus4c, kb4)
          endif
        endif
      elseif (colmn4 .eq. ' ') then
C       
C       DECODING RL CARD
C       
        ndfltd = ndfltd + 1
        read (buffer, 10080) rdelay, trclse
10080   format (bz, 9x, 2f5.2)
      else
        write (errbuf(1), 10090) buffer
        write (errbuf(2), 10100)
        call prterr('E', 2)
10090   format ('0', a)
10100   format (' COLUMN 4 ON THIS CARD MUST BE BLANK, V, OR, D ')
      endif
      return
      end
