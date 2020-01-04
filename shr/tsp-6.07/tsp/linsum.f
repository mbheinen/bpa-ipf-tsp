C    %W% %G%
      subroutine linsum
C     
C     THIS SUBROUTINE PRINTS THE LINE SUMMARY FOR ALL LINES
C     REQUESTED IN THE STUDY.  IT IS CALLED BY LINOUT.
C     
      include 'tspinc/prt.inc'
      include 'tspinc/linanl.inc'
      include 'tspinc/worst.inc'

      call forbtm()
      call fortop()
      call skipln(5)

      write (outbuf, 10000)
10000 format (39x, 'SUMMARY ONE OF LINE QUANTITIES DURING NONFAULT',
     & ' PERIODS')
      call prtout(1)
      if (wtim2 .ne. -1.) then
        write (outbuf, 10010) wtim1, wtim2
10010   format (39x, 'DURING TIME WINDOW ', f8.2, ' CYCLES TO ', f8.2,
     &   ' CYCLES.')
        call prtout(1)
      endif
      call skipln(5)
      write (outbuf, 10020)
10020 format (42x, 'LINE FLOW-MW', 10x, '1', 8x, 'LINE FLOW-MVAR', 9x,
     & '1', 7x, 'LINE CURRENT-AMPS')
      call prtout(1)
      write (outbuf, 10030)
10030 format (12x, 'LINE NAME', 11x, 'MAXIMUM', 3x, 'TIME', 2x,
     & 'MINIMUM', 3x, 'TIME', 4x, 'MAXIMUM', 3x, 'TIME', 2x, 'MINIMUM',
     & 3x, 'TIME', 4x, 'MAXIMUM', 3x, 'TIME', 2x, 'MINIMUM', 3x, 'TIME'
     & )
      call prtout(1)
      do jtrr = 1, ilinal
        write (outbuf, 10040) anlnm1(jtrr), anlkv1(jtrr), anlnm2(jtrr),
     &   anlkv2(jtrr), anlpar(jtrr), anlmax(1, jtrr), anltax(1, jtrr),
     &   anlmin(1, jtrr), anltin(1, jtrr), anlmax(2, jtrr), 
     &   anltax(2, jtrr), anlmin(2, jtrr), anltin(2, jtrr), 
     &   anlmax(3, jtrr), anltax(3, jtrr), anlmin(3, jtrr), 
     &   anltin(3, jtrr)
10040   format (1x, a8, f5.1, 1x, a8, f5.1, 1x, a1,  3(f9.2, 1x,
     &   f6.1, f9.2, 1x, f6.1, 2x))
        call prtout(1)
      enddo

      call forbtm()
      call fortop()
      call skipln(5)
      write (outbuf, 10050)
10050 format (39x, 'SUMMARY TWO OF LINE QUANTITIES DURING NONFAULT',
     & ' PERIODS')
      call prtout(1)
      if (wtim2 .ne. -1.) then
        write (outbuf, 10060) wtim1, wtim2
10060   format (39x, 'DURING TIME WINDOW ', f8.2, ' CYCLES TO ', f8.2,
     &   ' CYCLES.')
        call prtout(1)
      endif
      call skipln(5)
      write (outbuf, 10070)
10070 format (39x, 'APPARENT R-OHMS', 16x, 'APPARENT RDOT-OHMS/SEC')
      call prtout(1)
      write (outbuf, 10080)
10080 format (12x, 'LINE NAME', 11x, 'MAXIMUM', 3x, 'TIME', 2x,
     & 'MINIMUM', 3x, 'TIME', 4x, 'MAXIMUM', 3x, 'TIME', 2x, 'MINIMUM',
     & 3x, 'TIME')
      call prtout(1)
      do jtrr = 1, ilinal
        write (outbuf, 10090) anlnm1(jtrr), anlkv1(jtrr), anlnm2(jtrr),
     &   anlkv2(jtrr), anlpar(jtrr), anlmax(4, jtrr), anltax(4, jtrr),
     &   anlmin(4, jtrr), anltin(4, jtrr), anlmax(5, jtrr), 
     &   anltax(5, jtrr), anlmin(5, jtrr), anltin(5, jtrr)
10090   format (1x, a8, f5.1, 1x, a8, f5.1, 1x, a1, 2(f9.2, 1x,
     &   f6.1, f9.2, 1x, f6.1, 2x))
        call prtout(1)
      enddo

      call forbtm()
      call fortop()
      call skipln(5)
      write (outbuf, 10100)
10100 format (39x, 'SUMMARY THREE OF LINE QUANTITIES DURING NONFAULT',
     & ' PERIODS')
      call prtout(1)
      if (wtim2 .ne. -1.) then
        write (outbuf, 10110) wtim1, wtim2
10110   format (39x, 'DURING TIME WINDOW ', f8.2, ' CYCLES TO ', f8.2,
     &   ' CYCLES.')
        call prtout(1)
      endif
      call skipln(5)
      write (outbuf, 10120)
10120 format (42x, 'TCSC XC     ', 10x, '1', 8x, 'TCSC ANGLE   ', 9x,
     & '1')
      call prtout(1)
      write (outbuf, 10130)
10130 format (12x, 'LINE NAME', 11x, 'MAXIMUM', 3x, 'TIME', 2x,
     & 'MINIMUM', 3x, 'TIME', 4x, 'MAXIMUM', 3x, 'TIME', 2x, 'MINIMUM',
     & 3x, 'TIME')
      call prtout(1)
      do jtrr = 1, ilinal
        write (outbuf, 10140) anlnm1(jtrr), anlkv1(jtrr), anlnm2(jtrr),
     &   anlkv2(jtrr), anlpar(jtrr), anlmax(6, jtrr), anltax(6, jtrr),
     &   anlmin(6, jtrr), anltin(6, jtrr), anlmax(7, jtrr), 
     &   anltax(7, jtrr), anlmin(7, jtrr), anltin(7, jtrr)
10140   format (1x, a8, f5.1, 1x, a8, f5.1, 1x, a1, 2(f9.2, 1x,
     &   f6.1, f9.2, 1x, f6.1, 2x))
        call prtout(1)
      enddo
      call forbtm()
      call fortop()
      return
      end
