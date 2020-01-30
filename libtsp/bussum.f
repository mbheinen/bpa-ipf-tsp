C    %W% %G%
      subroutine bussum
C     
C     THIS SUBROUTINE PRINTS THE BUS SUMMARY FOR ALL BUSES
C     REQUESTED IN THE STUDY.  IT IS CALLED BY BUSOUT.
C     
      include 'tspinc/prt.inc'
      include 'tspinc/prtmax.inc'
      include 'tspinc/busanl.inc'
      include 'tspinc/worst.inc'
      call forbtm()
      call fortop()
      call skipln(5)
      write (outbuf, 10000)
10000 format (20x, 'SUMMARY OF BUS QUANTITIES DURING NONFAULT PERIODS')
      call prtout(1)
      if (wtim2 .ne. -1.) then
        write (outbuf, 10010) wtim1, wtim2
10010   format (18x, 'DURING TIME WINDOW ', f8.2, ' CYCLES TO ', f8.2,
     &   ' CYCLES.')
        call prtout(1)
      endif
      call skipln(5)
      write (outbuf, 10020)
10020 format (6x, 'BUS NAME', 14x, 'BUS FREQUENCY-HERTZ', 19x,
     & 'BUS VOLTAGE-PU')
      call prtout(1)
      write (outbuf, 10030)
10030 format (22x, 'MAXIMUM', 3x, 'TIME', 3x, 'MINIMUM', 3x, 'TIME',
     & 5x, 'MAXIMUM', 3x, 'TIME', 3x, 'MINIMUM', 3x, 'TIME')
      call prtout(1)
      do jtrr = 1, ibusal
        write (outbuf, 10040) anbnam(jtrr), anbkv(jtrr), anbmax(1,
     &   jtrr), anbtax(1, jtrr), anbmin(1, jtrr), anbtin(1, jtrr),
     &   anbmax(2, jtrr), anbtax(2, jtrr), anbmin(2, jtrr), anbtin(2,
     &   jtrr)
10040   format (3x, a8, 1x, f5.1, 3x, f8.4, 1x, f7.2, 1x, f8.4, 1x,
     &   f7.2, 3x, f8.4, 1x, f7.2, 1x, f8.4, 1x, f7.2)
        call prtout(1)
      enddo
      call forbtm()
      call fortop()
      return
      end
