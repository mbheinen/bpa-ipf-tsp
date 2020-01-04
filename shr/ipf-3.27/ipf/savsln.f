C    @(#)savsln.f	20.8 11/11/97
        subroutine savsln
C
C       THIS SUBROUTINE STORES/RETRIEVES BEST SOLUTION VALUES.
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/alpha2.inc'
        include 'ipfinc/amtrx.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/ikk.inc'
        include 'ipfinc/tbx.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/miscfile.inc'

        integer old_ntot, old_ntota, old_ntotb, old_ntotc, old_yptr

        save
C
C       Store best solution on scratch file lunscr
C
        rewind lunscr
        write(lunscr) (e(i),i=1,ntot)
        write(lunscr) (f(i),i=1,ntot)
        write (lunscr) (kvolt(i),i=1,ntot)  
        write (lunscr) (volt(i),i=1,ntot)   
        write (lunscr) (pnetu(i),i=1,ntot)             
        write (lunscr) (qnetu(i),i=1,ntot)             
        write (lunscr) (gkmu(i),bkmu(i),i=1,yptr)   
        if (ntota.gt.0) write (lunscr) (tap(i),i=1,ntota)
        if (ntotb.gt.0) write (lunscr) (tbx(7,i),i=1,ntotb)
        if (ntotc.gt.0) write (lunscr) (karea(7,i),i=1,ntotc)
        old_ntot = ntot
        old_ntota = ntota
        old_ntotb = ntotb
        old_ntotc = ntotc
        old_yptr = yptr
        return
C
C       Retrieve best solution from scratch file lunscr
C
        entry retsln
        rewind lunscr
        if (ntot .ne. old_ntot .or.
     &      yptr .ne. old_yptr .or. 
     &      ntota .ne. old_ntota .or.
     &      ntotb .ne. old_ntotb .or.
     &      ntotc .ne. old_ntotc) then
          write (errbuf(1), 100) ntot, old_ntot
  100     format (' Restore last solution variables failed. NTOT <> OLD_
     &NTOT ', 2i6)
          write (errbuf(2), 110) yptr, old_yptr
  110     format ('                                         YPTR <> OLD_
     &YPTR ', 2i6)
          write (errbuf(3), 120) ntota, old_ntota
  120     format ('                                         NTOTA <> OLD
     &_NTOTA ', 2i6)
          write (errbuf(4), 130) ntotb, old_ntotb
  130     format ('                                         NTOTB <> OLD
     &_NTOTB ', 2i6)
          write (errbuf(5), 140) ntotc, old_ntotc
  140     format ('                                         NTOTC <> OLD
     &_NTOTC ', 2i6)
          call prterx('W', 5)
        endif
        if (ntot .eq. old_ntot) then
          read(lunscr) (e(i),i=1,ntot)
          read(lunscr) (f(i),i=1,ntot)
          read (lunscr) (kvolt(i),i=1,ntot)
          read (lunscr) (volt(i),i=1,ntot)
          read (lunscr) (pnetu(i), i = 1, ntot)          
          read (lunscr) (qnetu(i), i = 1, ntot)          
          if (yptr .eq. old_yptr .and. 
     &        ntota .eq. old_ntota .and.
     &        ntotb .eq. old_ntotb .and.
     &        ntotc .eq. old_ntotc) then
            read (lunscr) (gkmu(i),bkmu(i),i=1,yptr)   
            if (ntota .gt. 0) read (lunscr) (tap(i),i=1,ntota) 
            if (ntotb .gt. 0) read (lunscr) (tbx(7,i),i=1,ntotb) 
            if (ntotc.gt.0) read (lunscr) (karea(7,i),i=1,ntotc)
          endif
        endif
        return
        end
