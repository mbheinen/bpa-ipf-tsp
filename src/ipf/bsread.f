C    @(#)bsread.f	20.4 2/13/96
      subroutine bsread
c
c     read bus records from different sources.
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		yptr
      include 'ipfinc/arcntl.inc'
c	Global variables used.
c		ntotic, 
      include 'ipfinc/area.inc'
c	Global variables used.
c		none
      include 'ipfinc/arsort.inc'
c	Global Variables used:
c		none
      include 'ipfinc/blank.inc'
c	Global variables used
c		ntot, ntot2, ltot, ntotc, kxtot, 
c		nztot, natot, card, kspare (equiv to spare),
      include 'ipfinc/branch.inc'
c	Global variables used:
c		ltot2
      include 'ipfinc/bus.inc'
c	Global variables used:
c		none		
      include 'ipfinc/cbsorc.inc'
c	Global variables used:
c		None
      include 'ipfinc/cbsort.inc'
c	Global variables used
c		None
      include 'ipfinc/cbus.inc'
c	Global variables used.
c		None
      include 'ipfinc/filnam.inc'
c	Global variables used.
c		bsdnam, bsbrnm	
      include 'ipfinc/jobctl.inc'
c	Global variables used
c		none	
      include 'ipfinc/lfiles.inc'
c	Global variables used.
c		inp
      include 'ipfinc/mrgtxt.inc'
c	Global variables used:
c		nbsmrg, nbrmrg, logmbs 
      include 'ipfinc/ordsta.inc'
c	Global variables used.
c		None
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
      include 'ipfinc/tempbsnm.inc'
C	Global variables used:
c		ibuscb, 
      include 'ipfinc/xdata.inc'
c	Global variables used:
c		None
c
      common /is_batch / is_batch

      character xbuf*120, null*1, skiprec*8
      integer error
      logical eoffile1, eoffile2, eoffile3, eoiread4, opened

      null = char(0)
      skiprec = '.Cc*! ' // char(10) // char(12)
 
c     kspare(1) = 0 : no base case data present
c                 1 : base case data present-rebuild data
c                 2 : base case data present-do not rebuild data
 
      if (kspare(1) .eq. 0) then     ! base case is NOT present yet...

         ntot=0
         ntot2=0
         ntotc=0
         kxtot=0
         ltot=0
         ltot2=0
         ntotic = 0
         ibuscb=0
         yptr = 0
         call bushinit ()       ! initialize the bus hash tables...
         call tbxhinit ()       ! initialize the tbx hash tables...

      else

         ibuscb=0         
         call rebldbse ()       ! Reinitialize base case if necessary
c
      endif
 
      kberr=1
      natot=0
      nztot=0
      ntoosw=1
      ntocsw=1
c
c     bus data input files in order of processing:
c
c             description     logical unit   file name
c
c     file1:  bus data          logmbs       scratch
c     file2:  bus data          busfile      bsdnam
c     file3:  bus-branch data   busbrn       bsbrnam
c     file4:  sys$input         inp          sys$input
c
 
      inquire (unit=logmbs, opened=opened)
      eoffile1 = .not. opened     ! LOGMBS file
      jbstot = 0
      do while (.not. eoffile1)  ! Read bus merge file
         read (logmbs, 100, end=110) xbuf
  100    format (a)
         jbstot = jbstot + 1
         card = xbuf(1:1)
         if (index ('B+XQ', card) .ne. 0) then
            call loadbus (xbuf,error) ! Process bus record
         else if (index ('AI', xbuf(1:1)) .ne. 0) then
            call loadarea (xbuf,error)  ! Process area record
         else if (index ('ELTR',card) .ne. 0) then
c
c           Store Branch data text in LOGMBR and leave
c           in INRCD for BRREAD
c
            call brntxt (xbuf)
            inrcd = xbuf
         else if (xbuf(1:2) .eq. '.#') then
c
c           Process " .# " case info 
c
            call casetxt (xbuf)
            inrcd = xbuf
         else if (index (skiprec, card) .ne. 0) then ! Skip illegal
C                                                    ! records
         else
            eoffile1 = .true.
         endif
         if (jbstot .ge. nbsmrg) eoffile1 = .true.
      enddo
  110 eoffile1 = .true.
      jbstot = nbrmrg + 1
c
      eoffile2 = .false.     ! BSDNAM file
      if (bsdnam .ne. ' ') then
         ierr=0
         call opnfil (busfil, bsdnam, ierr)
         if (ierr .ne. 0) then
            last = lastch (bsdnam)
            write (errbuf(1), 130) bsdnam(1:last), busfil
  130       format (' Cannot open file ', a, ' on logical unit ', i2)
            call prterx ('W', 1)
            goto 1000
         else
            do while (.not. eoffile2)  ! Read BSDNAM file
               read (busfil, 100, end=140) xbuf
               card = xbuf(1:1)
               if (index ('B+XQ', card) .ne. 0) then
                  call loadbus (xbuf,error) ! Process bus record
               else if (index ('AI', xbuf(1:1)) .ne. 0) then
                  call loadarea (xbuf,error)  ! Process area record
               else if (index ('ELTR',card) .ne. 0) then
c
c                 Store Branch data text in LOGMBR and leave
c                 in INRCD for BRREAD
c
                  call brntxt (xbuf)
                  inrcd = xbuf
               else if (xbuf(1:2) .eq. '.#') then
c
c                 Process " .# " case info 
c
                  call casetxt (xbuf)
                  inrcd = xbuf
               else if (index (skiprec, card) .ne. 0) then ! Skip
C                                                          ! illegal
C                                                          ! records
               else
                  eoffile2 = .true.
               endif
	    enddo
  140       eoffile2 = .true.
            call close_file (busfil)
            bsdnam = ' '
         endif
      endif
c
      eoffile3 = .false.     ! BSBRNM file
      if (bsbrnm .ne. ' ') then
         ierr=0
         call opnfil (busbrn, bsbrnm, ierr)
         if (ierr .ne. 0) then
            last = lastch (bsbrnm)
            write (errbuf(1), 130) bsbrnm(1:last), busbrn
            call prterx ('W', 1)
            goto 1000
         else
            do while (.not. eoffile3)  ! Read BSBRNM file
               read (busbrn, 100, end=160) xbuf
               card = xbuf(1:1)
               if (index ('B+XQ', card) .ne. 0) then
                  call loadbus (xbuf,error) ! Process bus record
               else if (index ('AI', xbuf(1:1)) .ne. 0) then
                  call loadarea (xbuf,error)  ! Process area record
               else if (index ('ELTR',card) .ne. 0) then
c
c                 Store Branch data text in LOGMBR and leave
c                 in INRCD for BRREAD
c
                  call brntxt (xbuf)
                  inrcd = xbuf
               else if (xbuf(1:2) .eq. '.#') then
c
c                 Process " .# " case info 
c
                  call casetxt (xbuf)
                  inrcd = xbuf
               else if (index (skiprec, card) .ne. 0) then ! Skip
C                                                          ! illegal
C                                                          ! records
               else
                  eoffile3 = .true.
               endif
            enddo
  160       eoffile3 = .true.
            call close_file (busbrn)
            bsbrnm  = ' '
         endif
      endif
 
      eoiread4 = .false.     ! SYS$INPUT
      xbuf = null            ! First read should be XBUF < BUF
      do while (.not. eoiread4)  ! Read SYS$INPUT file
         if (xbuf .eq. null) then
            xbuf = buf
         else
            read (inp, 100, end=170) xbuf
            go to 190
  170       write (errbuf(1), 180)
  180       format(' Unexpected end-of-file in input stream,',
     &             ' "( stop )" assumed.')
            call prterx ('W',1)
            xbuf ='( STOP ) END OF FILE'
            eoiread4 = .true.
  190       continue
         endif
         card = xbuf(1:1)
         if (index ('B+XQ', card) .ne. 0) then
            call loadbus (xbuf,error) ! Process bus record
         else if (index ('AI', xbuf(1:1)) .ne. 0) then
            call loadarea (xbuf,error)  ! Process area record
         else if (index ('ELTR',card) .ne. 0) then
c
c           Store Branch data text in LOGMBR
c
            call brntxt (xbuf)
         else if (xbuf(1:2) .eq. '.#') then
c
c           Process " .# " case info 
c
            call casetxt (xbuf)
            inrcd = xbuf
         else if (index (skiprec, card) .ne. 0) then ! Skip illegal
C                                                    ! records
         else
            eoiread4 = .true.
            inrcd = xbuf
            buf = xbuf
         endif
      enddo
 
      if (ntot .eq. 0) then
c
         write (errbuf(1),200)
  200    format(' No bus data in system.')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         goto 1000
      endif
c
      kspare(34) = 0
      return
c
 1000 buf = '( STOP ) Error in BSREAD'
      card = '('
      jobreq(1) = 'STOP'
      return
      end
