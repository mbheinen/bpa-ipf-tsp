C    @(#)fstout.f	20.12 5/27/99
      subroutine fstout

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/apcom.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/miscfile.inc'

      character null * 1, linefeed * 1, capital * 40, tempc * 60,
     &          word(50) * 60, filename * 60, outfile * 60, printf * 60
      integer open_file, status, newtyp(13)

      data newtyp /1,2,3,1,1,1,7,7,7,1,7,1,1/
C
      null = char(0)
      linefeed = char(10)

C     Set up the page header

      write (outbuf, 10) prgvsn
   10 format('BPA POWER FLOW PROGRAM VERSION:',a)
      call hedlod
 
      call uscan(inrcd(2:), word, nwrd, '=',' ,'//linefeed)
      filename = ' '
      outfile = ' '
      inpold = inp

      iwrd = 1
      do while (iwrd .le. nwrd)
        tempc = capital(word(iwrd))
        if ( tempc .eq. 'FILE') then
           iwrd = iwrd + 1
           if (word(iwrd) .eq. '=') iwrd = iwrd + 1
           filename = word(iwrd)
        else if ( tempc .eq. 'OUTFILE') then
           iwrd = iwrd + 1
           if (word(iwrd) .eq. '=') iwrd = iwrd + 1
           outfile = word(iwrd)
        endif
        iwrd = iwrd + 1
      enddo
      if (filename .ne. ' ') then
        call close_file (lunscr)
        inpold = inp
        inp = lunscr
        status = open_file(lunscr,filename,'F','R',iost)
        if (status .ne. 0) then
          lc = lastch( filename )
          if ( lc .eq. 0 ) lc = 1
          write( errbuf(1), 100 ) filename(1:lc)
  100     format(' Error opening file: ', a )
          call prterx ('E', 1)
          write( outbuf, 110 ) filename(1:lc), null
  110     format(' !!! ERROR OPENING FILE :  ',a,' !!!',a)
          call prtout (1)
          goto 900
        endif
      endif

      if (outfile .ne. ' ') then
        inquire (unit=lprt, name=printf)
        call close_file (lprt)
        status = open_file (lprt, outfile, 'FF', 'W', ios)
        if (status .ne. 0) go to 900
      endif

      call dbgprt(0)

C     READ INPUT TEXT, DEFINE CAP ARRAYS

      call cntinp

C     SIMPLIFY BUS TYPE ASSIGNMENTS

      do i = 1,ntot
         ntypu(i) = newtyp(ntypu(i))
      enddo
C
C     IF (KASE1(26).NE.0) CALL RESKED
C
C     DETERMINE EQUIVALENT SYSTEM

      call ereduc
C
C     BUILD ARRAY OF REACTIVE LIMITS

      call capqlt
C
C     BUILD ARRAY OF VOLTAGE LIMITS

      call capvlt
C
C     TRANSFER /CONT/ INTO /APIN/
C
      itmax = npass
      cx1 = tol
      cx2 = psm
      cx4 = vcrit
      ipqsln = irect
C
C     PERFORM NETWORK REDUCTION
C
      call equivy
C
C     TRANSFER KEY COUNTERS INTO /APIN/
C
      nbus = ntot
C
C     PERFORM SOLUTION AND OUTPUT RESULTS
C
      call apsoln
      call dbgprt(0)

  900 inp = inpold
      if (outfile .ne. ' ') then
        call close_file (lprt)
c
c       Strip ";" from name 
c
        last = index (printf, ';')
        if (last .gt. 0) printf(last:) = ' '
        status = open_file (lprt, printf, 'FF', 'W', ios)
      endif
      return
      end
