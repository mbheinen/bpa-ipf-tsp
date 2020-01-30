C    @(#)getyeq.f	20.3 2/13/96
      subroutine getyeq(k1, k2, id, ksect, yeq, y1, yxy, y2)

C     compute the following 2-port Y-matrices:

C     YEQ - Equivalent parallel 2-port
C     Y1  - 2-port left of section KSECT
C     YXY - 2-port for section KSECT
C     Y2  - 2-port right of section KSECT

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		None
      include 'ipfinc/branch.inc'
c	Global variables used:
c		brtype, ky, brsect, brnch_nxt, brid
      include 'ipfinc/bus.inc'
c	Global variables used:
c		None
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
      include 'ipfinc/prt.inc'
c	Global variables used:
c		None

      character id*1, nxid*1
      integer count1, count2, first1, first2, first, ptr, find_br, sect
      complex yeq(2, 2), y1(2, 2), y2(2, 2), yxy(2, 2)
      double complex yscr(3, 3), y(2,2), yxy_temp(2,2)
      logical found, finished 

      count1 = 0
      count2 = 0

      found = .false.
      first1 = 0
      last1 = 0
      last2 = 0
      first2 = 0
      match = 0

      ptr = find_br(k1, k2, id, sect, 0)

      if (ptr .gt. 0 .and. 
     &   (brtype(ptr) .eq. 4 .or. brtype(ptr) .eq. 9)) 
     &  ptr = brnch_nxt(ptr)

      do while (ptr .gt. 0 .and. 
     &         (ky(ptr) .eq. k2 .and. brid(ptr) .eq. id))
        if (brsect(ptr) .eq. 0) then
c
c	PIEQIV is expecting a double complex.  Use a temporary
c	variable so we don't have to change all the code in 
c	routines that call this routine.
c
          if (ksect .eq. brsect(ptr)) then 
              call pieqiv(ptr, yxy_temp, ierr)
	      do 70 i = 1, 2
		do 60 j = 1, 2
		    yxy(i,j) = cmplx(yxy_temp(i,j))
  60	        continue
  70          continue
          endif
          if (brtype(ptr) .eq. 1) goto 100
        endif
        if (ksect .eq. brsect(ptr)) then
          match = ptr
c
c	  PIEQIV is expecting a double complex.  Use a temporary
c	  variable so we don't have to change all the code in 
c	  routines that call this routine.
c
          call pieqiv(ptr, yxy_temp, ierr)
	  do 90 i = 1, 2
	     do 80 j = 1, 2
		yxy(i,j) = cmplx(yxy_temp(i,j))
  80	     continue
  90      continue
          found = .true.
        elseif (found) then
          count2 = count2 + 1
          if (first2 .eq. 0) first2 = ptr
          last2 = ptr
        else
          count1 = count1 + 1
          if (first1 .eq. 0) first1 = ptr
          last1 = ptr
        endif
  100   continue
        ptr = brnch_nxt(ptr)
      enddo

C     The equivalent pi admittance for a branch with sections is not
C     available; also, it cannot be computed using conventional calls
C     to FIRSEC, NEXSEC, and FINSEC since that would jeopardize the
C     data being stored for the same branch!

C     EQVFIR, EQVNEX, and EQVFIN are entry points in EQVSEC.  It
C     is a sharable image.  All data including the equivalent Y-matri
C     is stored in the calling program.

      nsect = 0
      first = first1
      if (first .eq. 0) first = match
      last = last2
      if (last .eq. 0) last = match
      if (last .eq. 0) last = last1
      ptr = first
      finished = .false.
      do while (ptr .ne. last .and. .not. finished)
        if (brtype(ptr) .ne. 9) then
          call pieqiv(ptr, y, ierr)
          nsect = nsect + 1
          if (nsect .eq. 1) then
            call eqvfird(y, yscr)
          else
            call eqvnexd(y, yscr)
          endif
        endif
        if (ptr .eq. last) then
           finished = .false.
        else
           ptr = brnch_nxt(ptr)
        endif
      enddo
      call eqvfin(yeq, yscr)

C     Step 1. YEQ is now completed. Get sections to left of KSECT.

      nsect = 0
      if (count1 .gt. 0) then
        finished = .false.
        ptr = first1
        do while (ptr .ne. last1 .and. .not. finished)
          if (brtype(ptr) .ne. 9) then
            call pieqiv(ptr, y, ierr)
            nsect = nsect + 1
            if (nsect .eq. 1) then
              call eqvfird(y, yscr)
            else
              call eqvnexd(y, yscr)
            endif
          endif
          if (ptr .eq. last1) then
             finished = .false.
          else
             ptr = brnch_nxt(ptr)
          endif
        enddo
        call eqvfin(y1, yscr)
      else
        do l = 1, 2
          do k = 1, 2
            y1(k, l) = cmplx(0.0, 0.0)
          enddo
        enddo
      endif

C     Step 2. Y1 is now completed. Get sections to right of KSECT

      nsect = 0
      if (count2 .gt. 0) then
        finished = .false.
        ptr = first2
        do while (ptr .ne. last2 .and. .not. finished)
          if (brtype(ptr) .ne. 9) then
            call pieqiv(ptr, y, ierr)
            nsect = nsect + 1
            if (nsect .eq. 1) then
              call eqvfird(y, yscr)
            else
              call eqvnexd(y, yscr)
            endif
          endif
          if (ptr .eq. last2) then
             finished = .false.
          else
             ptr = brnch_nxt(ptr)
          endif
        enddo
        call eqvfin(y2, yscr)
      else
        do l = 1, 2
          do k = 1, 2
            y2(k, l) = cmplx(0.0, 0.0)

C           Y2 is now completed.

          enddo
        enddo
      endif
      return
      end
