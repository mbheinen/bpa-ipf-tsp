C    @(#)sortpqcv.f	20.5 1/7/99
c****************************************************************
c
c   File: sortpqcv.for
c   Purpose: Sort and check the P/Q curve data
c
c   Author: Albert Schmidt  Date: 15 Oct 1992
c                       Modified: 
c   Called by: dfnbse.for
c
c****************************************************************
c
      subroutine sortpqcv
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/bus.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'

      logical removepq

      include 'ipfinc/bstype.inc'
      integer bustyp

      if (numcurv .gt. 0) then
         removepq = .false.
         i = numcurv
         do while (i .gt. 0)
c
c           check for valid bus type that the pqcurve is for...
c
            bustyp = kbsdta(1,pqbusptr(i))
            if (bustyp.ne.BSTYP_BE .and. bustyp.ne.BSTYP_BG .and.
     1          bustyp.ne.BSTYP_BQ .and. bustyp.ne.BSTYP_BX .and.
     2          bustyp.ne.BSTYP_BS) then
               write (errbuf(1),100) bus(pqbusptr(i)),base(pqbusptr(i))
  100          format(' Bus ',a8,f6.1,' is illegal type for PQ Curve')
               errbuf(2) = '       Data only valid for ' //
     1                     'BE, BG, BQ, BX, & BS buses.'
               call prterx ('W',2)
               removepq = .true.
            else

c              curve data on a valid bus type. Sort the points and 
c              data check.

               call pqsort(pqpgen(1,i), pqqmax(1,i), pqqmin(1,i), ierr)
               if (ierr .eq. 1) then
                  write (errbuf(1),110) bus(pqbusptr(i)), 
     1                                  base(pqbusptr(i))
  110             format (' No points on P/Q curve for bus ',a8,f6.1)
                  errbuf(2) = '    Data removed....'
                  call prterx ('W',2)
                  removepq = .true.
               else if (ierr .eq. 2) then
                  write (errbuf(1),120) bus(pqbusptr(i)), 
     1                                  base(pqbusptr(i))
  120             format (' Duplicate P values on P/Q curve for bus ',
     1                    a8,f6.1)
                  errbuf(2) = '    Data removed....'
                  call prterx ('W',2)
                  removepq = .true.
               else if (ierr .eq. 3) then
                  write (errbuf(1),130) bus(pqbusptr(i)), 
     1                                  base(pqbusptr(i))
  130             format (' *** Warning - all QMAX values are zero on ',
     1                    'P/Q curve for bus ',a8,f6.1)
                  errbuf(2) = '              Data may be missing....'
                  call prterx ('W',2)
               else if (ierr .eq. 4) then
                  write (errbuf(1),140) bus(pqbusptr(i)), 
     1                                  base(pqbusptr(i))
  140             format (' *** Warning - all QMIN values are zero on ',
     1                    'P/Q curve for bus ',a8,f6.1)
                  errbuf(2) = '              Data may be missing....'
                  call prterx ('W',2)
               else if (ierr .gt. 0) then
                  write (errbuf(1),300) bus(pqbusptr(i)), 
     1                                  base(pqbusptr(i))
  300             format (' *** Warning - unknown error on ',
     1                    'P/Q curve for bus ',a8,f6.1)
                  errbuf(2) = '    Data removed....'
                  call prterx ('W',2)
                  removepq = .true.
               endif
            endif
            if (removepq) then       ! copy last curve to overwrite
               pg_sched(i) = pg_sched(numcurv)                 
               pqid(i) = pqid(numcurv)
               pqactive(i) = pqactive(numcurv)
               pqbusptr(i) = pqbusptr(numcurv)
               pqnumunit(i) = pqnumunit(numcurv)
               do j = -1,15
                  pqpgen(j,i) = pqpgen(j,numcurv)
                  pqqmax(j,i) = pqqmax(j,numcurv)
                  pqqmin(j,i) = pqqmin(j,numcurv)
               enddo
               numcurv = numcurv - 1
            endif
            i = i - 1
            removepq = .false.
        enddo
      endif
     
      return
      end
