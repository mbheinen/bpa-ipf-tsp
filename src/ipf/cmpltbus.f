C    @(#)cmpltbus.f	20.7 1/7/99
      subroutine cmpltbus (error)
c                                                                       
c     Completes bus processing by searching through the temporary
c     tables to find the missing bus pointers to connect the data
c     to the bus.
c                                                                       
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tempbsnm.inc'
      include 'ipfinc/xdata.inc'

      common /is_batch / is_batch
c                                                                       
      character xbuf*120
      integer find_bus, error
      logical cmppqcurve           ! switch to compress pqcurve tables
      external bcdbus, bcdxdt, find_bus

      cmppqcurve = .false.
      numconv = 0
      do i = 1, ibuscb
         numconv = numconv + 1
         ix = find_bus (buscb(i), basecb(i))
         jx = ixndx(i)
         if (ix .gt. 0) then
            if (itype(i) .eq. 1) then
c              Remote control buses data
               if (kbsdta(9,jx) .eq. -i) kbsdta(9,jx) = ix
               if (kbsdta(13,jx) .eq. -i) kbsdta(13,jx) = ix
            else if (itype(i) .eq. 2) then
c              Additional customer bus data
               if (kbctbl(1,jx) .eq. -i) then
                  kbctbl(1,jx) = ix
                  call linkcbus (jx, error)
               endif
            else if (itype(i) .eq. 3) then
c              Switched "X" data
               if (xdata(1,jx) .eq. -i) xdata(1,jx) = ix
               if (xdata(2,jx) .eq. -i) xdata(2,jx) = ix
            else if (itype(i) .eq. 4) then
c              P/Q curve data
               if (pqbusptr(jx) .eq. -i) pqbusptr(jx) = ix
            endif
         else if (buscb(i) .eq. '~~~~~~~~') then
            if (itype(i) .eq. 1) then
            else if (itype(i) .eq. 2) then
            else if (itype(i) .eq. 3) then
               xdata(1,jx) = 0.0
               xdata(2,jx) = 0.0
            endif
         else
            if (itype(i) .eq. 1) then
c              Remote control buses data
               if (kbsdta(9,jx) .eq. -i) then
                  kbsdta(9,jx) = 0
                  write (errbuf(1),310) buscb(i), basecb(i)
  310             format (' Non-existant commutating bus ', a8, f6.1)
                  errbuf(2) = ' '                  
                  call bcdbus(jx,xbuf)           
                  write (errbuf(3),160) xbuf(1:80) 
  160             format(13x,'(',a80,')')          
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
               else if (kbsdta(13,jx) .eq. -i) then
                  kbsdta(1,jx) = 7
                  kbsdta(13,jx) = 0
                  write (errbuf(1),320) buscb(i), basecb(i)
  320             format(' Type "BG" or "BX" remote bus ',a8,f6.1,
     &                   ' is non-existant. Bus type changed to "BQ"' ) 
                  errbuf(2) = ' '                  
                  call bcdbus(jx,xbuf)           
                  write (errbuf(3),160) xbuf(1:80) 
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
               endif
            else if (itype(i) .eq. 2) then
c              Additional customer bus data
               if (kbctbl(1,jx) .eq. -i) then
                  kbctbl(1,jx) = 0
                  write (errbuf(1),330) buscb(i), basecb(i)
  330             format (' Non-existant continuation bus ', a8, f6.1)
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
               endif
            else if (itype(i) .eq. 3) then
c              Switched "X" data
               if (xdata(1,jx) .eq. -i) then
                  xdata(1,jx) = 0.0
                  write (errbuf(1),350) buscb(i), basecb(i)
  350             format (' Non-existant X-data bus ', a8, f6.1)
                  if (is_batch .eq. 0) then
                     call prterx ('E',1)
                  else
                     call prterx ('F',1)
                  endif
               else if (xdata(2,jx) .eq. -i) then
                  xdata(2,jx) = 0.0
                  write (errbuf(1),360) buscb(i), basecb(i)
  360             format (' Non-existant remote bus ', a8, f6.1)
                  errbuf(2) = ' '      
                  call bcdxdt(ix,xbuf) 
                  write (errbuf(3),160) xbuf(1:80)  
                  if (is_batch .eq. 0) then
                     call prterx ('E',3)
                  else
                     call prterx ('F',3)
                  endif
               endif
            else if (itype(i) .eq. 4) then
               cmppqcurve = .true.
               if (pqbusptr(jx) .eq. -i) pqbusptr(jx) = 0
                  write (errbuf(1),370) buscb(i), basecb(i)
  370             format (' P/Q Curve data for Non-existant bus ', a8,
     1                    f6.1, ' ignored.')
                  call prterx ('W',1)  
            endif
         endif
      enddo
 
      i = 0
      do while (cmppqcurve .and. i .lt. numcurv) 
c        delete curve data for non-existant busses
         i = i + 1                  
         if (pqbusptr(i) .eq. 0) then
            if (pqbusptr(numcurv) .gt. 0) then
               pg_sched(i) = pg_sched(numcurv)                 
               pqid(i) = pqid(numcurv)
               pqactive(i) = pqactive(numcurv)
               pqnumunit(i) = pqnumunit(numcurv)
               pqbusptr(i) = pqbusptr(numcurv)
               do j = -1,15
                  pqpgen(j,i) = pqpgen(j,numcurv)
                  pqqmax(j,i) = pqqmax(j,numcurv)
                  pqqmin(j,i) = pqqmin(j,numcurv)
               enddo
             else
               numcurv = numcurv - 1 
               i = i - 1
            endif
         endif
      enddo

      write (*, 100) numconv
  100 format (1x, i4, ' deferred name translations ')
      return
      end
