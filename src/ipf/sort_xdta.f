C    @(#)sort_xdta.f	20.5 3/29/99
      subroutine sort_xdta
c
c     This subroutine compresses deleted entitities from xdata and
c     resorts the array.
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qsdup.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/xsrt.inc'
 
      character xbuf * 120
      logical done
      external komxdt, swpxdt

C     Flag deleted entities.         
 
      do i = 1, kxtot
         if (xdata(1,i) .le. 0.0d0) xdata(1,i) = 19999.0d0
         keysrt(i) = i
      enddo
 
      dupsw = .false.
      if (kxtot .gt. 0) call qiksrt(1, kxtot, komxdt, swpxdt)

      if ( dupsw .or. (xdata(1,kxtot) .eq. 19999.0d0 )) then
 
C        Truncate counter for all deleted XDATA entities
 
         done = .false.
         do while ( kxtot .gt. 0  .and.  .not. done )
            if ( xdata(1,kxtot) .eq. 19999.0d0 ) then
               kxtot = kxtot - 1
            else
               done = .true.
            endif
         enddo
 
      endif
      if ( dupsw .and. kxtot .gt. 0) then
c
c        Flag and remove switched reactance buses..                
 
         j = 1
         k = 2
 
         do while (k .le. kxtot)
            if ( komxdt(j,k) .lt. 0 ) then
               j = j + 1
               if (j .lt. k) call swpxdt(j,k)
               k = k + 1
            else
               write (errbuf(1),150)
  150          format ('Duplicate xdata records ...')
               if (keysrt(j) .gt. keysrt(k)) call swpxdt(j,k)
               call bcdxdt(j,xbuf)
               write (errbuf(2),160) xbuf(1:40)
  160          format (' kept---- ', a)
               call bcdxdt(k,xbuf)
               write (errbuf(3),170) xbuf(1:40)
  170          format (' deleted- ', a)
               call prterx ('W',3)
               k = k + 1
            endif
         enddo
         kxtot = j
      endif
      return
      end
