C    @(#)xstep.f	20.3 2/13/96
      subroutine xstep (nt, bold, b1, b2)
C
C     This subroutine finds two discrete steps B1 and B2 which are
C     adjacent to (presumed) discrete value BOLD.
C
C           B1 <= BOLD <= B2 if BOLD => 0 or
C           B1 => BOLD => B2 if BOLD <= 0.
C
C     Input variables:
C
C           NT     - index to XDATA array
C
C     Output variables:
C
C           BOLD   - initial value of bus shunt susceptance
C           B1     - lower discrete value of shunt
C           B2     - upper discrete value of shunt
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/xdata.inc'
 
      logical match
C
C     Find interval B1 => BOLD => B2.
C
      totrek = xdata(3,nt)
      totcap = xdata(4,nt)
      bold = xdata(5,nt) + xdata(6,nt)
      b1 = 0.0
      b2 = 0.0
      match = .false.
      do 110 k = 7, 21, 2
      n = xdata(k,nt)
      if (n .eq. 0) go to 120
      if (bold .lt. 0.0) then
         if (xdata(k+1,nt) .gt. 0.0) go to 110
      else if (bold .gt. 0.0) then
         if (xdata(k+1,nt) .lt. 0.0) go to 110
      else
C
C     If BOLD is zero, set B1 to first negative step and B2 to
C     first positive step.
C
         match = .true.
         if (xdata(k+1,nt) .lt. 0.0 .and. b1 .eq. 0.0) then
            b1 = xdata(k+1,nt)
         else if (xdata(k+1,nt) .gt. 0.0 .and. b2 .eq. 0.0) then
            b2 = xdata(k+1,nt)
         endif
         if (b1 .lt. 0.0 .and. b2 .gt. 0.0) go to 120
         go to 110
      endif
      do 100 l=1,n
         b2 = b2 + xdata(k+1,nt)
         if (.not. match) then
            if (abs(b2 - bold) .lt. 1.0e-3) then
               match = .true.
            else
               b1 = b2
            endif
         else
            if (bold .gt. 0.0) then
               if (b2 .gt. bold .and. abs (b2 - bold) .gt. 1.0e-3) then
                  go to 120
               endif
            else
               if (b2 .lt. bold .and. abs (b2 - bold) .gt. 1.0e-3) then
                  go to 120
               endif
            endif
         endif
  100 continue
  110 continue
      if (bold .lt. totrek .or. bold .gt. totcap) then
         kt = xdata(1,nt)
         write (errbuf(1),112) bold, intbus(kt), intbas(kt), totrek,
     1      totcap
  112    format (' Discrete shunt reactance ',f7.1,' on BX bus '
     1        ,a8,f6.1,' exceeds limits with "X" data:'
     2        ,2f7.1)
         call prterx ('W',1)
      endif
  120 continue
 
      if (.not. match) then
         kt = xdata(1,nt)
         write (errbuf(1),130) bold, intbus(kt), intbas(kt), b1, b2
  130    format (' Reactance ',f7.1,' on BX bus ',a8, f6.1,
     1      ' is not properly discretized :', 2f7.1)
         call prterx ('W',1)
      endif
 
      return
      end
