C    @(#)check.f	20.7 5/27/99
      subroutine check (jout)
 
C     THIS SUBROUTINE CHECKS FOR CURRENT AND VOLTAGE VIOLATIONS
C
C          JOUT       OUTAGE NUMBER POINTING TO CLNC
C          IIB        RUNNING COUNTER ON ARRAYS BAD,AND IBAD
C          IPBAD(J)   BASE ADDRESS IN BAD AND IBAD CONTAINING
C                     PROBLEMS FOUND IN THE JTH OUTAGE
C          IBAD( )=-K MEANS THAT THE KTH LINE IN CLNO WAS OVERLOADED
C          IBAD( )=K  MEANS THAT VOLTAGE VIOLATION FOUND AT NODE K
C          IBAD( )=0  MEANS SYSTEM SEPARATION OR NON-CONVERGENCE
C          BAD( )     MAGNITUDE OF CURRENT OR VOLTAGE
C          BAD( )=0.0 WHEN IBAD( )=0 MEANS SYSTEM SEPARATION
C          BAD( )= N  WHEN IBAD( )=0 MEANS NON-CONVERGENCE
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/time1.inc'
      include 'ipfinc/comm_mode.inc'

      common /scratch/ kolum(MAXBUS), net(200), mtrx(MAXBUS), 
     &                 last_out, comm_out(MAXBUS)
      integer last_out, comm_out

      integer chek_lout, chek_bout
C
      do i = 1,novl  ! FILL RPAD1 AND RPAD2 WITH 'THETA K' AND 'THETA
         k = klno(1,i)
         m = klno(2,i)
         rpad1(1,i) = thi(k)
         rpad2(1,i) = thi(m)
      enddo
C
C     SUBTRACT 'THETA K' FROM 'THETA M' TO GET 'PSI KM' IN RPAD3
C
      call fvsub (rpad2,2,rpad1,2,rpad3,2,novl)
      if (idebug.gt.1) write(dbug,220) (rpad3(1,j),j=1,novl)
 220  format(' PSI KM IN CHECK',/,(1x,10e12.4))
C
C     COMPARE ANGLE DIFFERENCE TO LIMITS IN CLNO
C
      do 300 i = 1,novl
         k = klno(1,i)
         m = klno(2,i)
         if (chek_lout(jout, i) .ne. 0) go to 300
C
C        CHECK FORWARD ANGLE LIMITS
C
         if (rpad3(1,i) .ge. 0.0 .and. 
     &       rpad3(1,i) .le. clno(1,i)) go to 300
C
C        CHECK REVERSE ANGLE LIMITS
C
         if (rpad3(1,i) .lt. 0.0 .and. 
     &       rpad3(1,i) .ge. clno(1,i)) go to 300
C
C        A POSSIBLE OVERLOAD HAS BEEN DETECTED - CALCULATE CURRENT
C
         ykr = clno(5,i)
         yki = clno(6,i)
         ykmr = clno(3,i)
         ykmi = clno(4,i)
C
         ckmr = vi(1,k)*ykr - vi(2,k)*yki + vi(1,m)*ykmr - vi(2,m)*ykmi
         ckmi = vi(2,k)*ykr + vi(1,k)*yki + vi(2,m)*ykmr + vi(1,m)*ykmi
         curm = ckmr**2 + ckmi**2
         if (idebug.gt.1) then
            write(dbug,1000) i,jout,k,m,ykr,yki,ykmr,ykmi,ckmr,ckmi,
     &          curm,vi(1,k),vi(2,k),vi(1,m),vi(2,m)
 1000       format(' CHECK',4i10,/,1x,11e12.4)
         endif
         if (curm .le. 1.0) go to 300
C
         if (idebug.gt.0) write(dbug,285) i,k,m,curm
  285    format(' CURRENT VIOLATION: I,K,M,CURM',3i4,e13.5)
C
C        ENTER TYPE OF VIOLATION - NEGATIVE FOR BRANCH OVERLOAD
C
         if (iib .lt. MXIBAD) then
            ibad(iib) = -i
            bad(iib) = curm
            iib = iib + 1
         else if (iib .eq. MXIBAD) then
            ibad(iib) = -i
            bad(iib) = curm
            iib = iib + 1
            write (errbuf(1),286) jout,nout,i,k,m,iib
  286       format (' Overflow of "IBAD" array: JOUT,NOUT,I,K,M,IIB =',
     &          6i8)
            call prterx ('E',1)
         endif
  300 continue
C
C     CHECK FOR BUS VOLTAGE VIOLATION
C
      if (ipqsln .ne. 0) go to 410
      do 400 k = 1,nbus
         i = inp2opt(k)
c
c        Note: integer function chek_bout() defines outaged buses in
c              common array comm_out().
c
         if (chek_bout(jout, i) .ne. 0) go to 400
         if (comm_out(i) .ne. 0) go to 400
         if (ei(i) .lt. vlow(i)) go to 360
         if (ei(i) .le. vhi(i)) go to 400
C
C        ENTER TYPE OF VIOLATION - POSITIVE FOR BUS VOLTAGE VIOLATION
C
  360    if (iib .lt. MXIBAD) then
            ibad(iib) = i
            bad(iib) = ei(i)
            iib = iib + 1
         else if (iib .eq. MXIBAD) then
            ibad(iib) = i
            bad(iib) = ei(i)
            iib = iib + 1
            write (errbuf(1),362) jout,nout,i,k,0,iib
  362       format (' Overflow of "IBAD" array: JOUT,NOUT,I,K,M,IIB =',
     &         6i8)
            call prterx ('E',1)
         endif
         if (idebug.gt.0) write(dbug,320) i,ei(i),vlow(i),vhi(i)
  320    format(' VOLTAGE VIOLATION: I,EI,VLOW,VHI',i5,3e13.5)
  400 continue
  410 continue
      return
      end
