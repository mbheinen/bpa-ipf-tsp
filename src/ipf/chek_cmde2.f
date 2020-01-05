C    @(#)chek_cmde2.f	20.4 5/27/99
      subroutine chek_cmde2 (jout)
 
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

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/cmde_com.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/time1.inc'
      include 'ipfinc/comm_mode.inc'

      common /scratch/ kolum(MAXBUS), net(200), mtrx(MAXBUS), 
     &                 last_out, comm_out(MAXBUS)
      integer last_out, comm_out

      integer chek_lcmd2, chek_bcmd2
C
C     Compare angle difference to limits in CLNO
C
      do 300 i = 1,novl
         if (chek_lcmd2(jout, i) .ne. 0) go to 300
C
C        CHECK FORWARD ANGLE LIMITS
C
         k = klno(1,i)
         m = klno(2,i)
         kt = inp2opt(k)
         mt = inp2opt(m)
         torque = vangle(kt) - vangle(mt)
         if (torque .ge. 0.0 .and. torque .le. clno(1,i)) go to 300
C
C        CHECK REVERSE ANGLE LIMITS
C
         if (torque .lt. 0.0 .and. torque .ge. clno(1,i)) go to 300
C
C        A POSSIBLE OVERLOAD HAS BEEN DETECTED - CALCULATE CURRENT
C
         ykr = clno(5,i)
         yki = clno(6,i)
         ykmr = clno(3,i)
         ykmi = clno(4,i)
C
         ckmr = e(kt)*ykr - f(kt)*yki + e(mt)*ykmr - f(mt)*ykmi
         ckmi = f(kt)*ykr + e(kt)*yki + f(mt)*ykmr + e(mt)*ykmi
         curm = ckmr**2 + ckmi**2
         if (idebug.gt.1) then
            write(dbug,1000) i,jout,k,m,ykr,yki,ykmr,ykmi,ckmr,ckmi,
     &          curm,e(kt),f(kt),e(mt),f(mt)
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
      do 400 i = 1, ntot_alf
         nb = alf2inp(i)
         kt = inp2opt(nb)
c
c        Note: integer function chek_bcmd2 () defines outaged buses in
c              common array comm_out().
c
         if (chek_bcmd2 (jout, kt) .ne. 0) go to 400
         if (comm_out(nb) .ne. 0) go to 400
         if (vmag(kt) .lt. vlimn(kt)) go to 360
         if (vmag(kt) .le. vlimx(kt)) go to 400
C
C        ENTER TYPE OF VIOLATION - POSITIVE FOR BUS VOLTAGE VIOLATION
C
  360    if (iib .lt. MXIBAD) then
            ibad(iib) = nb
            bad(iib) = vmag(kt)
            iib = iib + 1
         else if (iib .eq. MXIBAD) then
            ibad(iib) = nb
            bad(iib) = vmag(kt)
            iib = iib + 1
            write (errbuf(1),362) jout,nout,nb,kt,0,iib
  362       format (' Overflow of "IBAD" array: JOUT,NOUT,NB,K,M,IIB =',
     &         6i8)
            call prterx ('E',1)
         endif
         if (idebug.gt.0) write(dbug,320) nb, vmag(kt), vlimn(kt),
     &      vlimx(kt)
  320    format(' VOLTAGE VIOLATION: I,VMAG,VMIN,VMAX',i5,3e13.5)
  400 continue
  410 continue
      return
      end
