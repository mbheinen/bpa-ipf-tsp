C    @(#)typesw.f	20.4 5/27/98
      subroutine typesw (iqsw)

C     This subroutine handles the type switching logic. All PV buses 
C     are checked for Q limit violations. If a limit is violated, the 
C     bus is switched to type PQ with Q held at the relevant limit.
C
C     IQSW = The minimum node number where type switched.
C     IQSW = 20000 if no switching occured.

      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/lfiles.inc'
 
      common /time1/  time(10),idebug
      common /toler/ toler(MXCPBS)
 
c     Note: This variable can be changed with a symbolic debugger
      common /term_debug/ iterm

      iqsw = 20000
      if (igenq .eq. 0) go to 1100
 
      do 1000 j = 1, igenq
      k = iqlim(j)
      if (k .lt. 0) go to 1000
 
C     PROCESS PV NODES ONLY IF REAL POWER IS NEAR CONVERGENCE
 
      ptol = toler(k)
 
      if (abs(delp(k)) .gt. ptol) go to 1000
      qtol = 2.0*ptol
 
C     FIND Q LIMITS WITH SHUNTS
 
      qhik = qhi(j)
      qlok = qlow(j)
      shnt = shunt(j)
      if (shnt .gt. 0.) qhik = qhik + (ei(k)-vhold(j))**2 * shnt
      if (shnt .lt. 0.) qlok = qlok + (ei(k)-vhold(j))**2 * shnt
      if (idebug.gt.0) write(dbug,100) k,qhik,qlok,rpad1(2,k),shnt
  100                  format(' K,QHIK,QLOK,Q(K),SHNT',i5,4e14.5)
 
      if (ipqv(k) .gt. 0) then
 
         if (iterm .ne. 0) then
            write (*,110) k, qhik, qlok, rpad1(2,k), shnt, qstate(j),
     &                    ipqv(k)
         endif
         write (dbug,110) k, qhik, qlok, rpad1(2,k), shnt, qstate(j),
     &                    ipqv(k)
  110    format(' TYPESW ERROR - K,QHIK,QLOK,Q(K),SHNT,QSTATE,IPQV ',
     1      i5,4e12.5,2i4)
         go to 1000
      endif
 
      if (ipqv(k) .lt. 0) go to 500
 
C     Check Q-limits
 
      if (rpad1(2,k)-qlok .ge. -qtol) go to 200
 
C     Lower limit violated. Switch type to PQ.
 
      ipqv(k) = -1
      qnet1(k) = qlok
      iqsw = min0(k,iqsw)
      if (idebug.gt.0) write (dbug,150) iqsw,k,rpad1(2,k),qnet1(k)
      if (iterm .gt. 0) write (*,150) iqsw,k,rpad1(2,k),qnet1(k)
  150 format(' TYPESW - LOWER Q LIMIT VIOLATED ',2i4,2f16.5)
      go to 900
 
  200 if (rpad1(2,k)-qhik .le. qtol) go to 1000
 
C     Upper limit violated. Switch type to PQ.
 
      ipqv(k) = -2
      qnet1(k) = qhik
      iqsw = min0(k,iqsw)
      if (idebug.gt.0) write (dbug,250) iqsw,k,rpad1(2,k),qnet1(k)
      if (iterm .gt. 0) write (*,250) iqsw,k,rpad1(2,k),qnet1(k)
  250 format(' TYPESW - UPPER Q LIMIT VIOLATED ',2i5,2f16.5)
      go to 900
 
C     Update Q-Schedule for a node that has been switched to PQ
 
  500 if (shnt .gt. 0.0) then
 
C        Q_scheduled is updated if SHUNT > 0 and Q at upper limit
 
         if (ipqv(k) .eq. -2) then
 
            qold = qnet1(k)
            qnew = qhik
            dold = delq(k)
            delq(k) = (qnew - rpad1(2,k)) / ei(k)
            if (idebug .gt. 0) write (dbug,700) k,qold,qnew,
     1         dold,delq(k)
            if (iterm .gt. 0) write (*,700) k,qold,qnew,
     1         dold,delq(k)
  700       format(' TYPESW - Node ',i4,' update QNET ',2f16.5,
     1         ' DELQ ',2f16.5)
         endif
 
      else if (shnt .lt. 0.0) then
 
C        Q_scheduled is updated if SHUNT < 0 and Q at lower limit
 
         if (ipqv(k) .eq. -1) then
 
            qold = qnet1(k)
            qnew = qlok
            dold = delq(k)
            delq(k) = (qnew - rpad1(2,k)) / ei(k)
            if (idebug .gt. 0) write (dbug,700) k,qold,qnew,
     1         dold,delq(k)
            if (iterm .gt. 0) write (*,700) k,qold,qnew,
     1         dold,delq(k)
         endif
 
      endif
C
C     Test for Q_restoration
C
      if (qstate(j) .ne. 1 .and. abs (delq(k)) .le. qtol) then
 
         if (ipqv(k) .eq. -1 .and. ei(k) .lt. 0.99 * vhold(j)) then
 
            ipqv(k) = 0
            iqsw = min0(k,iqsw)
            delq(k) = 0
            vold = ei(k)
            ei(k) = vhold(j)
            if (idebug .gt. 0) write (dbug,710) k,vold,ei(k)
            if (iterm .gt. 0) write (*,710) k,vold,ei(k)
  710       format(' TYPESW - Node ',i4,
     1             ' in state Qmin restored to PV :', 2f16.5)
C
C           Counter test for restoration from Q-max is ignored because
C           outage conditions usually cause voltage sag.
C
         endif
 
      endif
 
  900 continue
 1000 continue
 
 1100 return
      end
