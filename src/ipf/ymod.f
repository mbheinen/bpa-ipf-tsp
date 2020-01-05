C    @(#)ymod.f	20.3 2/13/96
      subroutine ymod (jout,isw,noconv)

c     This subroutine modifies the admittance elements. The elements 
c     in the upper-triangular part are modified for computing real and 
c     reactive power, and those in the lower-triangular part are 
c     modified the same way so that B" can be computed properly.
C
C           JOUT = OUTAGE NUMBER POINTING TO CLINE
C           ISW = -1   TAKE OUT A LINE
C           ISW =  1   RESTORE A LINE TAKEN OUT PREVIOUSLY BY ISW=-1
C           NOCONV=0   POWER FLOW CONVERGED,
C                 =1   POWER FLOW DIDN'T CONVERGE
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/lfiles.inc'
 
      common /time1/  time(10),idebug
 
      k = min0( klnc(1,jout), klnc(2,jout) )
      m = max0( klnc(1,jout), klnc(2,jout) )
      ykmr = clnc(1,jout)
      ykmi =-clnc(2,jout)
      if (k .eq. klnc(1,jout)) then
         yk  = -clnc(3,jout)
         ym  = -clnc(4,jout)
      else
         yk  = -clnc(4,jout)
         ym  = -clnc(3,jout)
      endif
      iptru = ipyu(1,k)
      klu = ipyu(2,k)
C
C     FIND WHERE THE OFF-DIAG ELEMENT IS STORED IN THE UPPER-TRI.
C
      do 100 l=1,klu
      mt = mfaru(iptru)
      if (mt .eq. m) go to 200
  100 iptru = iptru + 1
      write(dbug,1100) k,m,iptru,klu
 1100 format(/,' PROGRAMMING ERROR IN YMOD',4i10)
      call erexit
  200 continue
C
C     FIND WHERE THE OFF-DIAG ELEMENT IS STORED IN THE LOWER-TRI.
C
      iptrl = ipyl(1,m)
      kll = ipyl(2,m)
C
      do 220 l=1,kll
      mt = mfarl(iptrl)
      if (mt.eq.k) go to 240
  220 iptrl = iptrl + 1
      write(dbug,1100) k,m,iptrl,kll
      call erexit
  240 continue
      if (idebug.gt.1) write(dbug,1000) k,m,iptru,klu,ykmr,ykmi,yk,ym
 1000 format(/,' K,M,IPTRU,KLU,YKMR,YKMI,YK,YM',4i5,4f11.3)
C
      if (isw .eq. 1) go to 300
C
C     REMOVING A BRANCH CONNECTING NODES K AND M - MODIFY DIAGONAL 
c     TERM FIRST
C
      cykk(1,k) = cykk(1,k) - ykmr
      cykk(2,k) = cykk(2,k) - ykmi - yk
      cykk(1,m) = cykk(1,m) - ykmr
      cykk(2,m) = cykk(2,m) - ykmi - ym

C     MODIFY OFF-DIAG TERM

      ykmu(1,iptru) = ykmu(1,iptru) + ykmr
      ykmu(2,iptru) = ykmu(2,iptru) + ykmi
      ykml(1,iptrl) = ykmu(1,iptru)
      ykml(2,iptrl) = ykmu(2,iptru)
      go to 400
C
C     RESTORING A BRANCH CONNECTING NODES K AND M
C
  300 cykk(1,k) = cykk(1,k) + ykmr
      cykk(2,k) = cykk(2,k) + ykmi + yk
      cykk(1,m) = cykk(1,m) + ykmr
      cykk(2,m) = cykk(2,m) + ykmi + ym
C
      ykmu(1,iptru) = ykmu(1,iptru) - ykmr
      ykmu(2,iptru) = ykmu(2,iptru) - ykmi
      ykml(1,iptrl) = ykmu(1,iptru)
      ykml(2,iptrl) = ykmu(2,iptru)
C
  400 continue
      if (idebug.gt.1)
     1   write(dbug,1200) cykk(1,k),cykk(2,k),cykk(1,m),cykk(2,m),
     2                 ykmu(1,iptru),ykmu(2,iptru)
 1200 format(/,' MODIFIED ADMIT IN YMOD',3(2e13.4,5x))
 
      return
      end
