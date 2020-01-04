C    @(#)getpq.f	20.3 2/13/96
        subroutine getpq

C       Fast decoupled load-flow subroutine with compensation for
C       line outage. See Stott-Alsac paper for more details.

        include 'ipfinc/parametr.inc'

        include 'ipfinc/apcom.inc'
c	Global variables used:
c		nbus, pnet1, qnet1, ipyu, mfaru, kykk, ipqv,
c		delp, delq, rpad1, rpad2, rpad3, vi, ei, thi, ykmu
        include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
        include 'ipfinc/time1.inc'
c	Global variables used:
c		time, idebug

C       Convert voltage to rectangular form
c
c	local variables:
c	
        integer pflts, dirio, bufio
c
        call stats (cpub,clkt,pflts,dirio,bufio)
        call fcvcmb (ei,1,thi,1,rpad1,2,nbus)
        call frect (rpad1,2,vi,2,nbus)
        call stats (cpue,clkt,pflts,dirio,bufio)
        time(3) = time(3) + cpue - cpub
C
        do 10 k=1,nbus
           il=ipyu(2,k)
           if(il.eq.0) go to 10
           iptr=ipyu(1,k)
           do i=1,il
              m=mfaru(iptr)
              rpad1(1,iptr) = vi(1,k)
              rpad1(2,iptr) = vi(2,k)
              rpad2(1,iptr) = vi(1,m)
              rpad2(2,iptr) = vi(2,m)
              iptr=iptr+1
           enddo
10      continue
C
C       Using calls to vector routines to calculate all off-diagonal
C       contributions.
C
        n = ipyu(1,nbus) + ipyu(2,nbus) - 1
        call stats (cpub,clkt,pflts,dirio,bufio)
C
C       RPAD3 has VK * CONJG(VM)

        call fcvmul (rpad2,2,rpad1,2,rpad3,2,n,-1)
C
C       RPAD1 has VK * CONJG (VM) * CONJG (YKM)

        call fcvmul (rpad3,2,ykmu,2,rpad1,2,n, 1)
C
C       RPAD2 has CONJG(VK) * VM * CONJG (YKM)

        call fcvmul (rpad3,2,ykmu,2,rpad2,2,n,-1)
C
C       Zero out RPAD3 for P and Q computation
C
        zero = 0.
        call fvfill (zero,rpad3     ,2,nbus)
        call fvfill (zero,rpad3(2,1),2,nbus)
        do 50 k=1,nbus
           il=ipyu(2,k)
           if(il.eq.0) go to 50
           iptr=ipyu(1,k)
           do 40 j=1,il
              m=mfaru(iptr)
C
C             Power at node K
C
              rpad3(1,k) = rpad3(1,k) + rpad1(1,iptr)
              rpad3(2,k) = rpad3(2,k) + rpad1(2,iptr)
C
C             Power at node M
C
              rpad3(1,m) = rpad3(1,m) + rpad2(1,iptr)
              rpad3(2,m) = rpad3(2,m) + rpad2(2,iptr)
              iptr=iptr+1
   40      continue
   50   continue
        call stats(cpue,clkt,pflts,dirio,bufio)
        time(4) = time(4) + cpue - cpub
C
        if (idebug .gt. 10) then
           write(dbug,1500) (i,rpad3(1,i),i=1,nbus)
 1500      format(' P AFTER OFF-DIAG TERMS',/,(5(i5,e13.5)))
           write(dbug,1510) (i,rpad3(2,i),i=1,nbus)
 1510      format(' Q AFTER OFF-DIAG TERMS',/,(5(i5,e13.5)))
        endif
 
        call stats(cpub,clkt,pflts,dirio,bufio)
C
C       RPAD1 has VK**2
C
        call fvmul(ei,1,ei,1,rpad1,2,nbus)
C
C       RPAD2 has VK**2 * CONJG (YKK)
C
        call fcrvmu (cykk,2,rpad1,2,rpad2,2,nbus)
C
C       RPAD1 has total P and Q
C
        call fcvadd (rpad2,2,rpad3,2,rpad1,2,nbus)
        if (idebug .gt. 10) then
           write(dbug,1600) (i,rpad1(1,i),i=1,nbus)
 1600      format(' P AFTER DIAG TERMS',/,(5(i5,e13.5)))
           write(dbug,1610) (i,rpad1(2,i),i=1,nbus)
 1610      format(' Q AFTER DIAG TERMS',/,(5(i5,e13.5)))
        endif
 
        call stats(cpue,clkt,pflts,dirio,bufio)
        time(8) = time(8) + cpue - cpub
C
C       RPAD2 has delta P, delta Q
C
        call fvsub (pnet1,1,rpad1,2,rpad2,2,nbus)
        call fvsub (qnet1,1,rpad1(2,1),2,rpad2(2,1),2,nbus)
C
C       Zero out entries in delta-Q for all PV buses
C
        call flmg (rpad2(2,1),2,ipqv,1,rpad2(2,1),2,nbus)
C
C       Store delta-P / V in array DELP and delta-Q / V in array DELQ
C
        call fvdiv (ei,1,rpad2,2,delp,1,nbus)
        call fvdiv (ei,1,rpad2(2,1),2,delq,1,nbus)
C
        if (idebug .gt. 1)  then
           write(dbug,1150) (i,delp(i),i=1,nbus)
 1150      format(' DELP AT THE END OF GETPQ',/,(5(i5,e13.5)))
           write(dbug,1200) (i,delq(i),i=1,iq)
 1200      format(' DELQ AT THE END OF GETPQ',/,(5(i5,e13.5)))
        endif
        return
        end
