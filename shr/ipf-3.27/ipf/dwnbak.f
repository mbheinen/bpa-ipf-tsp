C    @(#)dwnbak.f	20.3 2/13/96
        subroutine dwnbak (ind)
C
C       This subroutine performs the operation INV[A] * [DPT].
C
C       IND is the factored index for [A] or [C] matrix.
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
 
        save
C
        dimension ind(2,*)
 
        do 390 kt = ntota+nbslck+1,ntotx-1
        dt = dpt(1,kt)
        ik = ind(1,kt)
        ikstop = ind(2,kt)-1
        if (ik.gt.ikstop) go to 390
  370   if (ik.ge.ikstop) go to 380
        mt = amtrx(ik)
        dt = dt-dpt(1,mt)*amtrx(ik+1)
        ik = ik+2
        go to 370
  380   dpt(1,kt) = dt*amtrx(ikstop)
  390   continue
C
C       BACK SUBSTITUTION
C
        do 430 kt = ntotx-1,ntota+nbslck+1,-1
        ik = ind(2,kt)
        ikstop = ind(1,kt+1)-1
        dt = dpt(1,kt)
  410   if (ik .ge. ikstop) go to 420
        mt = amtrx(ik)
        dt = dt-dpt(1,mt)*amtrx(ik+1)
        ik = ik+2
        go to 410
  420   dpt(1,kt) = dt
  430   continue
        return
C
C       Entry DWNBKT point performs the operation
C
C       INV[A(transpose)] * [DPT].
C
C       IND is the factored index for [A] or [C] matrix.
C
C       Step 1: Perform U(n)t * U(n-1)t * ... * U(1)t operation
C
        entry dwnbkt (ind)
 
        do 450 kt = ntota+nbslck+1,ntotx-1
        ik = ind(2,kt)
        dp = dpt(1,kt)
        do 440 ik = ind(2,kt),ind(1,kt+1)-1,2
        mt = amtrx(ik)
        xh = amtrx(ik+1)
        dpt(1,mt) = dpt(1,mt) - dp*xh
  440   continue
  450   continue
C
C       Step 2. Perform D(n)t * L(n)t * D(n-1)t * ... *
C                       L(2)t * D(1)t operation
C
        do 470 kt = ntotx-1,ntota+nbslck+1,-1
        ik = ind(2,kt) - 1
        rhin = amtrx(ik)
        dp = dpt(1,kt) * rhin
        dpt(1,kt) = dp
        do 460 ik = ind(1,kt),ind(2,kt)-3,2
        mt = amtrx(ik)
        rh = amtrx(ik+1)
        dpt(1,mt) = dpt(1,mt) - rh*dp
  460   continue
  470   continue
        return
        end
