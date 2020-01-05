C    @(#)trnfac.f	20.3 2/13/96
      subroutine trnfac
C
C     This subroutine factors the H matrix for the network.
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
 
 
C
      ikec = 1
C
C     Add identity row for slack bus elements
C
      do 610 kt=1,nbslck
         jndex(1,kt) = ikec
C
C     NORMALIZE ROW
C
         amtrx(ikec)=1.0
         ikec=ikec+1
         jndex(2,kt)=ikec
  610 continue
C
C     Define Jacobian elements for bus constraints
C
      do 1020 kt = nbslck+1,ntot
      jndex(1,kt) = ikec
      call trnjac (kt)
      mel=0
  640 mel=korder(mel)
      mt=kolum(mel)
      if (mt-kt) 740,644,642
C
C     Eliminate column MT from working row
C
  740 ik=jndex(2,mt)
      ikstop=jndex(1,mt+1)
      rh=rowh(mel)
      amtrx(ikec)=mt
      amtrx(ikec+1)=rh
      ikec=ikec+2
      krw=mel
C
C     Perform Row MT elimination
C
  800 if (ik.ge.ikstop) go to 640
      ml=amtrx(ik)
      if (ml.gt.max) go to 980
  960 if (kolum(krw)-ml) 970,1010,990
  970 ko=krw
      krw=korder(krw)
      go to 960

 1010 mlc=krw
      go to 1000

  980 max=ml
      ko=lp
      lp=mend
  990 korder(mend)=korder(ko)
      kolum(mend) = ml
      korder(ko)=mend
      mlc=mend
      ko=mend
      mend=mend+1
      rowh(mlc) = 0
 1000 continue
      xh=amtrx(ik+1)
      rowh(mlc) = rowh(mlc) - rh*xh
      ik=ik+2
      go to 800
C
C     Error - no residual diagonal element
C
  642 call erexit
C
C     Normalize row
C
  644 rh=rowh(mel)
      rhin=1.0/rh
      amtrx(ikec)=rhin
      ikec=ikec+1
      jndex(2,kt)=ikec
  690 mel = korder(mel)
      if (mel.eq.0) go to 1020
      mt = kolum(mel)
      ah = rowh(mel) * rhin
      amtrx(ikec)=mt
      amtrx(ikec+1) = ah
      ikec=ikec+2
      go to 690

 1020 continue
      jndex(1,ntot+1) = ikec
C
C     END OF BUS LOOP
C
      return
      end
