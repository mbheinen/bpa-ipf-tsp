C    @(#)dcjacb.f	20.5 8/19/99
        subroutine dcjacb   

C       COMPUTE DQ/DV FOR NODE KT;   
c       (IGNORE FACT THAT NODE KT MAY BE PV.)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dcsln.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/optim1.inc'
 
      double precision ek, em, rh

      max = 0   
      kl = 0  
      lp = 0  
      if (kt.gt.ntot) go to 210 
      kta = kt + ntota  
      lsw = 1 
      ek = e(kt)  
      aik = 0.0d0   
      do 180 l = km(kt), km(kt)-1+kmlen(kt)   
      mt = ikmu(l)                                   
      mta = mt + ntota  
      lp = lp + 1   
      em = e(mt)  
      bkm = bkmu(l)                                
      rh = ek * em * bkm  
      aik = aik + rh
      go to (130,140,150), lsw
  120 lp = lp-1
      go to 180
 
  130 if (mt.le.nbslck) go to 120
      lsw = 2
  140 if (mt.lt.kt) go to 150
      max = kta
      kl = lp
      lsw = 3
      korder(lp) = lp + 1
      kolum(lp) = kta
      lp = lp + 1
  150 if (ikk(1,mta).eq.2) go to 160
      lp = lp - 1
      go to 180
 
  160 rh = bkm
  170 max = mta
      kolum(lp) = mta
      rowh(lp) = -rh
      korder(lp) = lp + 1
 
  180 continue
      if (kl.ne.0) go to 190
      kl = lp + 1   
      lp = kl 
      kolum(lp) = kta 
      korder(lp) = lp + 1   
      max = kta 
  190 aik = -bkku(kt)                                  
      rowh(kl) = aik
C       
C     TEST FOR IDEAL BUS TIE BRANCH 
C       
      ix = nspar(kt)                              
      if (ix.eq.0) go to 210
C       
C     ADD A COLUMN FOR TIE ENTITY   
C       
  200 lp = lp + 1   
      korder(lp) = lp + 1   
      iax = txtie(8,ix)
      max = iabs (iax)  
      kolum(lp) = max   
      rh = 1.0d0
      if (iax.lt.0) rh = -1.0d0
      rh = rh/ek
      rowh(lp) = rh 
C       
C     CHECK FOR ADDITIONAL BUS TIES 
C       
      ix = ix + 1   
      if (ix.gt.ntxtie) go to 210   
      if (txtie(1,ix).eq.kt) go to 200 
  210 continue  
      korder(lp) = 0  
      mend = lp + 1 
      korder(0) = 1   
      return
      end   
