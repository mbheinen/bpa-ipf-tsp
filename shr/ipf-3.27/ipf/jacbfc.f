C    @(#)jacbfc.f	20.7 10/13/99
      subroutine jacbfc
C
C     Zero subscripts are used in arrays korder, kolum
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dcsln.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/intbus.inc'
C
      kta=kt+ntota
      if (kt.gt.ntot) go to 290
      if (msw.eq.0) go to 100
      if (ikk(1,kta).eq.2) go to 110
      lp = 0
      kolum(0) = 0  
      korder(lp) = 0
      max = 0   
      go to 330 

  100 continue  
      jt=jckikk(kta,3)
c
c     Disable area interchange to improve initial convergence
c
c     if (jt.gt.0) go to 212   

  110 max = 0   
      kl=0  
      lp=0  
      lsw=1 
      ek=e(kt)  
      aik=0.0   
      do 180 l = km(kt), km(kt)-1+kmlen(kt)   
      mt = ikmu(l)                                 
      mta=mt+ntota  
      lp=lp+1   
      em=e(mt)  
      bkm = bkmu(l)                                
      rh=ek*em*bkm  
      aik=aik+rh
      go to (130,140,150), lsw  
  120 lp=lp-1   
      go to 180 

  130 if (mt.le.nbslck) go to 120   
      lsw=2 

  140 if (mt.lt.kt) go to 150   
      max = kta 
      kl=lp 
      lsw=3 
      korder(lp)=lp+1   
      kolum(lp)=kta 
      lp=lp+1   

  150 if (msw.eq.0) go to 170   
      if (ikk(1,mta).eq.2) go to 160
      lp = lp - 1   
      go to 180 

  160 rh = bkm  
  170 max = mta 
      kolum(lp) = mta   
      rowh(lp)=-rh  
      korder(lp)=lp+1   
  180 continue  
      if (kl.ne.0) go to 190
      kl=lp+1   
      lp=kl 
      kolum(lp)=kta 
      korder(lp) = lp + 1   
      max = kta 
  190 if (msw.ne.0) aik = -bkku(kt)                
      rowh(kl) = aik
C       
C     Test for ideal bus tie branch 
C       
      ix = jckikk (kta,10)  
      if (ix.eq.0) go to 210
C       
C     Add a small residual value to diagonal to replace the branch  
C     removed.  
C       
      rowh(kl) = rowh(kl) + 0.001   
C       
C     Add a column for tie entity   
C       
  200 lp = lp + 1   
      korder(lp) = lp + 1   
      iax = txtie(8,ix)
      max = iabs (iax)  
      kolum(lp) = max   
      rh = 1.0  
      if (iax.lt.0) rh = -1.0   
      if (msw.ne.0) rh = rh/ek  
      rowh(lp) = rh 
C       
C     Check for additional bus ties 
C       
      ix = ix + 1   
      if (ix.gt.ntxtie) go to 210   
      if (txtie(1,ix).eq.kt) go to 200 
  210 continue  
      korder(lp)=0  
      mend=lp+1 
      korder(0)=1   
      go to 330 
C       
C     Area Interchange Constraints  
C       
  212 max=0 
      kolum(0)=0
      korder(0)=0   
      lp=0  
      mend=1
      korder(1)=1   
      j1=karea(3,jt)
      j2=j1+karea(4,jt)-1   
      do 280 j=j1,j2
      ix=kaloc(j)   
      iax=iabs(ix)  
      k1=tie(1,iax)
      k2=tie(7,iax)
      ka1=tie(2,iax)   
      ka2=tie(8,iax)   
      kdc=tie(9,iax)   
      if (kdc.ne.0) go to 280   
C       
C     Process phase shifter tie lines. (The following logic is  
C     vulnerable if K1 and K2 both have two or more bus ties!)  
C       
      i1 = jckikk(k1+ntota,10)  
      i2 = jckikk(k2+ntota,10)  
      if (i1 .eq. 0 .or. i2 .eq. 0) then
         go to 228  
      else if (txtie(1,i1) .eq. txtie(2,i2) .and. 
     1         txtie(2,i1) .eq. txtie(1,i2)) then 
      else  
         do 214 i1 = 1, ntxtie  
         if (txtie(1,i1) .eq. k1 .and. txtie(2,i1) .eq. k2)   
     1      go to 216   
  214    continue   
         go to 228  
  216    continue   
      endif 
        
      if (ix .lt. 0) go to 280  
        
      kx = dabs (txtie(8,i1))  
      rh = 1.0  
      if (txtie(8,i1) .lt. 0) rh = -rh 
      if (ka1.ne.jt) rh = -rh   
        
      lp = 0
      go to 222 

  221 if (kolum(lp) - kx) 222,225,224   

  222 ko=lp 
      lp=korder(lp) 
      if (lp.ne.0) go to 221
      max=kx

  224 korder(mend)=korder(ko)   
      korder(ko)=mend   
      kolum(mend)=kx
      ko=mend   
      lp=mend   
      mend=mend+1   
      rowh(lp)=0.0  
        
  225 continue  
      rowh(lp) = rowh(lp) + rh  
      ko = 0
      lp = korder(ko)   
      go to 280 
        
  228 mt=k1 
      if (ix.lt.0) mt=k2
      if (mt.le.nbslck) go to 280
      mta=mt+ntota  
  230 if (kolum(lp)-mta) 240,260,250
  240 ko=lp 
      lp=korder(lp) 
      if (lp.ne.0) go to 230
      max=mta   
  250 korder(mend)=korder(ko)   
      korder(ko)=mend   
      kolum(mend)=mta   
      ko=mend   
      lp=mend   
      mend=mend+1   
      rowh(lp)=0
      go to 270 

  260 ko=lp 
  270 rh=e(k1)*e(k2)*tie(6,iax) 
      if (ix.lt.0) rh=-rh   
      if (ka1.ne.jt) rh = -rh   
      rowh(lp)=rowh(lp)+rh  
  280 continue  
      go to 330 
C       
C     Add bus tie constraints   
C       
  290 ix = ikk(5,kta)   
      if (ix.le.0) call erexit  
      k1 = dmin1 (txtie(1,ix),txtie(2,ix)) 
      k2 = dmax1 (txtie(1,ix),txtie(2,ix)) 
      lp = 0
      if (k1.le.nbslck) go to 310   
      if (msw.eq.0) go to 300   
      if (ikk(1,k1+ntota).eq.1) go to 310   
  300 lp=lp+1   
      ko=lp 
      kolum(lp) = k1 + ntota
      max = kolum(lp)   
      korder(lp) = lp + 1   
      if (k1 .eq. txtie(1,ix)) then
         rowh(lp) = 1.0 
      else  
         rowh(lp) = -1.0
         if (msw .ne .0) rowh(lp) = -txtie(3,ix)
      endif 
      if (k1 .eq. k2) go to 320 
  310 k1 = k2   
      if (msw.eq.0) go to 300   
      if (ikk(1,k1+ntota).eq.2) go to 300   
        
  320 if (msw .eq. 0) then  
        
         lp = lp + 1
         ko = lp
         kolum(lp) = kta
         max = kolum(lp)
         korder(lp) = lp + 1
        
         k1 = txtie(1,ix)  
         k2 = txtie(2,ix)  
        
         cx = e(k1) * e(k2) * txtie(10,ix)  
         rowh(lp) = 1.0 / cx
        
      else  
        
         if (lp .eq. 0) then
C       
C           A bus tie spans two PV nodes. Change into pseudo-constraint.
C       
            lp = 1  
            ko = lp 
            kolum(lp) = kta 
            max = kolum(lp) 
            rowh(lp) = 1.0  
         endif  
        
      endif 
      mend = lp + 1 
      korder(0) = 1 
      korder(lp) = 0
  330 continue  
      if (kolum(lp) .ne. max) then  
         lp = 0 
  340    ko = lp
         lp = korder(lp)
         if (lp .eq. 0) call erexit 
         if (kolum(lp) .lt. max) go to 340  
      endif 
      return
      end   
