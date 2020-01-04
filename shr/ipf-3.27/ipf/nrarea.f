C    @(#)nrarea.f	20.3 2/13/96
      subroutine nrarea (jt, export, error) 
C              computes the area interchage export and export error.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc.inc'
 
      double precision ek, fk, em, fm, ikr, iki, gkm, bkm
 
      export = 0.0
      if (jt .eq. 0) call erexit
      j1 = karea(3,jt)  
      js = karea(4,jt) + j1 - 1 
      do 1300 j = j1, js
         ix = kaloc(j)  
         iax = iabs(ix) 
         k1 = tie(1,iax)   
         k2 = tie(7,iax)   
         mt = k1
         if (ix .lt. 0) mt = k2 
         ka1 = tie(2,iax)  
         ka2 = tie(8,iax)  
         kdc = tie(9,iax)  
         if (kdc .eq. 0) go to 1100 
         if (ix .lt. 0) go to 1300  
         kd = kdc   
 1000    k1x = dmin1 (dcline(1,kd),dcline(2,kd))  
         k2x = dmax1 (dcline(1,kd),dcline(2,kd))  
         if (k1x .ne. min0(k1,k2)) then 
            if (kd .ne. kdc) call erexit
            if (mtdcln .eq. 0) call erexit  
            kd = kdc + mtdcln   
            go to 1000  
         else if (k2x .ne. max0(k1,k2)) then
            call erexit 
         endif  
         if (k1 .eq. dcline(1,kd)) then 
            l1 = dcline(8,kd)   
            l2 = dcline(9,kd)   
         else   
            l1 = dcline(9,kd)   
            l2 = dcline(8,kd)   
         endif  
         v1 = dcbus(20,l1)  
         v2 = dcbus(20,l2)  
         pin = v1 * (v1 - v2) / (dcline(4,kd)*bmva) 
         if (jt .ne. ka1) pin = -pin
         export = export + pin  
         go to 1300 
 1100    ek = e(k1) 
         fk = f(k1) 
         em = e(k2) 
         fm = f(k2) 
         vksq = ek * ek + fk * fk   
         gkm = tie(5,iax)   
         bkm = tie(6,iax)   
         ikr = gkm * em - bkm * fm  
         iki = bkm * em + gkm * fm  
         pin = ek * ikr + fk * iki + vksq * tie(3,iax)  
         if (ka1 .ne. jt) then  
            pin = -pin  
         endif  
         if (ix .gt. 0) export = export + pin   
 1300 continue  
      continue  
      error = area(2,jt) - export   
      return
      end   
