C    @(#)kpxsrt.f	20.6 5/27/99
      function kpxsrt (m,n)

C     This function performs bus sort in the branch overload auxiliary 
C     index array.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/bus.inc'
 
      character id1 * 1, id2 * 1
 
      if (m .eq. n) then
         kpxsrt = 0
      else
         i = xsort(m)
         j = xsort(n)
         ix = ibrolp(1,i)
         jx = ibrolp(1,j)
 
C ***    KLNC(1,*) = K1                   
C ***    KLNC(2,*) = K2                   
C ***    KLNC(3,*) = OWNER                
C ***    KLNC(4,*) = 1000 + ID            
C ***                                     
         if (ix .le. nout .and. jx .le. nout) then
            k1 = klnc(1,ix)
            k2 = klnc(1,jx)
            kpxsrt = opt2inp(k1) - opt2inp(k2)
            if (kpxsrt .eq. 0) then
               m1 = klnc(2,ix)
               m2 = klnc(2,jx)
               kpxsrt = opt2inp(m1) - opt2inp(m2)
            endif
            if (kpxsrt .eq. 0) then
               jd = mod(klnc(4,ix),1000)
               call getchr(1,id1,jd)
               jd = mod(klnc(4,jx),1000)
               call getchr(1,id2,jd)
               kpxsrt = kompr (id1,id2,junk)
            endif
         else
            kpxsrt = ix - jx
         endif
      endif
      return
      end
