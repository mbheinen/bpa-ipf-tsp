C    @(#)obcddat.f	20.4 2/13/96
      subroutine obcddat (ind, xbuf)
      implicit none
      integer ind
      character xbuf * (*)                                              
c $$$
c $$$ Caution.  IPF conversion has revealed that RM and RN records
c $$$ are transposed differently.  The Pmin,Pmax and Qmin,Qmax 
c $$$ quantities were inconsistently transposed in the batch PF.
c $$$
c ***                                                                  *
c *** This subroutine encodes bus,branch,+bus,or xdata information in  *
c *** xbuf                                                             *
c ***                                                                  *
c ***   input data:                                                    *
c ***           common tables for bus,branch,+bus,xdata                *
c ***           pointer index to the tables 'ind'                      *
c ***                                                                  *
c ***   output data: xbuf*(*)                                          *
c ***                                                                  *
 
      include 'ipfinc/parametr.inc'

c***      include 'ipfinc:blank.inc'
c***       Global variables used:
c***               None
      include 'ipfinc/alt_case.inc'
c	Global variables used:
c		oldbus, okbrnch, obrid, okxdata, okbctbl, oldzone,
c		oldowner, okbsdta, oxdata, obctbl, obrnch, orateln, 
c		obrsect, okx, oky, obrnch_ptr, obusdta
      include 'ipfinc/com007.inc'
c	Global variables used:
c		bustyp, month
c***      include 'ipfinc:prt.inc'
c***       Global variables used:
c***               None

      common /obcdflg/ obldtbx, olowx, oxlinmn, onpctq, opctq(MAXBUS)
      logical obldtbx, olowx
      integer onpctq
      real oxlinmn, opctq

      character basbcd * 4, pl * 5, ql * 5, gmw * 5, gm * 4, bm * 4,    
     &    vmin * 4, vmax * 4, pct * 3 ,gpmx * 5, qmin * 5,        
     &    qmax * 5, bascnt * 4, br * 2, sr * 5, amin * 5,         
     &    amax * 5,vlv * 5, rate5 * 5, 
     &    bs1bcd * 4, bs2bcd * 4, tmax * 5, tmin * 5, vamax * 5,  
     &    vamin * 5, cr * 6, cx * 6, cg * 6, cb6 * 6, amiles * 4, 
     &    ctap1 * 5, ctap2 * 5, cb1 * 6, cg1 * 6, cb2 * 6,        
     &    cg2 * 6, cc * 6, ca * 4, cb * 4, type * 1,      
     &    buscnt * 8, gnom * 3, gmin * 3, suscp(8) * 5, id * 1,      
     &    cl * 6, ir * 1, pdc * 6, vdc * 5, arnet * 8, kyrc * 2,  
     &    secc * 1, nocktc * 1, step(8) * 1, intovc * 1, rvtype * 1, 
     &    pmin * 5, pmax * 5, rate4 * 4, xijmin * 6, xijmax * 6,  
     &    bismin * 6, bismax * 6, rate(3) * 4, yy(8) * 10,
     &    value(10) * 6, active * 1, pu * 2

      integer findex
      character code * 10

      integer bptr1
      integer nb, ncb
      integer ktyp, k1, k2, kt, kyr, mo, i, j, jbrx, jbr, ntybr,
     &        kb, ntap, jxd
      real sect, year, overrd, xpdc

      save                                                              
                                                                        
c ***                                                                  *
c *** ******************************************************************
c ***                                                                  *
c *** encode bus data                                                  *
c ***                                                                  *
      entry obcdbus (nb,xbuf)                                             
      basbcd = code (oldbase(nb),4,0)                                       
      ktyp = okbsdta(1,nb)                                               
      type = bustyp(ktyp)                                               
      if (ktyp .ne. 5 .and. ktyp .ne. 12) then                          
         buscnt = '       '                                             
         bascnt = '    '                                                
         pct = '   '                                                    
         if (ktyp .eq. 8 .or. ktyp .eq. 11) then                        
            k1 = okbsdta(13,nb)                                          
            if (k1 .gt. 0) then                                         
               buscnt = oldbus(k1)                                         
               bascnt = code (oldbase(k1), 4,0)                              
            else                                                        
               buscnt = oldbus(nb)                                         
               bascnt = basbcd                                          
            endif                                                       
            if (ktyp .ne. 11) then                                      
               pct = code (obusdta(14,nb),3,0)                        
            endif                                                       
         endif                                                          
                                                                        
         vmax = code(obusdta(11,nb),4,3)                                 
         vmin = code(obusdta(12,nb),4,3)                                 
         pl = code(obusdta(3,nb),5,0)                                    
         ql = code(obusdta(4,nb),5,0)                                    
         gm = code(obusdta(5,nb),4,0)                                    
         bm = code(obusdta(6,nb),4,0)                                    
         gpmx = code(obusdta(7,nb),4,0)                                  
         gmw = code(obusdta(8,nb),5,0)                                   
         qmax = code(obusdta(9,nb),5,0)                                  
         qmin = code(obusdta(10,nb),5,0)                                 
         if (ktyp .eq. 3) vmin = code(obusdta(12,nb),4,1)                
         write (xbuf,120) type, oldowner(nb), oldbus(nb), basbcd, 
     &      oldzone(nb),pl, ql, gm, bm, gpmx, gmw, qmax, qmin, vmax, 
     &      vmin, buscnt, bascnt, pct                                         
  120    format ('B', a1, 1x, a3, a8, a4, a2, 2a5, 3a4, 3a5, 2a4, a8,   
     1           a4, a3)                                                     
c ***                                                                  *
      else                                                              
c ***                                                                  *
         k1 = okbsdta(9,nb)                                              
         br = code(obusdta(3,nb),2,0)                                    
         sr = code(obusdta(4,nb),5,1)                                    
         amin = code(obusdta(5,nb),5,1)                                  
         amax = code(obusdta(6,nb),5,1)                                  
         vlv = code(obusdta(7,nb),5,1)                                   
         rate5 = code(obusdta(8,nb),5,1)                                 
         if (ktyp .eq. 5) then                                          
            if (k1 .gt. 0) then                                         
               buscnt = oldbus(k1)                                         
               bascnt = code (oldbase(k1),4,0)                              
            else                                                        
               buscnt = ' '                                      
               bascnt = ' '                                          
            endif
            write (xbuf,130) oldowner(nb), oldbus(nb), basbcd, 
     &                       oldzone(nb), br,  sr, amin, amax, vlv, 
     &                       rate5, buscnt, bascnt
  130       format ('BD', 1x, a3, a8, a4, a2, 3x, a2, 5a5, a8, a4)      
         else                                                           
            if (k1 .le. 0) then                                         
               br = ' '                                                
               sr = ' '                                             
               amin = ' '                                            
               amax = ' '                                            
               vlv = ' '                                            
               gnom = ' '                                             
               gmin = ' '                                             
               rate5 = ' '                                          
               pdc = ' '                                           
               vdc = ' '                                            
               buscnt = ' '                                             
               bascnt = ' '                                          
            else                                                        
               gnom = code(obusdta(10,nb),3,1)                           
               gmin = code(obusdta(11,nb),3,1)                           
               pdc = code(obusdta(13,nb),6,1)                            
               vdc = code(obusdta(14,nb),5,1)                            
               buscnt = oldbus(k1)                                         
               bascnt = code (oldbase(k1),4,0)                              
            endif                                                       
c ***                                                                  *
            write (xbuf,140) oldowner(nb), oldbus(nb), basbcd, 
     &         oldzone(nb) ,br, sr, amin, amax, vlv, rate5, buscnt, 
     &         bascnt, obusdta(12,nb) ,gnom, gmin, pdc, vdc                      
  140       format ('BM ', a3, a8, a4, a2, 3x, a2, 5a5, a8, a4, a1, 2a3 
     1               , a6, a5)                                                
c ***                                                                  *
         endif                                                          
      endif                                                             
      return                                                            
c ***                                                                  *
c *** ******************************************************************
c ***                                                                  *
c *** encode "+" bus data                                              *
c ***                                                                  *
      entry obcdcbs (ncb,xbuf)                                            
      kt = okbctbl(1,ncb)                                                
      kyr = okbctbl(7,ncb)                                               
      mo = mod(kyr,100)                                                 
      year = kyr / 100                                                  
      kyrc = code (year,2,0)                                            
      pl = code(obctbl(2,ncb),5,0)                                       
      ql = code(obctbl(3,ncb),5,0)                                       
      gm = code(obctbl(4,ncb),4,0)                                       
      bm = code(obctbl(5,ncb),4,0)                                       
      gmw = code(obctbl(6,ncb),5,0)                                      
      qmax = code(obctbl(11,ncb),5,0)                                    
      qmin = code(obctbl(12,ncb),5,0)                                    
      basbcd = code (oldbase(kt),4,0)                                       
      write (xbuf,150) obctbl(8,ncb), obctbl(10,ncb), oldbus(kt), 
     &   basbcd, obctbl(9,ncb), pl, ql, gm, bm, gmw, qmax, qmin, 
     &   month(mo+1), kyrc     
  150 format ('+', a1, 1x, a3, a8, a4, a2, 2a5, 2a4, 4x, a5, a5, a5, 
     1        17x, a1, a2)                                                        
      return                                                            
c ***                                                                  *
c *** ******************************************************************
c ***                                                                  *
c *** encode "X" bus data                                              *
c ***                                                                  *
      entry obcdxdt (jxd,xbuf)                                            
      k1 = okxdata(1,jxd)                                                
      basbcd = code (oldbase(k1),4,0)                                       
      k2 = okxdata(2,jxd)                                                
      if (k2 .gt. 0) then                                               
         buscnt = oldbus(k2)                                               
         bascnt = code (oldbase(k2),4,0)                                    
      else                                                              
         buscnt = oldbus(k1)                                               
         bascnt = basbcd                                                
      endif                                                             
      j = 0                                                             
      do 160 i = 7, 21, 2                                               
         j = j + 1                                                      
         step(j) = code (oxdata(i,jxd),1,0)                              
         suscp(j) = code (oxdata(i+1,jxd),5,0)                           
  160 continue                                                          
      write (xbuf,170) oldbus(k1), basbcd, buscnt, bascnt, (step(i),       
     1   suscp(i),i=1,8)                                                
  170 format ('X', 5x, a8, a4, 2x, a8, a4, 8(a1, a5))                   
      return                                                            
c ***                                                                  *
c *** ******************************************************************
c ***                                                                  *
c *** encode branch data                                               *
c ***                                                                  *
      entry obcdbrn (bptr1,xbuf)                                           
                                                                        
      jbrx = obrnch_ptr(bptr1)
      jbr = iabs(jbrx)
      k1 = okx(bptr1)
      k2 = oky(bptr1)
      ntybr = obrtype(bptr1)
      id = obrid(bptr1)
      sect = obrsect(bptr1)

      if (ntybr .eq. 2 .or. ntybr .eq. 7) then                          
         secc = ' '                                                     
         nocktc = ' '                                                   
      else                                                              
         secc = code (sect,1,0)                                         
         nocktc = code (obrnch(16,jbr),1,0)                              
      endif                                                             
      if (ntybr .eq. 1) then                                            
         mo = 0                                                         
         kyrc = ' '                                                     
      else                                                              
         mo = mod(okbrnch(11,jbr),100)                                   
         year = okbrnch(11,jbr) / 100                                    
         kyrc = code (year,2,0)                                         
      endif                                                             
      overrd = okbrnch(15,jbr)                                           
      if (jbrx .lt. 0) then
         if (overrd. gt. 0.0) overrd = 3.0 - overrd
      endif
      intovc = code (overrd,1,0)                                        
      bs1bcd = code (oldbase(k1),4,0)                                       
      bs2bcd = code (oldbase(k2),4,0)                                       
      if (ntybr .eq. 4 .or. ntybr .eq. 9) then                          
         rate4 = ' '                                                    
      else                                                              
         rate4 = code(obrnch(4,jbr),4,0)                                 
      endif                                                             
      do 200 i = 1, 3                                                
         if (orateln(i,jbr) .eq. 0.0) then                            
            rate(i) = ' '                                            
         else                                                        
            rate(i) = code (orateln(i,jbr),4,0)                       
         endif                                                       
  200 continue                                                       
c ***                                                                  *
      if ( ntybr .eq. 3 ) then                                          
c ***                                                                  *
c ***    encode "L" branch                                             *
c ***                                                                  *
         cr = code(obrnch(5,jbr),6,5)                                    
         if (olowx) then                                                 
            if (abs(obrnch(6,jbr)) .lt. oxlinmn) then                     
               cx = code(sign (oxlinmn, obrnch(6,jbr)), 6,5)              
            else                                                        
               cx = code(obrnch(6,jbr),6,5)                              
            endif                                                       
         else                                                           
            cx = code(obrnch(6,jbr),6,5)                                 
         endif                                                          
         cg = code(obrnch(7,jbr),6,5)                                    
         cb6 = code(obrnch(8,jbr),6,5)                                   
         amiles = code(obrnch(9,jbr),4,1)                                
         write (xbuf,210) obrnch(3,jbr), oldbus(k1), bs1bcd, intovc,        
     1      oldbus(k2), bs2bcd, id, secc, rate4, nocktc, cr, cx, cg,       
     2      cb6, amiles, month(mo+1), kyrc, rate(1), rate(2)            
  210    format ('L', 2x, a3, a8, a4, a1, a8, a4, a1, a1, a4, a1, 4a6,  
     1      a4, 8x, a1, a2, t81, 2a4)                                   
      else if ( ntybr .eq. 5 .or. ntybr .eq. 6 ) then                   
c ***                                                                  *
c ***    encode "T" data                                               *
c ***                                                                  *
         cr = code(obrnch(5,jbr),6,5)                                    
         if (olowx) then                                                 
            if (abs(obrnch(6,jbr)) .lt. oxlinmn) then                     
               cx = code(sign (oxlinmn, obrnch(6,jbr)), 6,5)              
            else                                                        
               cx = code(obrnch(6,jbr),6,5)                              
            endif                                                       
         else                                                           
            cx = code(obrnch(6,jbr),6,5)                                 
         endif                                                          
         cg = code(obrnch(7,jbr),6,5)                                    
         cb6 = code(obrnch(8,jbr),6,5)                                   
         if (jbrx .gt. 0) then
            ctap1 = code(obrnch(9,jbr),5,2)                                 
            ctap2 = code(obrnch(10,jbr),5,2)                                
         else if (ntybr .eq. 5) then
            ctap2 = code(obrnch(9,jbr),5,2)                                 
            ctap1 = code(obrnch(10,jbr),5,2)                                
         else
            ctap1 = code(-obrnch(9,jbr),5,2)                                 
            ctap2 = code(obrnch(10,jbr),5,2)                                
         endif
         type = ' '                                                     
         if (ntybr .eq. 6) type = 'P'                                   
         write (xbuf,220) type, obrnch(3,jbr), oldbus(k1), bs1bcd,
     &      intovc, oldbus(k2), bs2bcd, id, secc, rate4, nocktc, cr, 
     &      cx ,cg, cb6, ctap1, ctap2, month(mo+1), kyrc, rate(1), 
     &      rate(2), rate(3)                                                     
  220    format ('T', a1, 1x, a3, a8, a4, a1, a8, a4, a1, a1, a4, a1,   
     1      4a6, 2a5, 2x, a1, a2, t81, 3a4)                             
c ***                                                                  *
      else if ( ntybr .eq. 8 ) then                                     
c ***                                                                  *
c ***    encode "E"                                                    *
c ***                                                                  *
         cr = code(obrnch(5,jbr),6,5)                                    
         if (olowx) then                                                 
            if (abs(obrnch(6,jbr)) .lt. oxlinmn) then                     
               cx = code(sign (oxlinmn, obrnch(6,jbr)), 6,5)              
            else                                                        
               cx = code(obrnch(6,jbr),6,5)                              
            endif                                                       
         else                                                           
            cx = code(obrnch(6,jbr),6,5)                                 
         endif                                                          
         if (jbrx .gt. 0) then
            cg1 = code(obrnch(7,jbr),6,5)                                   
            cb1 = code(obrnch(8,jbr),6,5)                                   
            cg2 = code(obrnch(9,jbr),6,5)                                   
            cb2 = code(obrnch(10,jbr),6,5)                                  
         else
            cg2 = code(obrnch(7,jbr),6,5)                                   
            cb2 = code(obrnch(8,jbr),6,5)                                   
            cg1 = code(obrnch(9,jbr),6,5)                                   
            cb1 = code(obrnch(10,jbr),6,5)                                  
         endif
         write (xbuf,230) obrnch(3,jbr), oldbus(k1), bs1bcd, intovc,        
     1      oldbus(k2), bs2bcd, id, secc, rate4, nocktc, cr, cx, cg1,      
     2      cb1, cg2, cb2, month(mo+1), kyrc, rate(1), rate(2)          
  230    format ('E', 2x, a3, a8, a4, a1, a8, a4, a1, a1, a4, a1, 6a6,  
     1           a1, a2, t81, 2a4)                                           
c ***                                                                  *
      else if ( ntybr .eq. 2 .or. ntybr .eq. 7 ) then                   
c ***                                                                  *
c ***    encode "LD" and "LM" data                                     *
c ***                                                                  *
         mo = 0                                                         
         rate4 = code(obrnch(4,jbr),4,0)                                 
         amiles = code(obrnch(16,jbr),4,0)                               
         cr = code(obrnch(5,jbr),6,2)                                    
         cl = code(obrnch(6,jbr),6,2)                                    
         cc = code(obrnch(7,jbr),6,2)                                    
         if (ntybr .eq. 2) then                                         
            write (xbuf,240) obrnch(3,jbr), oldbus(k1), bs1bcd, intovc,
     1         oldbus(k2), bs2bcd, rate4, cr, cl, cc, amiles,              
     2         month(mo+1), kyrc                                        
  240       format ('LM', 1x, a3, a8, a4, a1, a8, a4, 2x, a4, 3a6, 15x, 
     1              a4, a1, a2)                                             
         else                                                           
            vdc = code(obrnch(9,jbr),5,1)                                
            ca = code(obrnch(10,jbr),4,1)                                
            cb = code(obrnch(18,jbr),4,1)                                
            ir = 'I'                                                    
            if (obrsect(bptr1) .eq. 1) ir = 'R'                         
            xpdc = obrnch(8,jbr)
            if (jbrx .lt. 0) then
               if (abs(xpdc) .lt. 1000.0) then                               
                  xpdc = -xpdc                                             
               else if (ir .eq. 'I') then
                  ir = 'J'                                              
               else if (ir. eq. 'R') then                                   
                  ir = 'S'                                              
               endif                                                     
            endif
            if (abs(xpdc) .lt. 1000.0) then                     
               pdc = code (xpdc,5,1)                            
            else                                                        
               pdc = code (abs(xpdc),5,1)                       
               if (ir .eq. 'I' .and. xpdc .lt. 0.0) ir = 'J'    
               if (ir .eq. 'R' .and. xpdc .lt. 0.0) ir = 'S'    
            endif                                                       
            write (xbuf,250) obrnch(3,jbr), oldbus(k1), bs1bcd, 
     &         oldbus(k2), bs2bcd, rate4 ,cr, cl, cc, ir, pdc, vdc, ca, 
     &         cb, amiles, kyrc                                             
  250       format ('LD', 1x, a3, a8, a4, 1x, a8, a4, 2x, a4, 3a6, a1,  
     1              2a5, 2a4, a4, a2)                                        
         endif                                                          
c ***                                                                  *
      else if ( ntybr .eq. 4 ) then                                     
c ***                                                                  *
c ***    encode "R" data                                                     *
c ***                                                                  *
         write (type,260) obrnch(3,jbr)                                  
  260    format (a1)                                                    
         kb = okbrnch(4,jbr)                                             
                                                                        
         if ( kb .gt. 0 ) then                                          
            buscnt = oldbus( kb )                                          
            bascnt = code ( oldbase(kb), 4, 0 )                             
         else                                                           
            buscnt = oldbus( k1 )                                          
            bascnt = code ( oldbase(k1), 4, 0 )                             
         endif                                                          
c ***                                                                  *
         if (jbrx .gt. 0) then
            tmax = code(obrnch(6,jbr),5,2)                                  
            tmin = code(obrnch(7,jbr),5,2)                                  
            vamax = code(obrnch(9,jbr),5,0)                                 
            vamin = code(obrnch(10,jbr),5,0)                                
         else
            if (type .eq. 'M' .or. type .eq. 'P') then
               tmin = code(-obrnch(6,jbr),5,2)                                  
               tmax = code(-obrnch(7,jbr),5,2)                                  
            else
               tmax = code(obrnch(6,jbr),5,2)                                  
               tmin = code(obrnch(7,jbr),5,2)                                  
            endif
            if (type .eq. 'P' .or. type .eq. 'Q') then                                      
               vamax = code(-obrnch(9,jbr),5,0)                                 
               vamin = code(obrnch(10,jbr),5,0)                                
            else if (type .eq. 'M' .or. type .eq. 'N') then
               vamax = code(-obrnch(10,jbr),5,0)                                 
               vamin = code(-obrnch(9,jbr),5,0)                                
            else
               vamax = code(obrnch(9,jbr),5,0)                                 
               vamin = code(obrnch(10,jbr),5,0)                                
            endif
         endif
         ntap = obrnch(8,jbr)                                            
         write (xbuf,270) obrnch(3,jbr), oldbus(k1), bs1bcd, oldbus(k2),
     1      bs2bcd, buscnt, bascnt, tmax, tmin, ntap, vamax, vamin      
  270    format ('R', a1, 4x, a8, a4, 1x, a8, a4, 2x, a8, a4, 2a5, i2,  
     1            2a5)                                                        
      else if ( ntybr .eq. 9 ) then                                     
c ***                                                                  *
c ***    encode "RZ"                                                   *
c ***                                                                  *
         rvtype = code(obrnch(4,jbr),1,0)                                
         if (jbrx .gt. 0) then
            pmax = code(obrnch(5,jbr),5,0)                                  
            pmin = code(obrnch(6,jbr),5,0)                                  
         else
            pmin = code(-obrnch(5,jbr),5,0)                                  
            pmax = code(-obrnch(6,jbr),5,0)                                  
         endif
         rate4 = code(obrnch(7,jbr),4,0)                                 
         xijmax = code(obrnch(8,jbr),6,5)                                
         xijmin = code(obrnch(9,jbr),6,5)                                
         bismax = code(obrnch(10,jbr),6,5)                               
         bismin = code(obrnch(18,jbr),6,5)                               
         write (xbuf,280) oldbus(k1), bs1bcd, oldbus(k2), bs2bcd, id, 
     &      secc, rvtype, pmax, pmin, rate4, xijmax, xijmin, bismax, 
     &      bismin   
  280    format ('RZ', 4x, a8, a4, 1x, a8, a4, a1, a1, a1, 2a5, 
     1           a4, 4a6 )                                                           
c ***                                                                  *
c ***    Encode equivalent - pi                                        *
c ***                                                                  *
      else if (ntybr .eq. 1 ) then                                      
         if (len (xbuf) .lt. 120) then
            write (xbuf,290) oldbus(k1), bs1bcd, oldbus(k2), bs2bcd, id,
     &         secc    
  290       format ('L*', 4x, a8, a4, 1x, a8, a4, a1, a1,                  
     1           ' *** equivalent pi ***')                                   
         else
            do i = 1, 8
               yy(i) = code (obrnch(i+3,jbr), 10, 5)
            enddo
            write (xbuf,300) oldbus(k1), bs1bcd, oldbus(k2), bs2bcd, id,
     &         secc, yy
  300       format ('L*', 4x, a8, a4, 1x, a8, a4, a1, a1, t39, 8a10)
         endif
      endif                                                             
c ***                                                                  *
      return                                                            
      end                                                               
