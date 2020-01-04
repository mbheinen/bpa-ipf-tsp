C    @(#)gtlnload.f	20.3 2/13/96
C****************************************************************
C
C       File: gtlnload.for
C
C       Purpose: Routine to compute the line loading (in amps for
c                lines and MVA for transformers) and obtain the 
c                extended ratings.
C
C                Special processing is required for any line section: 
C                First, compute the flow using pi-equivalent branch; 
C                next compute the flows in individual sections, using 
C                the intermediate nodal voltages.
C                                                                       
C       Input parameters:  
C
C             PTR    - kx(), ky() index.
C
C       Output parameters:
C      
C             AIN    - line input current in amps or transformer
c                      complex power in MVA
C             LOAD   - percent loading of minimum rating
C             RATTHR - thermal rating (amps)
C             RATBTL - bottle-neck rating (amps)
C             RATLLF - loss-of-life rating (amps) ** Not used for lines
C             RATNOM - nominal rating (amps)
C
C       Author: Walt Powell  Date: 12 November 1992
C       Called by: ovldlnsrpt, ovldtxsrpt
C
C****************************************************************
C
      subroutine gtlnload (ptr, ain, lnload, ratthr, ratbtl, 
     1                     ratllf, ratnom)
      integer ptr
      real lnload
                                                                       
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva, kdtot, mtdcln
      include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8), kbsdta, inp2opt, base
      include 'ipfinc/branch.inc'
c	Global variables used:
c		kx, ky, brtype, brsect, brid, brnch_nxt
      include 'ipfinc/ordsta.inc'
c	Global variables used:
c		ordvlt
      include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dc2t(r*8)
      include 'ipfinc/dcmt.inc'
c	Global variables used:
c		dcmtbs(r*8), dcmtln(r*8)
                                                                             
      complex * 16 v(2), y(2,2), a(2), s(2)                              
      character rattag * 1
      integer p
      logical found

      save v, y, a, s, base1, base2
C                                                                       
C     Local Variables                                                   
                                                                        
      k1 = kx(ptr)
      k2 = ky(ptr)
      ltype = brtype(ptr)
C
C     ISECT = 0 : Line without sections
C             1 : First line section being processed
C             2 : subsequent line sections being processed
C
c
c     Define "base1" and "base2" appropriately with respect to
c     section brsect(ptr)
c
      base1 = base(k1)
      base2 = base(k2)
      if (ltype .eq. 2 .or. ltype .eq. 7 .or. ltype .eq. 4) then
         isect = 0
      else if (brsect(ptr) .eq. 0) then
         isect = 0
      else
         base1 = base(k1)
         base2 = base(k1)    
         p = kbsdta(16,k1)
         isect = 2
         found = .false.
         do while (p .gt. 0)
            if (ky(p) .eq. k2 .and. brid(p) .eq. brid(ptr)) then
               found = .true.
               if (brtype(p) .eq. 1) then
                  isect = 1
               else 
                  if (brtype(p) .eq. 5 .or. brtype(p) .eq. 6) then
                     base1 = base(k2)    ! Set for next section
                     base2 = base(k2)
                  endif
                  if (brsect(p) .eq. brsect(ptr)) p = 0
               endif
            endif
            if (p .gt. 0) p = brnch_nxt(p)
         enddo
      endif
                                                                                
      if (ordvlt .eq. 1) then
         kt = k1
         mt = k2
      else
         kt = inp2opt(k1)                                                 
         mt = inp2opt(k2)                                                 
      endif
      lnload = 0.0
C 
C     Get Nominal/Extended ratings
C 
      call getrat (ptr, rating, rattag, ratnom, ratthr, ratllf, ratbtl)
      if (ltype .eq. 2) then                                         
C                                                                       
C        Compute multi-terminal d-c line flows                       
C                                                                     
         do 100 i = 1, mtdcln                                        
            if (k1 .eq. dcmtln(1,i) .and. k2 .eq. dcmtln(2,i)) then  
               l1 = dcmtln(8,i)                                      
               l2 = dcmtln(9,i)                                      
               idc = i                                               
               go to 102                                             
            else if (k1 .eq. dcmtln(2,i) .and. k2 .eq. dcmtln(1,i))  
     1         then                                                  
               l1 = dcmtln(9,i)                                      
               l2 = dcmtln(8,i)                                      
               idc = i                                               
               go to 102                                             
            endif                                                    
  100    continue                                                    
         call erexit (0)                                             
  102    vt1 = dcmtbs(20,l1)                                         
         vt2 = dcmtbs(20,l2)                                         
         ain = (vt1 - vt2) / dcmtln(4,idc)                           
         ain = 1000.0 * abs (ain)
         if (rating .gt. 0.0) then
            lnload = 100.0 * ain / rating
         endif
                                                                        
      else if (ltype .eq. 7) then                                    
C                                                                       
C        Compute two-terminal d-c line flows                         
C                                                                       
         do 110 i = 1, kdtot                                         
            if (k1 .eq. dc2t(1,i) .and. k2 .eq. dc2t(3,i)) then    
               vt1 = dc2t(40,i)                                      
               ain = 0.001 * dc2t(39,i)                              
               vt2 = dc2t(41,i)                                      
            else if (k1 .eq. dc2t(3,i) .and. k2 .eq. dc2t(1,i))    
     1         then                                                  
               vt1 = dc2t(41,i)                                      
               vt2 = dc2t(40,i)                                      
               ain = -0.001 * dc2t(39,i)                               
            endif                                                    
  110    continue                                                    
         pin = vt1 * ain                                             
         pout = -vt2 * ain                                           
         ain = 1000.0 * abs (ain)                                          
         if (rating .gt. 0.0) then
            lnload = 100.0 * ain / rating
         endif
                                                                        
      else                                                           
C                                                                       
C        Compute a-c line flows                                      
C                                                                       
         call pieqiv(ptr, y, kerr)                                   
         if (isect .eq. 0) then
            v(1) = dcmplx(e(kt),f(kt))                                     
            v(2) = dcmplx(e(mt),f(mt))                                     
            do i=1,2                                                
               a(i)=dcmplx(0.0d0,0.0d0)                                      
               do j=1,2                                             
                 a(i) = a(i) + y(i,j) * v(j)                                    
               enddo
               s(i) = v(i) * dconjg(a(i))
            enddo                                       
         else if (isect .eq. 1) then
            v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)
            a(2) = y(2,1) * v(1) + y(2,2) * v(2)
            s(2) = v(2) * dconjg(a(2))
         else
            v(1) = v(2)
            a(1) = -a(2)
            s(1) = -s(2)
            v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)
            a(2) = y(2,1) * v(1) + y(2,2) * v(2)
            s(2) = v(2) * dconjg(a(2))
         endif
         if (ltype .eq. 5 .or. ltype .eq. 6) then
            a1 = cdabs(s(1))*bmva
            a2 = cdabs(s(2))*bmva
            ain = amax1(a1, a2)
            if (rating .gt. 0.0) then
               lnload = 100.0 * ain / rating
            endif
         else if (ltype .ne. 1) then
            a1 = 577.3502692 * bmva * cdabs(a(1)) / base1
            a2 = 577.3502692 * bmva * cdabs(a(2)) / base2
            ain = amax1 (a1, a2)
            if (rating .gt. 0.0) then
               lnload = 100.0 * ain / rating
            endif
         endif

      endif                                                          
                                                                        
      return                                                            
      end                                                               
