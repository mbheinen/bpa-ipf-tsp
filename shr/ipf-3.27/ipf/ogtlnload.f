C    @(#)ogtlnload.f	20.3 2/13/96
C****************************************************************
C
C       File: ogtlnload.for
C
C       Purpose: Routine to compute the line loading (in amps for
c                lines and MVA for transformers) and obtain the 
c                extended ratings FOR THE ALTERNATE BASE CASE IN 
C                RESIDENCE!
C
C                Special processing is required for any line section: 
C                First, compute the flow using pi-equivalent branch; 
C                next compute the flows in individual sections, using 
C                the intermediate nodal voltages.
C                                                                       
C       Input parameters:  
C
C             PTR    - okx(), oky() index.
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
C       Called by: lfodifrpt
C
C****************************************************************
C
      subroutine ogtlnload (ptr, ain, lnload, ratthr, ratbtl, 
     1                     ratllf, ratnom)
      integer ptr
      real lnload
                                                                       
      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
                                                                        
      complex v(2), y(2,2), a(2), s(2)                              
      character rattag * 1
      integer p
      logical found

      save v, y, a, s
C                                                                       
C     Local Variables                                                   
                                                                        
      k1 = okx(ptr)
      k2 = oky(ptr)
      ltype = obrtype(ptr)
C
C     ISECT = 0 : Line without sections
C             1 : First line section being processed
C             2 : subsequent line sections being processed
c
c     Define "base1" and "base2" appropriately with respect to
c     section brsect(ptr)
c
      base1 = oldbase(k1)
      base2 = oldbase(k2)
      if (ltype .eq. 2 .or. ltype .eq. 7 .or. ltype .eq. 4) then
         isect = 0
      else if (obrsect(ptr) .eq. 0) then
         isect = 0
      else
         base1 = oldbase(k1)
         base2 = oldbase(k1)    
         p = okbsdta(16,k1)
         isect = 2
         found = .false.
         do while (p .gt. 0)
            if (oky(p) .eq. k2 .and. obrid(p) .eq. obrid(ptr)) then
               found = .true.
               if (obrtype(p) .eq. 1) then
                  isect = 1
               else 
                  if (obrtype(p) .eq. 5 .or. obrtype(p) .eq. 6) then
                     base1 = oldbase(k2)  ! Set for next section
                     base2 = oldbase(k2)
                  endif
                  if (obrsect(p) .eq. obrsect(ptr)) p = 0
               endif
            endif
            if (p .gt. 0) p = obrnch_nxt(p)
         enddo
      endif
            
      kt = oinp2opt(k1)
      mt = oinp2opt(k2)
      lnload = 0.0
C 
C     Get Nominal/Extended ratings
C 
      call ogetrat (ptr, rating, rattag, ratnom, ratthr, ratllf, ratbtl)
      if (ltype .eq. 2) then                                         
C                                                                       
C        Compute multi-terminal d-c line flows                       
C                                                                     
         do 100 i = 1, omtdcln                                        
            if (k1 .eq. okdcmtl(1,i) .and. k2 .eq. okdcmtl(2,i)) then  
               l1 = okdcmtl(8,i)                                      
               l2 = okdcmtl(9,i)                                      
               idc = i                                               
               go to 102                                             
            else if (k1 .eq. okdcmtl(2,i) .and. k2 .eq. okdcmtl(1,i))  
     1         then                                                  
               l1 = okdcmtl(9,i)                                      
               l2 = okdcmtl(8,i)                                      
               idc = i                                               
               go to 102                                             
            endif                                                    
  100    continue                                                    
         call erexit (0)                                             
  102    vt1 = odcmtbs(20,l1)                                         
         vt2 = odcmtbs(20,l2)                                         
         ain = (vt1 - vt2) / odcmtln(4,idc)                           
         ain = 1000.0 * abs (ain)
         if (rating .gt. 0.0) then
            lnload = 100.0 * ain / rating
         endif
                                                                        
      else if (ltype .eq. 7) then                                    
C                                                                       
C        Compute two-terminal d-c line flows                         
C                                                                       
         do 110 i = 1, okdtot                                         
            if (k1 .eq. okdc2t(1,i) .and. k2 .eq. okdc2t(3,i)) then    
               vt1 = odc2t(40,i)                                      
               ain = 0.001 * odc2t(39,i)                              
               vt2 = odc2t(41,i)                                      
            else if (k1 .eq. okdc2t(3,i) .and. k2 .eq. okdc2t(1,i))    
     1         then                                                  
               vt1 = odc2t(41,i)                                      
               vt2 = odc2t(40,i)                                      
               ain = -0.001 * odc2t(39,i)                               
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
         call opieqiv(ptr, y, kerr)                                   
         if (isect .eq. 0) then
            v(1) = cmplx(olde(kt),oldf(kt))                                     
            v(2) = cmplx(olde(mt),oldf(mt))                                     
            do i=1,2                                                
               a(i)=cmplx(0.0,0.0)                                      
               do j=1,2                                             
                 a(i) = a(i) + y(i,j) * v(j)                                    
               enddo
               s(i) = v(i) * conjg(a(i))
            enddo                                       
         else if (isect .eq. 1) then
            v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)
            a(2) = y(2,1) * v(1) + y(2,2) * v(2)
            s(2) = v(2) * conjg(a(2))
         else
            v(1) = v(2)
            a(1) = -a(2)
            s(1) = -s(2)
            v(2) = (a(1) - y(1,1) * v(1)) / y(1,2)
            a(2) = y(2,1) * v(1) + y(2,2) * v(2)
            s(2) = v(2) * conjg(a(2))
         endif
         if (ltype .eq. 5 .or. ltype .eq. 6) then
            a1 = cabs(s(1))*bmva
            a2 = cabs(s(2))*bmva
            ain = amax1(a1, a2)
            if (rating .gt. 0.0) then
               lnload = 100.0 * ain / rating
            endif
         else if (ltype .ne. 1) then
            a1 = 577.3502692 * bmva * cabs(a(1)) / base1
            a2 = 577.3502692 * bmva * cabs(a(2)) / base2
            ain = amax1 (a1, a2)
            if (rating .gt. 0.0) then
               lnload = 100.0 * ain / rating
            endif
         endif

      endif                                                          
                                                                        
      return                                                            
      end                                                               
