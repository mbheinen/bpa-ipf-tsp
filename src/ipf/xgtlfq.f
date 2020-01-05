C    @(#)xgtlfq.f	20.4 9/10/96
C****************************************************************
C
C   File: xgtlfq.f
C   Purpose: Routine to compute line flow information using the
c            the alternate base case data.  Note that this is
c            a limited edition of the original gtlfq module.
c            The limitation is that the alternate data set excludes
c            some bus input data and some data accessing routines.
C
C   Author: Walt Powell  Date: 26 July 1993
C                        Modified: 
C   Called by:
C
C****************************************************************
C
      subroutine xgtlfq (ptr, pin, qin, ploss, qloss, ovld, ratec,
     &                  actual_amps, whichend1, actual_mva, whichend2)
      integer ptr, whichend1, whichend2
      character ratec*10
C                                                                       
C     This subroutine computes the line flow quantities. Special 
c     processing is required for any line section: First, compute
C     the flow using pi-equivalent branch; next compute the flows
C     in individual sections, using the intermediate nodal voltages.
C                                                                       
C     Input paramters:                                                  
C                                                                       
C     ptr    - branch index.                                            
C                                                                       
C     Output paramters:                                                 
C                                                                       
C     pin    - line input power in MW
C     qin    -  "                  MVAR
C     ploss  - line losses
C     qloss  -  "                                                       
C     ovld   - loading in percent
C     ratec  - rating (amps or MVA) and type (character)
C     actual_amps - loading (amps) 
C     whichend1   - "1" sending end, "2" receiving end
C     actual_mva  - loading (MVA) 
C     whichend2   - "1" sending end, "2" receiving end
                                                                       
      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
                                                                        
      complex v(2), y(2,2), a(2), s(2)                              
      character rattag * 1
      integer p, ptemp
      save v, y, a, s
C                                                                       
C     Local Variables                                                   
                                                                        
      k1 = okx(ptr)
      k2 = oky(ptr)
      ltype = obrtype(ptr)
      base1 = oldbase(k1)
      base2 = oldbase(k2)
C
C     isect = 0 : Line without sections
C             1 : First line section being processed
C             2 : subsequent line sections being processed
C
      if (ltype .eq. 2 .or. ltype .eq. 7 .or. ltype .eq. 4) then
         isect = 0
      else if (obrsect(ptr) .eq. 0) then
         isect = 0
      else
         p = okbsdta(16,k1)
         ptemp = 0
         isect = 1
         do while (p .gt. 0)
            if (oky(p) .eq. k2 .and. obrid(p) .eq. obrid(ptr)) then
               if ( ptemp .eq. 0 ) ptemp = p
               if (p .eq. ptr) then
                  p = 0
               else if (obrtype(p) .eq. 1) then
               else if (obrsect(p) .gt. 0) then
                  isect = 2
                  p = 0
               endif
            endif
            if (p .gt. 0) p = obrnch_nxt(p)
         enddo
c
c        Define "base1" and "base2" appropriately with respect to
c        section obrsect(ptr)
c
c***     p = okbsdta(16,k1)   ***** save some searching
         p = ptemp
         base2 = base1
         do while (p .gt. 0)
            if (oky(p) .eq. k2 .and. obrid(p) .eq. obrid(ptr)) then
               if (obrtype(p) .eq. 5 .or. obrtype(p) .eq. 6) then
                  base2 = oldbase(k2)
               elseif (obrtype(p) .ne. 1) then
                  base1 = base2
               endif
            endif
            if (p .eq. ptr) p = 0
            if (p .gt. 0) p = obrnch_nxt(p)
         enddo
      endif
                                                                        
      kt = oinp2opt(k1)                                                 
      mt = oinp2opt(k2)                                                 
      ovld = 0.0
      ratec = ' '
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
         pin = vt1 * ain                                             
         pout = -vt2 * ain                                           
         ain = 1000.0 * ain                                          
         aout = -ain                                                 
         qin = 0                                                     
         qout = 0                                                    
         amag = abs (ain)
         whichend1 = 1
         actual_amps = amag
         whichend2 = 1
         actual_mva = pin
         if (rating .gt. 0.0) then
            ovld = 100.0 * amag / rating
            irate = rating
            write (ratec, 103) rattag, irate, 'amp'
  103       format (a, 1x, i4, 1x, a)
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
         ain = 1000.0 * ain                                          
         aout = -ain                                                 
         qin = 0                                                     
         qout = 0                                                    
         amag = abs (ain)
         whichend1 = 1
         actual_amps = amag
         whichend2 = 1
         actual_mva = pin
         if (rating .gt. 0.0) then
            ovld = 100.0 * amag / rating
            irate = rating
            write (ratec, 103) rattag, irate, 'amp'
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
         pin=real(s(1))*bmva                                         
         qin=aimag(s(1))*bmva                                        
         pout=real(s(2))*bmva                                        
         qout=aimag(s(2))*bmva                                       
         if (ltype .eq. 5 .or. ltype .eq. 6) then
            if (cabs(a(1)) .ge. cabs(a(2))) then
               whichend1 = 1
               actual_amps = 577.3502692 * bmva * cabs(a(1)) / base1
            else
               whichend1 = 2
               actual_amps = 577.3502692 * bmva * cabs(a(2)) / base2
            endif
            a1 = cabs(s(1))*bmva
            a2 = cabs(s(2))*bmva
            vamag = amax1(a1, a2)
            if (a1 .ge. a2) then
               whichend2 = 1
               actual_mva = a1
            else
               whichend2 = 2
               actual_mva = a2
            endif
            if (rating .gt. 0.0) then
               ovld = 100.0 * vamag / rating
               irate = rating
               write (ratec, 103) rattag, irate, 'MVA'
            endif
         else if (ltype .eq. 1) then
            if (cabs(a(1)) .ge. cabs(a(2))) then
               whichend1 = 1
               actual_amps = 577.3502692 * bmva * cabs(a(1)) / base1
            else
               whichend1 = 2
               actual_amps = 577.3502692 * bmva * cabs(a(2)) / base2
            endif
            a1 = cabs(s(1)) * bmva
            a2 = cabs(s(2)) * bmva
            vamag = amax1(a1, a2)
            if (a1 .ge. a2) then
               whichend2 = 1
               actual_mva = a1
            else
               whichend2 = 2
               actual_mva = a2
            endif
         else 
            a1 = 577.3502692 * bmva * cabs(a(1)) / base1
            a2 = 577.3502692 * bmva * cabs(a(2)) / base2
            amag = amax1 (a1, a2)
            if (a1 .ge. a2) then
               whichend1 = 1
               actual_amps = a1
            else
               whichend1 = 2
               actual_amps = a2
            endif
            if (cabs(s(1)) .ge. cabs(s(2))) then
               whichend2 = 1
               actual_mva = cabs(s(1)) * bmva
            else
               whichend2 = 2
               actual_mva = cabs(s(2)) * bmva
            endif
            if (rating .gt. 0.0) then
               ovld = 100.0 * amag / rating
               irate = rating
               write (ratec, 103) rattag, irate, 'amp'
            endif
         endif

      endif                                                          
                                                                        
      ploss = pin + pout                                             
      qloss = qin + qout                                             

      return                                                            
      end                                                               
