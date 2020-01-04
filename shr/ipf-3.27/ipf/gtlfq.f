C    @(#)gtlfq.f	20.3 2/13/96
C****************************************************************
C
C   File: gtlfq.f
C   Purpose: Routine to compute line flow information using the
c            the base case data in residence. 
C
C   Author: Walt Powell  Date: 26 July 1993
C                        Modified: 
C   Called by: out_brn
C
C****************************************************************

      subroutine gtlfq (ptr, pin, qin, ploss, qloss, ovld, ratec,
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
c	Global variables used:
c		bmva kdtot, mtdcln, lskp
      include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global variables used:
c		inp2opt, kbsdta, e(r*8), f(r*8), base
      include 'ipfinc/branch.inc'
c	Global variables used:
c		brtype, kx, ky, brsect, brid, brnch_nxt
      include 'ipfinc/ordsta.inc'
c	Global variables used:
c		ordvlt
      include 'ipfinc/dc2t.inc'
c	Global variables used:
c		dc2t(r*8)
      include 'ipfinc/dcmt.inc'
c	Global variables used:
c		dcmtbs(r*8), dcmtln(r*8)
      include 'ipfinc/pfstates.inc'
c	Global variables used:
c		None
                             
      complex * 16 y_temp(2,2)                                           
      complex v(2), y(2,2), a(2), s(2)                              
      character rattag * 1
      integer p, ptemp
      save v, y, a, s
C                                                                       
C     Local Variables                                                   
C********** check for ostates to implement plotting w/o solved case
C***      if ( ostates .lt. 5  .or.  brtype(ptr) .eq. 4 ) then
c*** check "lskp" instead of "ostates" to allow data to be calculated
c***       after minor changes ("ostates" is set to "2" in this case)
      if ( lskp .eq. 0  .or.  brtype(ptr) .eq. 4 ) then
        pin    = 0.0
        qin    = 0.0
        ploss  = 0.0
        qloss  = 0.0
        ovld = 0.0
        ratec = ' '
        actual_amps = 0.0
        whichend1   = 1
        actual_mva  = 0.0
        whichend2   = 1
        go to 1000
      endif

                                                                        
      k1 = kx(ptr)
      k2 = ky(ptr)
      ltype = brtype(ptr)
      base1 = base(k1)
      base2 = base(k2)
C
C     isect = 0 : Line without sections
C             1 : First line section being processed
C             2 : subsequent line sections being processed
C
      if (ltype .eq. 2 .or. ltype .eq. 7 .or. ltype .eq. 4) then
         isect = 0
      else if (brsect(ptr) .eq. 0) then
         isect = 0
      else
         p = kbsdta(16,k1)
         ptemp = 0
         isect = 1
         do while (p .gt. 0)
            if (ky(p) .eq. k2 .and. brid(p) .eq. brid(ptr)) then
               if ( ptemp .eq. 0 ) ptemp = p
               if (p .eq. ptr) then
                  p = 0
               else if (brtype(p) .eq. 1) then
               else if (brsect(p) .gt. 0) then
                  isect = 2
                  p = 0
               endif
            endif
            if (p .gt. 0) p = brnch_nxt(p)
         enddo
c
c        Define "base1" and "base2" appropriately with respect to
c        section brsect(ptr)
c
c***     p = kbsdta(16,k1)   ***** save some searching
         p = ptemp
         base2 = base1
         do while (p .gt. 0)
            if (ky(p) .eq. k2 .and. brid(p) .eq. brid(ptr)) then
               if (brtype(p) .eq. 5 .or. brtype(p) .eq. 6) then
                  base2 = base(k2)
               elseif (brtype(p) .ne. 1) then
                  base1 = base2
               endif
            endif
            if (p .eq. ptr) p = 0
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
      ovld = 0.0
      ratec = ' '

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
c
c	pieqiv is now double precision.
c
         do 120 i=1,2
             do 130 j=1,2
                 y_temp(i,j) = y(i,j)
  130        continue
  120    continue  
         call pieqiv(ptr, y_temp, kerr)                                   
         do 140 i=1,2
             do 150 j=1,2
                 y(i,j) = y_temp(i,j)
  150        continue
  140    continue
         if (isect .eq. 0) then
            v(1) = cmplx(e(kt),f(kt))                                   
            v(2) = cmplx(e(mt),f(mt))                                 
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
         pin=real(s(1)) * bmva                                         
         qin=aimag(s(1)) * bmva                                        
         pout=real(s(2)) * bmva                                        
         qout=aimag(s(2)) * bmva                                       
         if (ltype .eq. 5 .or. ltype .eq. 6) then
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

 1000 return                                                            
      end                                                               
