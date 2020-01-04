C    %W% %G%
      logical function irange (intgr,ilow,ihigh)                        
C     -   Tests whether INTGR is in the range of ILOW to IHIGH,         
C     -      inclusive.                                                 
C     -   IRANGE returns true if it is, false if not.                   
      if (ilow .le. intgr .and. intgr .le. ihigh) then                  
         irange = .true.                                                
      else                                                              
         irange = .false.                                               
      endif                                                             
      return                                                            
      end                                                               
