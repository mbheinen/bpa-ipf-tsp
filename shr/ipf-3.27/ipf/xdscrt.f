C    @(#)xdscrt.f	20.4 6/27/97
      function xdscrt (nt, nb, bold, b1, b2)                            
C                                                                       
C     This function (X-Discrete) discretizes BOLD within the            
C     the intervals                                                     
C                                                                       
C           B1 <= xdscrt <= B2 if BOLD => 0 or                          
C           B1 => xdscrt => B2 if BOLD <= 0.                            
C                                                                       
C     Input variables:                                                  
C                                                                       
C           NT     - index to XDATA array                              
C           NB     - index to BUS-BASE array                            
C           BOLD   - initial value of bus shunt susceptance             
C                                                                       
C     Output variables:                                                 
C                                                                       
C           B1     - lower discrete value of shunt                      
C           B2     - upper discrete value of shunt                      
C                                                                       
C                    ABS (B1) <= ABS (xdscrt) <= ABS (B2)               
C                                                                       
C           xdscrt - Discrete value of shunt                            
C                                                                       
                                                                              
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/xdata.inc'
                                                                        
      bnew = bold                                                       
C                                                                       
C     Find interval B1 => BNEW => B2.                                   
C                                                                       
      totrek = xdata(3,nt)                                              
      totcap = xdata(4,nt)                                              
      b1 = 0.0                                                          
      b2 = 0.0                                                          
      do 110 k = 7, 21, 2                                               
      n = xdata(k,nt)                                                   
      if (n .eq. 0) go to 120                                           
      if (bnew .lt. 0.0) then                                           
         if (xdata(k+1,nt) .gt. 0.0) go to 110                          
      else if (bnew .gt. 0.0) then                                      
         if (xdata(k+1,nt) .lt. 0.0) go to 110                          
C                                                                       
C     If BOLD is zero, set B1 to zero and B2 to first non_zero step.    
C                                                                       
      else                                                              
         b1 = 0.0                                                       
         b2 = xdata(k+1,nt)                                             
         go to 120                                                      
      endif                                                             
      do 100 l=1,n                                                      
         b1 = b2                                                        
         b2 = b2 + xdata(k+1,nt)                                        
         if (bnew .gt. 0.0) then                                        
            if (b2 .gt. bnew) go to 120                                 
         else                                                           
            if (b2 .lt. bnew) go to 120                                 
         endif                                                          
  100 continue                                                          
  110 continue                                                          
      if (kbsdta(1,nb) .eq. 11 .and.
     &   (bnew .lt. totrek .or. bnew .gt. totcap)) then                  
         write (errbuf(1),112) bnew, bus(nb), base(nb), totrek,         
     1      totcap                                                      
  112    format (' Initial shunt reactance ', f7.1, ' on bus ',
     1      a8, f6.1, ' is incompatible with "X" data:',
     2      2f7.1)                                                   
         call prterx ('W',1)                                            
      endif                                                             
                                                                        
  120 continue                                                          
C                                                                       
C     Find which value B1 or B2 is closest to BNEW.                     
C                                                                       
      if (abs (bnew - b1) .lt. abs (bnew - b2)) then                    
         bnew = b1                                                      
      else                                                              
         bnew = b2                                                      
      endif                                                             
      if (abs (totrek) .gt. 100.0 .or. totcap .gt. 100) then
         tolerance = 1.0
      else
         tolerance = 0.1
      endif 
      if (abs (bnew - bold) .gt. tolerance) then                             
         write (errbuf(1),140) bold, bus(nb), base(nb), b1, b2          
  140    format (' Initial shunt reactance ', f7.1, ' on BX bus ',
     1     a8, f6.1, ' is not properly discretized :', 2f7.1)            
         if (kbsdta(1,nb) .eq. 11) then
            call prterx ('W',1)                                            
         else if (kbsdta(1,nb) .ne. 7) then
           call prterx ('I',1)                                            
         endif
      endif                                                             
      if (kase1(38) .gt. 0) then                                        
         write (dbug,160) nt, bold, bnew, b1, b2, totrek, totcap        
  160    format (' Discrete BX state: ', i4, ' Initial X ', 2f10.1,        
     1      ' Adjacent states ', 2f10.1, ' Total X ', 2f10.1)             
      endif                                                             
      xdscrt = bnew                                                     
      return                                                            
      end                                                               
