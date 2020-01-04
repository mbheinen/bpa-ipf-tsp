C    @(#)kombus.f	20.3 2/13/96
       integer function kombus(i,j)                                             
                                                                        
       include 'ipfinc/parametr.inc' 

       include 'ipfinc/bus.inc' 
       include 'ipfinc/qsdup.inc'
                                                                        
       m = alf2inp(i) 
       n = alf2inp(j) 
       if (kompr (bus(m), bus(n), kombus) .eq. 0) then                         
          kombus = 100.0 * (base(m) - base(n))                                 
          if ((kombus .eq. 0) .and. (m .ne. n)) dupsw = .true.  
       endif                                                            
                                                                        
       return                                                           
       end                                                              
