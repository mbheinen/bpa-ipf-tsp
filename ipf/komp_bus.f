C    @(#)komp_bus.f	20.3 2/13/96
       integer function komp_bus(m,n)  
                                       
       include 'ipfinc/parametr.inc' 

       include 'ipfinc/bus.inc' 
       include 'ipfinc/qsdup.inc'
                                               
       if (kompr (bus(m), bus(n), komp_bus) .eq. 0) then  
          komp_bus = 100.0 * (base(m) - base(n))     
          if ((komp_bus .eq. 0) .and. (m .ne. n)) dupsw = .true.  
       endif     
     
       return  
       end       
