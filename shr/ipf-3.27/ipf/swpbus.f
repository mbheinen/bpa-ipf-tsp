C    @(#)swpbus.f	20.3 2/13/96
       subroutine swpbus(m,n)                                           
                                                                        
       include 'ipfinc/parametr.inc' 

       include 'ipfinc/bus.inc' 

       i = alf2inp(m) 
       alf2inp(m) = alf2inp(n) 
       alf2inp(n) = i 

       return  
       end  
