C    @(#)komp_cmde.f	20.3 2/13/96
       integer function komp_cmde (m,n)  
                                       
       include 'ipfinc/parametr.inc' 
 
       include 'ipfinc/bus.inc' 
       include 'ipfinc/comm_mode.inc' 
       include 'ipfinc/qksrt.inc' 
                                               
       common /sort_cmde/ num_sort, sort(100)
       integer num_sort, sort
 
       integer p, q
c
c      Sort fields: key = 1
c      Fields: 1 - comm_mode
c
c      Sort fields: key = 2
c      Fields: 1 - bus 1
c              2 - type
c              3 - bus 2
c              4 - ID
c              5 - change code
c
       if (key .eq. 1) then
          komp_cmde = kompr (comm_mode(m), comm_mode(n), junk)
       else
          p = sort(m)
          q = sort(n)

          k1 = inp2alf(opt2inp(orig_type(3,p)))
          k2 = inp2alf(opt2inp(orig_type(3,q)))
          komp_cmde = k1 - k2
          if (komp_cmde .eq. 0) then
             komp_cmde = orig_type(1,p) - orig_type(1,q)
          endif
          if (komp_cmde .eq. 0) then
             k1 = orig_type(4,p)
             if (k1 .gt. 0) k1 = inp2alf(opt2inp(k1))
             k2 = orig_type(4,q)
             if (k2 .gt. 0) k2 = inp2alf(opt2inp(k2))
             komp_cmde = k1 - k2
          endif
          if (komp_cmde .eq. 0) then
             komp_cmde = orig_type(5,p) - orig_type(5,q)
          endif
          if (komp_cmde .eq. 0) then
             komp_cmde = orig_type(2,p) - orig_type(2,q)
          endif
       endif
     
       return  
       end       
