C    @(#)swap_cmde.f	20.5 8/19/99
       subroutine swap_cmde (m,n)  
                                       
       include 'ipfinc/parametr.inc' 
 
       include 'ipfinc/comm_mode.inc' 
       include 'ipfinc/qksrt.inc' 
                                               
       common /sort_cmde/ num_sort, sort(100)
       integer num_sort, sort
 
       character tempc * 80
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
          tempc = comm_mode(m)
          comm_mode(m) = comm_mode(n)
          comm_mode(n) = tempc
          itemp = comm_ptr(m)
          comm_ptr(m) = comm_ptr(n)
          comm_ptr(n) = itemp
          itemp = comm_status(m)
          comm_status(m) = comm_status(n)
          comm_status(n) = itemp
       else
          itemp = sort(m)
          sort(m) = sort(n)
          sort(n) = itemp
       endif     
       return  
       end       
