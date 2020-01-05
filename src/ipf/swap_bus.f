C    @(#)swap_bus.f	20.5 11/11/97
       subroutine swap_bus(m,n)                                           
                                                                        
       include 'ipfinc/parametr.inc' 

       include 'ipfinc/bus.inc' 
       include 'ipfinc/xsrt.inc' 
       include 'ipfinc/wsccbase.inc' 

       character tempc*8

       itemp = keysrt(m)
       keysrt(m) = keysrt(n)
       keysrt(n) = itemp

       tempc = bus(m)
       bus(m) = bus(n)
       bus(n) = tempc

       temp = base(m)
       base(m) = base(n)
       base(n) = temp

       tempc = owner(m)
       owner(m) = owner(n)
       owner(n) = tempc

       tempc = zone(m)
       zone(m) = zone(n)
       zone(n) = tempc

       tempc = wsccbase(m)
       wsccbase(m) = wsccbase(n)
       wsccbase(n) = tempc

       itemp = bus_number(m)
       bus_number(m) = bus_number(n)
       bus_number(n) = itemp

       do i = 1, 16
          itemp = kbsdta(i,m)
          kbsdta(i,m) = kbsdta(i,n)
          kbsdta(i,n) = itemp
       enddo

       temp = e(m)
       e(m) = e(n)
       e(n) = temp

       temp = f(m)
       f(m) = f(n)
       f(n) = temp

       temp = vstart(m)
       vstart(m) = vstart(n)
       vstart(n) = temp

       temp = capcor(1,m)
       capcor(1,m) = capcor(1,n)
       capcor(1,n) = temp

       temp = capcor(2,m)
       capcor(2,m) = capcor(2,n)
       capcor(2,n) = temp

       return  
       end  
