C    @(#)kmpuvov.f	20.3 2/13/96
	integer function kmpuvov (p, q)
        integer p, q
c
c	This function compares vltsrt(p) with vltsrt(q) using fields
c       <area_name><bus_name><base_kv> 
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/sortuvov.inc'

        if (p .eq. q) then
           kmpuvov = 0
        else 
           m = vltsrt(p)
           n = vltsrt(q)
           if (inp2alf(m) .gt. ntot_alf .or. inp2alf(n) .gt. ntot_alf) 
     &        then
              kmpuvov = kompr (bus(m), bus(n), junk)
              if (kmpuvov .eq. 0) then
                 kmpuvov = 100.0 * (base(m) - base(n))
              endif
              if (kmpuvov .eq. 0) then
                 kmpuvov = kompr (arcnam(jarzn(m)), arcnam(jarzn(n)), 
     &                            junk)
              endif
           else
              kmpuvov = kompr (arcnam(jarzn(m)), arcnam(jarzn(n)), junk)
              if (kmpuvov .eq. 0) then
                 kmpuvov = kompr (bus(m), bus(n), junk)
              endif
              if (kmpuvov .eq. 0) then
                 kmpuvov = 100.0 * (base(m) - base(n))
              endif
           endif
        endif
        return
        end        
