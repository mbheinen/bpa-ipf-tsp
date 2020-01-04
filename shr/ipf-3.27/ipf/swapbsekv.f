C    @(#)swapbsekv.f	20.3 2/13/96
	subroutine swapbsekv (p, q)
        integer p, q
c
c	This function exchanges basekvs(p) with basekvs(q) 
c
        include 'ipfinc/bsekvhsh.inc'
  
        real temp

        temp = basekvs(p)
        basekvs(p) = basekvs(q)
        basekvs(q) = temp

        return
        end        
