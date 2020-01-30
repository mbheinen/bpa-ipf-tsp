C    @(#)kmpbsekv.f	20.3 2/13/96
	integer function kmpbsekv (p, q)
        integer p, q
c
c	This function compares basekvs(p) with basekvs(q) 
c
        include 'ipfinc/bsekvhsh.inc'

        if (p .eq. q) then
           kmpbsekv = 0
        else 
           kmpbsekv = 100.0 * (basekvs(p) - basekvs(q))
        endif
        return
        end        
