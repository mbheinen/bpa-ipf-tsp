C    @(#)kmpowner.f	20.3 2/13/96
	integer function kmpowner (p, q)
        integer p, q
c
c	This function compares owner_o(p) with owner_o(q) 
c
        include 'ipfinc/ownhash.inc'

        if (p .eq. q) then
           kmpowner = 0
        else 
           i = alf2own(p)
           j = alf2own(q)
           kmpowner = kompr (owner_o(i), owner_o(j), junk)
        endif
        return
        end        
