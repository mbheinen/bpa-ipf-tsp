C    @(#)swapuvov.f	20.3 2/13/96
	subroutine swapuvov (p, q)
        integer p, q
c
c	This function swaps vltsrt(p) with vltsrt(q).
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/sortuvov.inc'

        itemp = vltsrt(p)
        vltsrt(p) = vltsrt(q)
        vltsrt(q) = itemp

        return
        end        
