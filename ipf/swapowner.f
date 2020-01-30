C    @(#)swapowner.f	20.3 2/13/96
	subroutine swapowner (p, q)
        integer p, q
c
c	This function exchanges alf2own(p) with alf2own(q) 
c
        include 'ipfinc/ownhash.inc'
  
        integer temp 

        temp = alf2own(p)
        alf2own(p) = alf2own(q)
        alf2own(q) = temp

        return
        end        
