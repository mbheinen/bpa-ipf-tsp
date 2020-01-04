C    @(#)kmpzone.f	20.3 2/13/96
	integer function kmpzone (p, q)
        integer p, q
c
c	This function compares zones(p) with zones(q) 
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/arcntl.inc'

        if (p .eq. q) then
           kmpzone = 0
        else 
           kmpzone = kompr (acznam(p), acznam(q), junk)
        endif
        return
        end        
