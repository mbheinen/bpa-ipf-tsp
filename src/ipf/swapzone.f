C    @(#)swapzone.f	20.4 11/11/97
	subroutine swapzone (p, q)
        integer p, q
c
c	This function exchanges acznam(p) with acznam(q) 
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/arcntl.inc'
  
        character ztemp*2

        ztemp = acznam(p)
        acznam(p) = acznam(q)
        acznam(q) = ztemp

        itemp = acznum(p)
        acznum(p) = acznum(q)
        acznum(q) = itemp

        itemp = zone_number(p)
        zone_number(p) = zone_number(q)
        zone_number(q) = itemp

        return
        end        
