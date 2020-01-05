C    @(#)ownsch.f	20.3 2/13/96
	integer function ownsch (owner)
        character owner * (*)
c
c	This function performs a hash search for OWNER.
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/ownhash.inc'

        integer        first, last, p, compare 
        logical        found 

        first = 1 
        last = numown
        found = .false. 

        do while (first .le. last .and. .not.found) 
           p = (first + last) / 2 
           compare = kompr (owner_o(alf2own(p)), owner, junk) 
           if (compare .eq. 0) then 
              found = .true. 
           else if (compare .lt. 0) then 
              first = p + 1 
           else 
              last = p - 1 
           endif 
        end do 

        if (found) then 
           ownsch = alf2own(p) 
        else 
           ownsch = -max0 (first, last) 
        endif 

        return
        end
