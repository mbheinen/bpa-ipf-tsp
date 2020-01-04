C    @(#)bldowner.f	20.3 2/13/96
	integer function bldowner (owner)
	character*(*)  owner  
c
c	Function bldowner installs uniques owners into owner_o(*)
c
      	include 'ipfinc/ownhash.inc'

      	integer        h, owner_hash, p
      	external       owner_hash
      	logical        status

      	status = (numown .le. MAXOWNERS)

      	if (status) then
           h = owner_hash (owner)
           p = htable_o(h)
           do while (p .gt. 0)         !search for existing entities
              if (owner .ne. owner_o(p)) then
                 p = nextptr_o(p)
              else
                 p = -p                   
              endif
           enddo
           if (p .eq. 0) then
              numown = numown + 1
              nextptr_o(numown) = htable_o(h)
              htable_o(h) = numown
              owner_o(numown) = owner
              bldowner = numown
           else
              bldowner = -p         !duplicate entity flagged!
           endif
        else
           bldowner = 0             !overflow flagged!
        endif
        return
        end
