C    @(#)bldbsekv.f	20.3 2/13/96
	integer function bldbsekv (basekv)
	real basekv  
c
c	Function bldbsekv installs unique basekv into basekvs(*)
c
      	include 'ipfinc/bsekvhsh.inc'

      	integer        h, bsekvhsh, p
      	logical        status

      	status = (numbases .le. MAXBASEKVS)

      	if (status) then
           h = bsekvhsh (basekv)
           p = htable_k(h)
           do while (p .gt. 0)         !search for existing entities
              if (basekv .ne. basekvs(p)) then
                 p = nextptr_k(p)
              else
                 p = -p                   
              endif
           enddo
           if (p .eq. 0) then
              numbases = numbases + 1
              nextptr_k(numbases) = htable_k(h)
              htable_k(h) = numbases
              basekvs(numbases) = basekv
              bldbsekv = numbases
           else
              bldbsekv = -p         !duplicate entity flagged!
           endif
        else
           bldbsekv = 0             !overflow flagged!
        endif
        return
        end
