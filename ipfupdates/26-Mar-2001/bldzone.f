C    %W% %G%
	integer function bldzone (zone, areanum)
	character*(*)  zone  
        integer areanum
c
c	Function bldzone installs unique zones into acznam(*)
c
      	include 'ipfinc/parametr.inc'
      	include 'ipfinc/blank.inc'
      	include 'ipfinc/arcntl.inc'
      	include 'ipfinc/zonehash.inc'

      	integer        h, zone_hash, p
      	external       zone_hash
      	logical        status

      	status = (nztot .le. MAXCZN)

      	if (status) then
           h = zone_hash (zone)
           p = htable_z(h)
           do while (p .gt. 0)         !search for existing entities
              if (zone .ne. acznam(p)) then
                 p = nextptr_z(p)
              else
                 p = -p                   
              endif
           enddo
           if (p .eq. 0) then
              nztot = nztot + 1
              nextptr_z(nztot) = htable_z(h)
              htable_z(h) = nztot
              acznam(nztot) = zone 
              acznum(nztot) = areanum
              bldzone = nztot
           else
              bldzone = -p         !duplicate entity flagged!
           endif
        else
           bldzone = 0             !overflow flagged!
        endif
        return
        end
