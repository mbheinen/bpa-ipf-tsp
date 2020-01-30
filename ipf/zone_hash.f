C    @(#)zone_hash.f	20.3 2/13/96
	integer function zone_hash (zone)
	character*(*)  zone

c	Integer function zone_hash compute the hash value of zone

	include        'ipfinc/parametr.inc'
	include        'ipfinc/zonehash.inc'

	integer        k, i

        k = 0
        do i = 1, 2
           k = k + k + ichar (zone(i:i))
        end do
	zone_hash = mod (k, HASHSIZE_Z) + 1

	return
	end
