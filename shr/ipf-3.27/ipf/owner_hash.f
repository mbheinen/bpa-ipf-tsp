C    @(#)owner_hash.f	20.3 2/13/96
	integer function owner_hash (owner)
	character*(*)  owner

c	Integer function owner_hash compute the hash value of owner

	include 'ipfinc/ownhash.inc'

	integer        k, i

        k = 0
        do i = 1, 3
           k = k + k +ichar (owner(i:i))
        end do
	owner_hash = mod (k, OWN_HASHSIZE) + 1

	return
	end
