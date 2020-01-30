C    @(#)bsekvhsh.f	20.3 2/13/96
	integer function bsekvhsh (basekv)
	real basekv

c	Integer function bsekvhsh computes the hash value of basekv

	include        'ipfinc/bsekvhsh.inc'

	integer        k
        
        real PI
        parameter (PI = 3.1415926535)

        k = PI * basekv
	bsekvhsh = mod (k, BSE_HASHSIZE) + 1

	return
	end
