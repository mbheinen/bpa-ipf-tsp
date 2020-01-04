C    @(#)kmpvlimit.f	20.3 2/13/96
	integer function kmpvlimit (i,j)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/sortuvov.inc'

        kmpvlimit = 1000.0 * (v_range(1,i) - v_range(1,j))

        return
        end
