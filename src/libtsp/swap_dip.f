C    %W% %G%
	subroutine swap_dip (i, j)
	integer i, j

	include 'tspinc/vfhistory.inc'

        integer itemp

        itemp = sort_index(i)
        sort_index(i) = sort_index(j)
        sort_index(j) = itemp

        return
        end
