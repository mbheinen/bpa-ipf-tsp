C    %W% %G%
	subroutine swap_frpt (i, j)
	integer ia, count
c
c       This subroutine swaps entities for SORT-FINAL-REPORT-BUSES
c
	include 'tspinc/vfhistory.inc'

        integer itemp

        itemp = sort_index(i)
        sort_index(i) = sort_index(j)
        sort_index(j) = itemp

        return
        end

