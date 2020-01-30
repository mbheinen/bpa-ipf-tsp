C    %W% %G%
	integer function komp_frpt (i, j)
	integer i, j
c
c       This subroutine kompares entities for SORT-FINAL-REPORT-BUSES
c
	include 'tspinc/vfhistory.inc'

        integer ii, jj
        logical test1, test2
        
        ii = sort_index(i)
        jj = sort_index(j)
        if (sort_key .le. 3) then
          komp_frpt = 1000.0 * (rbuss(sort_key*2,ii) 
     &                       - rbuss(sort_key*2,jj))
        else
          komp_frpt = -1000.0 * (rbuss(sort_key*2,ii) 
     &                        - rbuss(sort_key*2,jj))
        endif

        return
        end

