C    %W% %G%
	subroutine sort_dip (diptyp)
	integer diptyp

	include 'tspinc/vfhistory.inc'

        integer ib
        external komp_dip, swap_dip

        save
C
C       Obtain a sort index sort_index(*) ordered largest delta-t to 
C       smallest delta-t in DIP_POINTS(DIPTYP,*).   Use this index 
C       for report.
C
        do ib = 1, MAX_DIPS
          sort_index(ib) = ib   !initialize index
        enddo
        sort_key = diptyp
        call qiksrt (1, MAX_DIPS, komp_dip, swap_dip)

        return
        end
