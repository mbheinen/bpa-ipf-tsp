C    %W% %G%
	integer function komp_dip (i, j)
        integer i, j

	include 'tspinc/vfhistory.inc'

        integer ii, jj
        real dif1, dif2

        save
C
C       Obtain a sort index IBUSS(3,*) ordered largest delta-t to 
C       smallest delta-t in DIP_POINTS(DIPTYP,*).   Use this index 
C       for report.
C
        ii = sort_index(i)
        jj = sort_index(j)
        dif1 = dip_points(sort_key,2,ii) - dip_points(sort_key,1,ii)
        dif2 = dip_points(sort_key,2,jj) - dip_points(sort_key,1,jj)
        komp_dip = -1000.0 * (dif1 - dif2)

        return
        end
