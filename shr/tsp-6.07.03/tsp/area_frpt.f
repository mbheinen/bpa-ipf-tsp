C    %W% %G%
	subroutine area_frpt (ia, count)
	integer ia, count
c
c       This subroutine processes WORST-BUS-BY-AREA and
c       PRINT-FINAL-REPORT-BUS.
c
	include 'tspinc/vfhistory.inc'

        integer ib, is, n0, n1
        logical finished, debug, nochng, test1, test2
        
        data debug / .false. /

        save

C       TO WORST-BUS-BY-AREA

        iar = 0                           !initialize area index
	do while (iar .lt. MAX_AREA)           !check all areas...
          iar = iar + 1                   !increment area index
          if (arcnam(iar) .ne. ' ') then  !skip if no area name
            ib = 0                        !initialize bus index
            finished = .false.
            do while (ib .lt. mxxbus .and. .not. finished)  !check all 
C                                                            buses...
              ib = ib + 1                 !increment bus index
              is = sort_index(ib)         !pointer to bus seq number
              ibus = ibuss(1,is)          !bus seq number
              if (ibus .gt. 0) then       !skipped bus
                if (iar .eq. iarea(ibus)) then !proper area found...

c                 PRINT-FINAL-REPORT-BUS.
                  call bus_frpt (ibus, ia)
                  finished = .true.
                endif
              endif
            enddo                       !next area
          endif
        enddo

        return
        end
