C    %W% %G%
	subroutine sort_frpt (ia, count)
	integer ia, count
c
c       This subroutine processes SORT-FINAL-REPORT-BUSES
c
	include 'tspinc/vfhistory.inc'

        integer ib, ibus, ic, icount
        logical test1, test2, finished
	external komp_frpt, swap_frpt

        save

        do ib = 1, mxxbus
          sort_index(ib) = ib      !initialize indices for sorting
        enddo

        sort_key = ia
        call qiksrt (1, mxxbus, komp_frpt, swap_frpt)
c
c       Results are sorted - print single largest (or smallest 
c       violation.
c
        ic = 0                  !initialize counter
        ib = 0                  !initialize bus index
        icount = iabs (count)
        finished = .false.
        do while (ib .lt. icount .and. .not. finished) !write worst buses
          ib = ib + 1           !increment bus index
          is = sort_index(ib)   !pointer to bus seq number
          ibus = ibuss(1,is)    !bus sequence number
          test1 = (ia .lt. 4 .and. ic .le. icount .and.
     &            rbuss(ia*2,is) .gt. valmin(ia))  !limit to buses below 
c                                                   threshold
          test2 = (ia .eq. 4 .and. ic .le. icount .and.
     &            rbuss(ia*2,is) .lt. valmin(ia))  !limit to buses 
c                                                   above threshold
          if (test1 .or. test2) then
            finished = .true.
          else
            if (ibus .gt. 0) then
c             print-final-report-bus   !not a bus to skip so print
              call bus_frpt (is, ia)
              ic = ic + 1
            endif
          endif
        enddo

        return
        end

