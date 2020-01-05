C    @(#)gt_ykmltc.f	20.2 3/29/99
	subroutine gt_ykmltc (error) 
        integer error 
c 
c       This routine generates the Y-admittances of non-transformers 
c       and non-phase shifters. 
c 
        include 'ipfinc/parametr.inc' 
 
        include 'ipfinc/alpha.inc' 
        include 'ipfinc/blank.inc' 
        include 'ipfinc/branch.inc' 
        include 'ipfinc/bus.inc' 
        include 'ipfinc/tran.inc' 
        include 'ipfinc/ordsta.inc' 
        include 'ipfinc/ykm_ltc.inc' 
c 
        integer ptr 
        complex*16 y(2,2)
 
        error = 0 
        if (.not. ykm_flag) then 
  	  do jt = 1, ntota 
            k1 = ltran(1,jt) 
            k2 = ltran(9,jt) 
            if (ordltc .eq. 2) then 
              k1 = opt2inp(k1) 
              k2 = opt2inp(k2) 
            endif 
 
            do i = 1, 2 
              do j = 1, 2 
                ykm_ltc(i,j,jt) = cmplx (0d0, 0d0) 
	        ykm_ltc(i,j,jt) = cmplx (0d0, 0d0) 
              enddo 
            enddo 
 
            ptr = kbsdta(16,k1) 
            do while (ptr .gt. 0) 
              if (ky(ptr) .eq. k2) then 
                if (brtype(ptr) .ne. 4 .and.  
     &              brtype(ptr) .ne. 5 .and. 
     &              brtype(ptr) .ne. 6) then   
                  call pieqiv (ptr, y, error) 
                  do i = 1, 2 
                    do j = 1, 2 
                      ykm_ltc(i,j,jt) = ykm_ltc(i,j,jt) + y(i,j) 
	            enddo 
                  enddo 
                endif 
              endif 
              ptr = brnch_nxt(ptr) 
            enddo 
          enddo 
          ykm_flag = .true. 
        endif 
        return 
        end 
