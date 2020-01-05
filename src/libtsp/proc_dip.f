C    %W% %G%
	subroutine proc_dip (ib, ia, value, time, dipptr, diptyp)
	integer ib, ia, dipptr, diptyp
        real value

	include 'tspinc/vfhistory.inc'

        integer violation, ptr, last, old_dip
        real thrshd
        logical debug, finished

        data debug / .false. /

        save

        if (ia .eq. 1) then
          if (.not. load_bus (ib)) then
            diptyp = 1                                             
            thrshd = vdipnl                                        
          else
            diptyp = 2                                              
            thrshd = vdipld                                         
          endif
        else if (ia .eq. 2) then
          diptyp = 3                                                 
          thrshd = frqdip                                            
        endif

c       get-dip-activity              ! search threaded list for 
c                                     ! IDIP entity                       
        dipptr = 0
        last = 0                      ! previous index
        ptr = ptrbus(ib)              

        finished = .false.
        do while (.not. finished)     ! while in threaded list ...
          if (ptr .gt. 0) then
            if (idip(ptr) .lt. diptyp) then
              last = ptr
              ptr = pointer(ptr)                                   
            else 
              if (idip(ptr) .eq. diptyp) then
                dipptr = ptr          ! match - return current index
              endif
              finished = .true.
            endif
          else
           finished = .true.
          endif
        enddo

        if (ia .eq. 1 .or. ia .eq. 2) then
          violation = 0                                                 
          if (ia .lt. 4 .and. value .lt. thrshd) violation = 1          
          if (ia .eq. 4 .and. value .gt. thrshd) violation = 1          
          if (violation .eq. 1 .and. dipptr .eq. 0) then

c           add-new-dip-entity            

            ptr = ptrbus(ib)              
            last = 0                      ! previous index
            finished = .false.
            do while (.not. finished)     ! while in threaded list ...
              if (ptr .gt. 0) then
                if (idip(ptr) .lt. diptyp) then
                  last = ptr              ! no match, get next entity
                  ptr = pointer(ptr)                                   
                else 
                  if (idip(ptr) .eq. diptyp) then
                    dipptr = ptr          ! match - return current index
                  else 
                    if (nextptr .gt. 0) then
c                                         ! not found, insert new entity
                      newptr = nextptr    ! retrieve next available 
c                                           entity
                      nextptr = pointer(nextptr)                     
                      if (last .eq. 0) then ! link new entity to previous 
c                                         index
                        ptrbus(ib) = newptr         
                      else 
                        pointer(last) = newptr
                      endif
                      pointer(newptr) = ptr
                      ptr = newptr
                      dipptr = ptr          ! return new index
                    else
                      dipptr = 0            ! silent error - no available 
c                                             space
                    endif
                  endif
                  finished = .true.
                endif
              else                        ! end of threaded list - 
c                                           insert at end
                if (nextptr .gt. 0) then
                  newptr = nextptr
                  nextptr = pointer(nextptr)
                  if (last .eq. 0) then
                    ptrbus(ib) = newptr
                  else 
                    pointer(last) = newptr
                  endif
                  pointer(newptr) = ptr
                  ptr = newptr
                  dipptr = ptr
                else
                  dipptr = 0               ! silent error - no available 
c                                            space
                endif
                finished = .true.
              endif
            enddo

            if (dipptr .gt. 0) then
              idip(dipptr) = diptyp
              dip(1,dipptr) = time
              dip(3,dipptr) = value
              if (debug) then
                write (*, 7780) ib, dipptr, value
 7780           format (' Added new DIP entity IB = ', i4,
     &                  ' PTR = ', i4, ' DIP ', e10.3)
              endif
            else
              write (*, 7790) ia, ib, diptyp, value
 7790         format (' Error - New DIP entity ignored ', 3i6, e10.3)
            endif

          else if (violation .eq. 0 .and. dipptr .gt. 0) then

c           remove-current-dip-entity                            

            lowjj = 1
            dif1 = dip_points(diptyp,2,lowjj)
     &           - dip_points(diptyp,1,lowjj)
            do jj = 2, MAX_DIPS      ! search for smallest delta-t entity
              dif2 = dip_points(diptyp,2,jj) - dip_points(diptyp,1,jj)
              if (dif2 .lt. dif1) then
                dif1 = dif2
                lowjj = jj
              endif
            enddo
            dif2 = dip(2,dipptr) - dip(1,dipptr)
            if (dif1 .lt. dif2) then
              idip_points(diptyp,lowjj) = ib  ! update minimum DIP if 
c                                               pertinent
              dip_points(diptyp,1,lowjj) = dip(1,dipptr)
              dip_points(diptyp,2,lowjj) = dip(2,dipptr)
              dip_points(diptyp,3,lowjj) = dip(3,dipptr)
            endif
            if (debug) then
              write (*, 7810) ib, dipptr, lastptr, dif1, dif2
 7810         format (' Remove DIP entity IB = ', i4, ' PTR = ', i4,
     &                ' LASTPTR = ', i4, ' DIF1/DIF2 ', 2e10.3)
            endif
            if (last .eq. 0) then
              ptrbus(ib) = pointer(dipptr)    ! rethread current link
            else 
              pointer(last) = pointer(dipptr)
            endif
            if (lastptr .gt. 0) pointer(lastptr) = dipptr
c                                             ! chain newly available 
c                                               space
            lastptr = dipptr
            pointer(lastptr) = 0                                           ! FLAG END OF LIST

            idip(dipptr) = 0
          else if (violation .eq. 1 .and. dipptr .gt. 0) then
c
c           Update current entity
c
            old_dip = dip(2,dipptr)
            dip(2,dipptr) = time                                 
            if (ia .lt. 4 .and. dip(3,dipptr) .gt. value) then
              dip(3,dipptr) = value
            else if (ia .eq. 4 .and. dip(3,dipptr) .lt. value) then
              dip(3,dipptr) = value
            endif
            if (debug) then
               write (*, 7800) ib, dipptr, old_dip, value
 7800          format (' Update DIP entity IB = ', i4, ' PTR = ',
     &                 ' DIP1/DIP2 ', 2e10.3)
            endif
          endif
        endif

        return
        end
