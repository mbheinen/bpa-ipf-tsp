C    %W% %G%
	subroutine updt_dip (diptyp)
	integer diptyp

	include 'tspinc/vfhistory.inc'

        integer ib, ptr, lowjj
        real dif1, dif2
        logical debug

        data debug / .false. /

        save
c
c       LOWJJ flags smallest delta-t entity
c
        lowjj = 1
        dif1 = dip_points(diptyp,2,lowjj) - dip_points(diptyp,1,lowjj)
        do jj = 2, MAX_DIPS 
          dif2 = dip_points(diptyp,2,jj) - dip_points(diptyp,1,jj)
          if (dif2 .lt. dif1) then
            dif1 = dif2
            lowjj = jj
          endif
        enddo

        do ib = 1, mxxbus       ! search threaded list for IDIP entity
          ptr = ptrbus(ib)      ! search threaded list for IDIP entity
          do while (ptr .gt. 0)
            if (idip(ptr) .lt. diptyp) then ! any of three entities
c                                             pertain to same bus
              ptr = pointer(ptr)
            else
              if (idip(ptr) .eq. diptyp) then
                dif2 = dip(2,ptr) - dip(1,ptr)
                if (dif1 .lt. dif2) then
                  idip_points(diptyp,lowjj) = ib  ! update minimum
c                                                   DIP if pertinent
                  dip_points(diptyp,1,lowjj) = dip(1,ptr)
                  dip_points(diptyp,2,lowjj) = dip(2,ptr)
                  dip_points(diptyp,3,lowjj) = dip(3,ptr)
                  if (debug) then
                    write (*, 7820) ib, ptr, idip(ptr), dip(1,ptr),
     &                dip(2,ptr)
 7820               format (' Update Min DIP entity IB = ', i4,
     &                ' PTR = ', i4, ' IDIP() = ', i4, 2f8.3)
                  endif
c
c                 Find new LOWJJ for smallest delta-t entity
c
                  lowjj = 1
                  dif1 = dip_points(diptyp,2,lowjj) 
     &                 - dip_points(diptyp,1,lowjj)
                  do jj = 2, MAX_DIPS 
                    dif2 = dip_points(diptyp,2,jj) 
     &                   - dip_points(diptyp,1,jj)
                    if (dif2 .lt. dif1) then
                      dif1 = dif2
                      lowjj = jj
                    endif
                  enddo
                endif
                ptr = 0
              else
                ptr = 0
              endif
            endif
          enddo
        enddo

        return
        end
