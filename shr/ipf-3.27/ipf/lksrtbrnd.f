C    @(#)lksrtbrnd.f	20.6 8/30/00
        subroutine lksrtbrnd (indx, k1, k2, type, id, sect, bptr1, 
     &                        bptr2, error)
        integer indx, k1, k2, sect, type, bptr1, bptr2, error
        character id*1
c
c       This subroutine is similar to lkbrdata except it inserts new
c       branches in alpha sort order and not not at the top of the 
c       stack.
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'

        integer p, pold
        logical finished

        pold = 0
        p = kbsdta(16,k1)
        finished = (p. eq. 0)
        do while (.not. finished)
          if (inp2alf(ky(p)) .gt. inp2alf(k2)) then
            finished = .true.
          else if (inp2alf(ky(p)) .eq. inp2alf(k2)) then
            if (type .eq. 1) then
              finished = .true.
            else if (brtype(p) .eq. 1) then
              pold = p
              p = brnch_nxt(p)
              finished = (p .eq. 0)                      
            else if (kompr (brid(p), id, junk) .gt. 0) then
              if (brsect(p) .gt. sect) then
                finished = .true.
              else
                pold = p
                p = brnch_nxt(p)
                finished = (p .eq. 0)                      
              endif
            else
              pold = p
              p = brnch_nxt(p)
              finished = (p .eq. 0)                      
            endif
          else
            pold = p
            p = brnch_nxt(p)
            finished = (p .eq. 0)                      
          endif
        enddo
        ltot2 = ltot2 + 1
        brnch_ptr(ltot2) = indx
        brnch_nxt(ltot2) = p
        if (pold .eq. 0) then
          kbsdta(16,k1) = ltot2
        else
          brnch_nxt(pold) = ltot2
        endif
        bptr1 = ltot2

        pold = 0
        p = kbsdta(16,k2)
        finished = (p. eq. 0)
        do while (.not. finished)
          if (inp2alf(ky(p)) .gt. inp2alf(k1)) then
            finished = .true.
          else if (inp2alf(ky(p)) .eq. inp2alf(k1)) then
            if (type .eq. 1) then
              finished = .true.
            else if (brtype(p) .eq. 1) then
              pold = p
              p = brnch_nxt(p)
              finished = (p .eq. 0)                      
            else if (kompr (brid(p), id, junk) .gt. 0) then
              if (brsect(p) .lt. sect) then
                finished = .true.
              else
                pold = p
                p = brnch_nxt(p)
                finished = (p .eq. 0)                      
              endif
            else
              pold = p
              p = brnch_nxt(p)
              finished = (p .eq. 0)                      
            endif
          else
            pold = p
            p = brnch_nxt(p)
            finished = (p .eq. 0)                      
          endif
        enddo
        ltot2 = ltot2 + 1
        brnch_ptr(ltot2) = -indx
        brnch_nxt(ltot2) = p
        if (pold .eq. 0) then
          kbsdta(16,k2) = ltot2
        else
          brnch_nxt(pold) = ltot2
        endif
        bptr2 = ltot2

        return
        end
