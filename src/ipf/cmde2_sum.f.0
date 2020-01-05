C    @(#)cmde2_sum.f    20.2 8/21/98                           
C****************************************************************
C
C       File: cmde2_sum.f
C       Purpose: Routine to report the COMMON-MODE-OUTAGE data
C
C       Called by: p_gtcmde2
C
C****************************************************************
	subroutine cmde2_sum (error, mode, lunout)
        implicit none
        integer error, mode, lunout
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/blank.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/qksrt.inc'
 
        common /sort_cmde/ num_sort, sort(100)
        integer num_sort, sort
 
        logical finished 
        integer findstr, status, p, pold, q, qold, pbr, k1, k2, i 
        external komp_cmde, swap_cmde
        character rec_type(4)*1, chg_type(4)*1, br_own*3, flag*40
 
        data rec_type / 'B', '+', 'L', 'T' /
        data chg_type / 'D', 'M', ' ', 'R' /
 
        error = 0              ! 0 = normal
c                              ! 1 = error condition
 
c       Error check and print out Common Mode Summary

        outbuf = ' '
        write (lunout, fmt='(a)') outbuf
        if (comm_status(mode) .eq. 0) then
          flag = ' '
        else
          flag = '* * WARNING - MISSING MODE ELEMENT * *'
        endif
        write (outbuf, 190) mode, comm_mode(mode)(1:40), flag
  190   format (t2, i3, t8, a, t52, a)
        write (lunout, fmt='(a)') outbuf
        outbuf = ' '
        write (lunout, fmt='(a)') outbuf
        write (outbuf, 200)
  200   format (t8, 'Ty  Ch  Own  Zones  Bus1           Bus2     ',
     &          '       /----- Load -----//----- Shunt ----//--',
     &          ' Generation --/') 
        write (lunout, fmt='(a)') outbuf
        write (outbuf, 202)
  202   format (t66, 'MW     MVAR       MW     MVAR       ',
     &          'MW     MVAR') 
        write (lunout, fmt='(a)') outbuf
        p = comm_ptr(mode)
        num_sort = 0
        do while (p .gt. 0)
           q = change_ptr(p)
           do while (q .gt. 0)
              num_sort = num_sort + 1
              sort(num_sort) = q
              q = orig_nxt(q)
           enddo
           p = change_nxt(p)
        enddo
        key = 2
        if (num_sort .gt. 1) 
     &     call qiksrt (1, num_sort, komp_cmde, swap_cmde)
c       call chek_cmde2 (mode, error)
c
        p = comm_ptr(mode)
        do while (p .gt. 0)
           q = change_ptr(p)
           do while (q .gt. 0)
              if (orig_type(1,q) .eq. 1 .and. 
     &            orig_type(2,q) .eq. 1) then
                 k1 = orig_type(3,q)
                 qold = q
                 q = orig_nxt(q)
                 do while (q .gt. 0)
                    if ((orig_type(1,q) .eq. 3 .or.
     &                  orig_type(1,q) .eq. 4) .and. 
     &                  orig_type(2,q) .eq. 1 .and.
     &                  orig_type(3,q) .eq. k1) then
                    endif
                    q = orig_nxt(q)
                 enddo
              else
                 q = orig_nxt(q)
              endif
           enddo
           p = change_nxt(p)
        enddo
c
c       print out the MODE
c
        do i = 1, num_sort
           q = sort(i)
           if (q .lt. 0) then
           else if (orig_type(1,q) .eq. 1 .or. 
     &              orig_type(1,q) .eq. 2) then
              k1 = orig_type(3,q)
              write (outbuf, 210) rec_type(orig_type(1,q)),
     &          chg_type(orig_type(2,q)), owner(k1), zone(k1),
     &          bus(k1), base(k1),
     &          orig_val(3,q) * bmva, orig_val(4,q) * bmva,
     &          orig_val(5,q) * bmva, orig_val(6,q) * bmva,
     &          (orig_val(1,q) + orig_val(3,q)) * bmva,
     &          (orig_val(2,q) + orig_val(4,q)) * bmva
  210         format (t8, a1, 3x, a1, 3x, a3, 2x, a2, 5x,
     &               a8, f6.1, t59, 6f9.1)
              write (lunout, fmt='(a)') outbuf
           else if (orig_type(1,q) .eq. 3 .or. 
     &              orig_type(1,q) .eq. 4) then
              k1 = orig_type(3,q)
              k2 = orig_type(4,q)
              pbr = iabs(brnch_ptr(orig_type(6,q)))
              if (pbr .gt.0) then 
                call getchr(3,br_own,kbrnch(3,pbr))
              else
                br_own = '***'
              endif
              if (inp2alf(k1) .lt. inp2alf(k2)) then
                 write (outbuf, 220) rec_type(orig_type(1,q)),
     &             chg_type(orig_type(2,q)), br_own, zone(k1),
     &             zone(k2), bus(k1), base(k1),
     &             bus(k2), base(k2), char(orig_type(5,q))
  220            format (t8, a1, 3x, a1, 3x, a3, 2x, a2, 1x, a2,
     &                   2x, a8, f6.1, 1x, a8, f6.1, 1x, a)
                 write (lunout, fmt='(a)') outbuf
              endif
           endif
        enddo
        
        return
        end  
