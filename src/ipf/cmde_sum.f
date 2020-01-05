C    @(#)cmde_sum.f	20.12 2/28/00
C****************************************************************
C
C       File: cmde_sum.f
C       Purpose: Routine to report the COMMON-MODE-OUTAGE data
C
C       Called by: OTEXT
C
C****************************************************************
	subroutine cmde_sum (error)
        implicit none
        integer error
 
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
        integer findstr, status, p, q, qold, proc_cmde, pbr
        integer k1, k2, i, j, mode, mt, kt
        real bkku_tot, gkku_tot
        external komp_cmde, swap_cmde
        character rec_type(4)*1, chg_type(2)*1, br_own*3, flag*40
 
        data rec_type / 'B', '+', 'L', 'T' /
        data chg_type / 'D', 'M' /
 
        error = 0              ! 0 = normal
c                              ! 1 = error condition
c
c       Error check and print out Common Mode Summary
c
        call forbtm()
        outbuf = 'Summary of Common Mode Outages'
        call rpnlod
    
        write (outbuf, 180)
  180   format (t2, 'Mode  Name')
        call shdlod(1)
        outbuf = ' '
        call shdlod(2)
        call shdlod(3)
        call shdlod(4)
        call shdlod(5)
        call fortop()
c
c       Sort comm_mode()
c 
        key = 1
        if (num_comm_mode .gt. 1) 
     &     call qiksrt (1, num_comm_mode, komp_cmde, swap_cmde)

        do mode = 1, num_comm_mode
           call space(1)
           if (comm_status(mode) .eq. 0) then
             flag = ' '
           else
             flag = '* * WARNING - MISSING MODE ELEMENT * *'
           endif
           write (outbuf, 190) mode, comm_mode(mode)(1:40), flag
  190      format (t2, i3, t8, a, t52, a)
           call prtout (1)
           call space(1)
           write (outbuf, 200)
  200      format (t8, 'Ty  Ch  Own  Zones  Bus1           Bus2     ',
     &             '       /----- Load -----//----- Shunt ----//--',
     &             ' Generation --/') 
           call prtout (1)
           write (outbuf, 202)
  202      format (t66, 'MW     MVAR       MW     MVAR       ',
     &             'MW     MVAR') 
           call prtout (1)
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
     &        call qiksrt (1, num_sort, komp_cmde, swap_cmde)
           call chek_cmde (mode, error)
c
c          Compute residual shunt admittance for all deleted buses.  
c          (Delete shunt { Y(1,1) } of all lines associated with each bus)
c
           i = 1
           do while (i .le. num_sort)
              q = sort(i)
              if (q .gt. 0) then
                 if (orig_type(1,q) .eq. 1 .and. 
     &               orig_type(2,q) .eq. 1) then
                    kt = orig_type(3,q)
                    gkku_tot = gkku(kt)
                    bkku_tot = bkku(kt)
                    qold = q
                    j = i + 1
                    do while (j .le. num_sort)
                       q = sort(j)
                       if (q .gt. 0) then
                          if ((orig_type(1,q) .eq. 3 .or.
     &                         orig_type(1,q) .eq. 4) .and. 
     &                         orig_type(2,q) .eq. 1 .and.
     &                         orig_type(3,q) .eq. kt) then
                             gkku_tot = gkku_tot - orig_val(3,q)
                             bkku_tot = bkku_tot - orig_val(4,q)
                          else
                             j = num_sort + 1
                          endif
                       endif
                       j = j + 1
                    enddo
                    orig_val(5,qold) = gkku_tot
                    orig_val(6,qold) = bkku_tot
                 endif
              endif
              i = i + 1
           enddo
c
c          print out the MODE
c
           do i = 1, num_sort
              q = sort(i)
              if (q .lt. 0) then
              else if (orig_type(1,q) .eq. 1 .or. 
     &                 orig_type(1,q) .eq. 2) then
                 kt = orig_type(3,q)
                 k1 = opt2inp(kt)
                 write (outbuf, 210) rec_type(orig_type(1,q)),
     &             chg_type(orig_type(2,q)), owner(k1), zone(k1),
     &             bus(k1), base(k1),
     &             orig_val(3,q) * bmva, orig_val(4,q) * bmva,
     &             orig_val(5,q) * bmva, orig_val(6,q) * bmva,
     &             (orig_val(1,q) + orig_val(3,q)) * bmva,
     &             (orig_val(2,q) + orig_val(4,q)) * bmva
  210            format (t8, a1, 3x, a1, 3x, a3, 2x, a2, 5x,
     &                   a8, f6.1, t59, 6f9.1)
                 call prtout (1)
              else if (orig_type(1,q) .eq. 3 .or. 
     &                 orig_type(1,q) .eq. 4) then
                 kt = orig_type(3,q)
                 k1 = opt2inp(kt)
                 mt = orig_type(4,q)
                 k2 = opt2inp(mt)
                 pbr = iabs(brnch_ptr(orig_type(6,q)))
                 call getchr(3,br_own,kbrnch(3,pbr))
                 if (inp2alf(k1) .lt. inp2alf(k2)) then
                   write (outbuf, 220) rec_type(orig_type(1,q)),
     &                chg_type(orig_type(2,q)), br_own, zone(k1),
     &                zone(k2), bus(k1), base(k1),
     &                bus(k2), base(k2), char(orig_type(5,q))
  220               format (t8, a1, 3x, a1, 3x, a3, 2x, a2, 1x, a2,
     &                      2x, a8, f6.1, 1x, a8, f6.1, 1x, a)
                    call prtout (1)
                 endif
              endif
           enddo
        enddo
        
        call forbtm()
        outbuf = 'Outage Simulation Input'
        call rpnlod
    
        outbuf = ' '
        call shdlod(1)
        call shdlod(2)
        call shdlod(3)
        call shdlod(4)
        call shdlod(5)
 
        return
        end  
