C    @(#)cknt_cmde.f	20.3 2/13/96
C****************************************************************
C
C       File: cknt_cmde.f
C       Purpose: Routine to check for isolated subsystems created
C                by common mode outage "mode"
C
c       Note:    Array sort(num_sort) initally contains a list of 
C                deleted (common mode) buses; it ultimately contains
c                a list of buses in an isolated subsystem.
c
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: get_cmde
C
C****************************************************************
        subroutine cknt_cmde (mode, error)
        integer mode, error
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/apcom.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/comm_mode.inc'

        common /sort_cmde/ num_sort, sort(100)
        integer num_sort, sort

        common /scratch/ kolum(MAXBUS), net(200), mtrx(MAXBUS), 
     &                   last_out, comm_out(MAXBUS), num_isol,
     &                   isol(100)
        integer last_out, comm_out

        integer ptrl, ptru

        complex y

        error = 0

        do k = 1, nbus
           kolum(k)=0
        enddo
        do k = 1, num_sort
           kolum(sort(k)) = -1   ! Flag deleted buses
        enddo
C       
C       Expand outwards in eliminated network from a kernel node  
C       
        num_isol = 0
        num_syst = 0 
        do kt = 1, nbus
           if (kolum(kt) .eq. 0) then
              num_syst = num_syst + 1
              net(num_syst) = 1
              klast = 1
              mtrx(klast) = kt
              kolum(kt) = num_syst
              knext = 0
              do while (knext .lt. klast)
                 knext = knext + 1
                 nt = mtrx(knext)
                 do ptrl = ipyl(1,nt), ipyl(1,nt) + ipyl(2,nt) - 1
                    mt = mfarl(ptrl)
                    if (kolum(mt) .eq. 0) then
                       y = cmplx (ykml(1,ptrl), ykml(2,ptrl))
                       if (cabs (y) .gt. 0.1) then
                          klast = klast+1 
                          if (klast .gt. MAXBUS) call erexit ()
                          mtrx(klast) = mt 
                          kolum(mt) = num_syst  
                          net(num_syst) = net(num_syst) + 1
                       endif
                    endif
                 enddo
                 do ptru = ipyu(1,nt), ipyu(1,nt) + ipyu(2,nt) - 1
                    mt = mfaru(ptru)
                    if (kolum(mt) .eq. 0) then
                       y = cmplx (ykmu(1,ptru), ykmu(2,ptru))
                       if (cabs (y) .gt. 0.1) then
                          klast = klast+1 
                          if (klast .gt. MAXBUS) call erexit ()
                          mtrx(klast) = mt 
                          kolum(mt) = num_syst  
                          net(num_syst) = net(num_syst) + 1
                       endif
                    endif
                 enddo
              enddo
           endif
        enddo

        if (num_syst .gt. 1) then
           error = 1
c
c          Retrieve populations of all minor subsystems.
c
           max_net = 1
           do i = 2, num_syst
              if (net(i) .gt. net(max_net)) max_net = i
           enddo

           kt = 1
           do while (kt .le. nbus .and. num_isol .lt. 100)
              if (kolum(kt) .ne. max_net .and. kolum(kt) .gt. 0) then
                 num_isol = num_isol + 1
                 isol(num_isol) = kt
              endif
              kt = kt + 1
           enddo
        endif

        return
        end   
