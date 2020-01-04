C    @(#)link_zbo.f	20.3 2/13/96
        integer function link_zbo (dummy1, dummy2)
        integer dummy1, dummy2
C
C****************************************************************
C
C       File: link_zbo.f
C
C	Purpose: Routine to link zones-basekv-owners
C
C       Author: Walt Powell  Date: 18 May 1992
C       Called by: p_gtdata.f
C
C****************************************************************
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

        integer p, oldp, link_zo, link_bo, link_zb
 
c       ----------------------------------------------------------
        entry link_zo (iz1, io1)
c
c       link zone-owner
c           
        oldp = 0
        p = zn_ow(iz1)
        do while (p .gt. 0 .and. (zbo(p) .lt. io1))
           oldp = p
           p = zbo_nxt(p)
        enddo
        if (p .eq. 0 .or. (zbo(p) .ne. io1)) then
           num_zbo = num_zbo + 1
           if (oldp .eq. 0) then 
              zn_ow(iz1) = num_zbo
           else
              zbo_nxt(oldp) = num_zbo
           endif
           zbo(num_zbo) = io1
           zbo_nxt(num_zbo) = p
        endif           
c
c       link owner-zone
c           
        oldp = 0
        p = ow_zn(io1)
        do while (p .gt. 0 .and. (zbo(p) .lt. iz1))
           oldp = p
           p = zbo_nxt(p)
        enddo
        if (p .eq. 0 .or. (zbo(p) .ne. iz1)) then
           num_zbo = num_zbo + 1
           if (oldp .eq. 0) then 
              ow_zn(io1) = num_zbo
           else
              zbo_nxt(oldp) = num_zbo
           endif
           zbo(num_zbo) = iz1
           zbo_nxt(num_zbo) = p
        endif           
        link_zo = 0
        return

c       ----------------------------------------------------------
        entry link_bo (ib2, io2)
c
c       link base-owner
c           
        oldp = 0
        p = bs_ow(ib2)
        do while (p .gt. 0 .and. (zbo(p) .lt. io2))
           oldp = p
           p = zbo_nxt(p)
        enddo
        if (p .eq. 0 .or. (zbo(p) .ne. io2)) then
           num_zbo = num_zbo + 1
           if (oldp .eq. 0) then 
              bs_ow(ib2) = num_zbo
           else
              zbo_nxt(oldp) = num_zbo
           endif
           zbo(num_zbo) = io2
           zbo_nxt(num_zbo) = p
        endif           
c
c       link owner-base
c           
        oldp = 0
        p = ow_bs(io2)
        do while (p .gt. 0 .and. (zbo(p) .lt. ib2))
           oldp = p
           p = zbo_nxt(p)
        enddo
        if (p .eq. 0 .or. (zbo(p) .ne. ib2)) then
           num_zbo = num_zbo + 1
           if (oldp .eq. 0) then 
              ow_bs(io2) = num_zbo
           else
              zbo_nxt(oldp) = num_zbo
           endif
           zbo(num_zbo) = ib2
           zbo_nxt(num_zbo) = p
        endif           
        link_bo = 0
        return

c       ----------------------------------------------------------
        entry link_zb (iz3, ib3)
c
c       link zone-base
c           
        oldp = 0
        p = zn_bs(iz3)
        do while (p .gt. 0 .and. (zbo(p) .lt. ib3))
           oldp = p
           p = zbo_nxt(p)
        enddo
        if (p .eq. 0 .or. (zbo(p) .ne. ib3)) then
           num_zbo = num_zbo + 1
           if (oldp .eq. 0) then 
              zn_bs(iz3) = num_zbo
           else
              zbo_nxt(oldp) = num_zbo
           endif
           zbo(num_zbo) = ib3
           zbo_nxt(num_zbo) = p
        endif           
c
c       link base-zone
c           
        oldp = 0
        p = bs_zn(ib3)
        do while (p .gt. 0 .and. (zbo(p) .lt. iz3))
           oldp = p
           p = zbo_nxt(p)
        enddo
        if (p .eq. 0 .or. (zbo(p) .ne. iz3)) then
           num_zbo = num_zbo + 1
           if (oldp .eq. 0) then 
              bs_zn(ib3) = num_zbo
           else
              zbo_nxt(oldp) = num_zbo
           endif
           zbo(num_zbo) = iz3
           zbo_nxt(num_zbo) = p
        endif           
        link_zb = 0
        return

        end
