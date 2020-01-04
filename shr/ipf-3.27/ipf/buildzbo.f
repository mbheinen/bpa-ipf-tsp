C    %W% %G%
        integer function buildzbo(status)
        integer status
C
C****************************************************************
C
C       File: buildzbo.f
C
C	Purpose: Routine to link zones-basekv-owners
C
C       Author: Walt Powell  Date: 18 May 1992
C       Called by: p_gtdata.f
C
C****************************************************************
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/zonehash.inc'
        include 'ipfinc/ownhash.inc'
        include 'ipfinc/bsekvhsh.inc'
        include 'ipfinc/zbo.inc'

        integer bldowner, bldzone, bldbsekv, ptr, kmpzone, p, h, 
     &          oldp, zone_hash, bsekvhsh 
        character own*3
        external kmpzone, swapzone, kmpbsekv, swapbsekv, kmpowner, 
     &           swapowner

        buildzbo = 0   ! Initialize return status "success"

        write (*, 90)
   90   format (' * Rebuilding cross-reference arrays ...')

c       Initialize zone hash tables

        do nb = 1, MAXCZN
           zn_bs(nb) = 0
           zn_ow(nb) = 0
        enddo

c       Initialize owner hash tables

        do nb = 1, MAXOWNERS 
           ow_zn(nb) = 0
           ow_bs(nb) = 0
           alf2own(nb) = nb
        enddo

c       Initialize basekv hash tables

        do nb = 1, MAXBASEKVS
           bs_zn(nb) = 0 
           bs_ow(nb) = 0
        enddo

        num_zbo = 0
        numown = 0
        numbases = 0
        nztot = 0
c
c       Build cross-reference arrays.  The zone (acznam(1:nztot)) and
c       and basekv (basekvs(1:numbases)) arrays can be completed with
c       one pass through the bus data arrays. They are to be sorted 
c       before being linked.
c
c       Reinitialize zone hash tables
c
        do nb = 1, MAXCZN
           nextptr_z(nb) = 0
        enddo
        do nb = 1, HASHSIZE_Z
           htable_z(nb) = 0
        enddo
c
c       Reinitialize basekv hash tables
c
        do nb = 1, MAXBASEKVS
           nextptr_k(nb) = 0
        enddo
        do nb = 1, BSE_HASHSIZE
           htable_k(nb) = 0
        enddo
c
c       Rebuild owner hash tables 
c
        do nb = 1, MAXOWNERS
           nextptr_o(nb) = 0
        enddo
        do nb = 1, OWN_HASHSIZE
           htable_o(nb) = 0
        enddo

        do nbx = 1, ntot_alf
           nb = alf2inp(nbx)
           iz = iabs (bldzone(zone(nb), jarzn(nb)))
           ib = iabs (bldbsekv(base(nb)))
        enddo

        if (nztot .gt. 0) then
           call qiksrt (1, nztot, kmpzone, swapzone)
        endif
c
c       Rebuild zone hash tables from sorted zone array
c
        do nb = 1, MAXCZN
           nextptr_z(nb) = 0
        enddo
        do nb = 1, HASHSIZE_Z
           htable_z(nb) = 0
        enddo

        do nb = 1, nztot
           h = zone_hash (acznam(nb))
c
c          Insert zone acznam(nb) at end of linked list
c
           oldp = 0
           p = htable_z(h)
           do while (p .gt. 0)
              oldp = p
              p = nextptr_z(p)
           enddo
           if (oldp .eq. 0) then 
              htable_z(h) = nb
           else
              nextptr_z(oldp) = nb
           endif
           nextptr_z(nb) = 0
        enddo

        if (numbases .gt. 0) then
           call qiksrt (1, numbases, kmpbsekv, swapbsekv)
        endif
c
c       Rebuild basekv hash tables from sorted basekv array
c
        do nb = 1, MAXBASEKVS
           nextptr_k(nb) = 0
        enddo
        do nb = 1, BSE_HASHSIZE
           htable_k(nb) = 0
        enddo

        do nb = 1, numbases
           h = bsekvhsh (basekvs(nb))
c
c          Insert base basekvs(nb) at end of linked list
c
           oldp = 0
           p = htable_k(h)
           do while (p .gt. 0)
              oldp = p
              p = nextptr_k(p)
           enddo
           if (oldp .eq. 0) then 
              htable_k(h) = nb
           else
              nextptr_k(oldp) = nb
           endif
           nextptr_k(nb) = 0
        enddo

        do nbx = 1, ntot_alf
           nb = alf2inp(nbx)
           io = iabs (bldowner(owner(nb)))
           iz = iabs (bldzone(zone(nb), jarzn(nb)))
           ib = iabs (bldbsekv(base(nb)))
c
c          link zone-owner-base
c           
           status = link_zo( iz, io)
           status = link_bo( ib, io)
           status = link_zb( iz, ib)

           ptr = kbsdta(15,nb)
           do while (ptr .gt. 0)
              call getchr (3, own, kbctbl(10,ptr))
              iox = iabs(bldowner(own))
              if (io .ne. iox) then
c
c                link zone-owner-base
c           
                 status = link_zo( iz, iox)
                 status = link_bo( ib, iox)
              endif
              ptr = bctbl_nxt(ptr)
           enddo

           ptr = kbsdta(16,nb)
           do while (ptr .gt. 0)
              if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 4) then
                 nbr = iabs (brnch_ptr(ptr))
                 call getchr (3, own, kbrnch(3,nbr))
                 iox = iabs (bldowner(own))
                 if (io .ne. iox) then
c
c                   link zone-owner-base
c           
                    status = link_zo( iz, iox)
                    status = link_bo( ib, iox)
                 endif
              endif
              ptr = brnch_nxt(ptr)
           enddo
        enddo

        if (numown .gt. 0) then
           call qiksrt (1, numown, kmpowner, swapowner)
        endif

        return
        end
