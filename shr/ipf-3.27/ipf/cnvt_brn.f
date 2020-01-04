C    @(#)cnvt_brn.f	20.1 10/10/96
C****************************************************************
C
C   	File: cvnt_brn.f
C
C   	Purpose: Convert current entity of branch data record according
C                to input options
C                             
C       Input parameters:
C
C             ptr      - current entity in kx(), ky(), etc
C             nsec     - current entity a pseudo()
c             xbuf1    - first converted branch record
c             xbuf2    - secont converted branch record
C
C   	Author: Walter Powell                 Date: 3 Oct 1996
C   	Called by: ext_brn.f
C
C****************************************************************
C
      integer function cnvt_brn ( ptr, nsec, xbuf1, xbuf2)
      character *(*) xbuf1, xbuf2
      integer ptr, nsec
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/pseudo_b.inc'

      integer side, ptrx
      logical found
c
c     Convert section according to current entity in pseudo(1,nsec)
c
      if (pseudo(5,nsec) .ne. 0) then
c
c       Convert unbalanced pi-branches to equivalent balanced pi-s.
c
        nbr = iabs (brnch_ptr(ptr))
        r = brnch(5,nbr)
        x = brnch(6,nbr)
        g1 = brnch(7,nbr)
        b1 = brnch(8,nbr)
        g2 = brnch(9,nbr)
        b2 = brnch(10,nbr)

        ptrx = ltot2 + 1
        if (pseudo(3,nsec) .gt. 0) then
          kx(ptrx) = pseudo(3,nsec)
        else
          kx(ptrx) = kx(ptr)
        endif
        ky(ptrx) = pseudo(6,nsec)
        brid(ptrx) = brid(ptr)
        brsect(ptrx) = 0
        brtype(ptrx) = 3
        brnch_ptr(ptrx) = brnch_ptr(ptr)

        side = pseudo(5,nsec)
        do i = 1, 2
          if (side .eq. 1) then
            brnch(5,nbr) = 0.0
            brnch(6,nbr) = 0.0001
            brnch(7,nbr) = 0.0
            brnch(8,nbr) = 0.5 * (amin1 (b1, b2) - amax1 (b1, b2))
            brnch(9,nbr) = 0.0
            brnch(10,nbr) = 0.0
          else
            brnch(6,nbr) = x - 0.0001
            brnch(7,nbr) = 0.5 * (g1 + g2)
            brnch(8,nbr) = amax1 (b1, b2) 
            brnch(9,nbr) = 0.0
            brnch(10,nbr) = 0.0
          endif
                              
          if (i .eq. 1) then
            call bcdbrn( ptrx, xbuf1 )
          else
            call bcdbrn( ptrx, xbuf2 )
          endif

          kx(ptrx) = pseudo(6,nsec)
          if (pseudo(4,nsec) .gt. 0) then
            ky(ptrx) = pseudo(4,nsec)
          else
            ky(ptrx) = ky(ptr)
          endif

          side = 3 - side
          brnch(5,nbr) = r
          brnch(6,nbr) = x
          brnch(7,nbr) = g1
          brnch(8,nbr) = b1
          brnch(9,nbr) = g2
          brnch(10,nbr) = b2

        enddo
        cnvt_brn = 2
      else
c
c       Convert section to pseudo bus
c
        ptrx = ltot2 + 1
        kx(ptrx) = pseudo(3,nsec)
        ky(ptrx) = pseudo(4,nsec)
        brid(ptrx) = brid(ptr)
        brsect(ptrx) = 0
        brtype(ptrx) = brtype(ptr)
        brnch_ptr(ptrx) = brnch_ptr(ptr)
                            
        call bcdbrn( ptrx, xbuf1 )

        xbuf2 = ' '
        cnvt_brn = 1
      endif
      return
      end
