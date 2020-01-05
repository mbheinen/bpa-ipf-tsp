C    @(#)gtptibsv.f	20.3 3/29/99
C****************************************************************
C
C     File: gtptibsv.f
C
C     Purpose: Routine to extract bus data into a PTI-context 
C              array 
C
C     Input parameters:
C
C             p        - bus index
C             array    - array(24) in calling program
C                        (1) : bus type
c                       (17) : bfixed
c                       (18) : badjustable
c                       (19) : Pload (B record only)
c                       (20) : Qload (B record only)
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: ext_ptib.f
C
C****************************************************************
      subroutine gtptibsv (nb, array, version, pti_type)
      real array(*)
      integer nb, version, pti_type

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'

      integer error, ptr, bus_type(16)
      character cbtype*1, cbown*3, cbyear*2

      data bus_type / 1, 2, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 /

      error = 0                                                       

      do i = 1, 20
        array(i) = 0.0
      enddo
	
      ntype = kbsdta(1,nb)
      array(1) = ntype
      pti_type = bus_type(ntype)
      kt = inp2opt(nb)
      vm = dsqrt (e(kt) ** 2 + f(kt) ** 2)
      va = 57.29577 * datan2 (f(kt), e(kt))
      if (ntype .eq. 5 .or. ntype .eq. 12) go to 900  ! Skip DC buses

      do i = 2, 16
        array(i) = busdta(i,nb)
      enddo
      
      pgen = array(8)
      bbsdta = array(6)
      array(19) = array(3)
      array(20) = array(4)

c     Load supplementary ("+" card) load and shunt data

      bsup_fix = 0.0
      bsup_adj = 0.0
      ptr = kbsdta(15,nb)
      do while (ptr .gt. 0)
        call getchr(1, cbtype, kbctbl(8, ptr))
        call getchr(2, cbyear, kbctbl(9, ptr))
        call getchr(3, cbown, kbctbl(10, ptr))
        if (cbyear .eq. '*I' .or. 
     &     (cbtype .eq. 'A' .and. cbyear .eq. '01'))  then
            array(3) = array(3) + bctbl(2, ptr) * vm
     &                          + bctbl(4, ptr) * vm ** 2
            array(4) = array(4) + bctbl(3, ptr) * vm
     &                          + bctbl(5, ptr) * vm ** 2
        elseif (cbyear .eq. '*P' .or.
     &         (cbtype .eq. 'A' .and. cbyear .eq. '02'))  then
            array(3) = array(3) + bctbl(2, ptr) 
     &                          + bctbl(4, ptr) * vm ** 2
            array(4) = array(4) + bctbl(3, ptr) 
     &                          + bctbl(5, ptr) * vm ** 2
        elseif (cbtype .eq. 'A' .and. cbyear .eq. '*Z') then
            array(3) = array(3) + bctbl(2, ptr) 
     &                          + bctbl(4, ptr) * vm ** 2
            array(4) = array(4) + bctbl(3, ptr) 
     &                          + bctbl(5, ptr) * vm ** 2
        else
            array(3) = array(3) + bctbl(2, ptr) 
            array(4) = array(4) + bctbl(3, ptr) 
            array(5) = array(5) + bctbl(4, ptr) 
            if (cbtype .eq. 'A' .or. ntype .eq. 11)  then
               bsup_fix = bsup_fix + bctbl(5, ptr)
            else
               bsup_adj = bsup_adj + bctbl(5, ptr)
            endif
        endif

        array(8) = array(8) + bctbl(6,ptr)
        pgen = pgen + bctbl(6,ptr)
        array(9) = array(9) + bctbl(11,ptr)
        array(10) = array(10) + bctbl(12,ptr)
        ptr = bctbl_nxt(ptr)
      enddo
c
c     Net any generation on type BX or BC buses, or BQ buses 
c     (without Qmin, Qmax limits)
c
      badj = bbsdta + bsup_adj
c
      if (pgen .ne. 0.0 .and.
     &   (ntype .eq. 4 .or. ntype .eq. 11) .or.
     &   (ntype .eq. 7 .and. badj .ne. 0.0 .and.
     &  array(9) .eq. 0.0 .and. array(10) .eq. 0.0)) then
        array(3) = array(3) - array(8)
        array(4) = array(4) - array(9)
        array(19) = array(19) - array(8)
        array(20) = array(20) - array(9)
        array(8) = 0.0
        array(9) = 0.0
        pgen = 0.0
      endif
c
c     Change type BC, B , or BT buses with generation and non-zero
c     Q-limits to type BG
c
      if ((ntype .eq. 1 .or. ntype .eq. 4 .or. ntype .eq. 10) .and.
     &    pgen .ne. 0.0 .and.
     &    (array(9) .ne. 0.0 .or. array(10) .ne. 0.0)) then
        pti_type = 2
      endif
c
c     Identify switchable shunt in use.  Even if fixed, it will become 
c     the "BINIT" value of switched shunt data, not "BL" in bus data
c
      bsw_usd = 0.0
      kxd = busxdtptr(nb)
      if (kxd .gt. 0)  bsw_usd = xdata(5,kxd) + xdata(6,kxd)
c
c     Implement PTI variants into bus
c
C     All bus types except BV, BX, BD, and BM: Set Vmin to 0.0

      if (ntype .ne. 5 .and. ntype .ne. 6 .and. ntype .ne. 11 .and.
     &    ntype .ne. 12) then
        array(12) = 0.0
      endif

C     Bus types "B ", "BC", "BV", "BT": non-zero QMIN set to 0.0

      if (ntype .eq. 1 .or. ntype .eq. 4 .or. 
     &    ntype .eq. 6 .or. ntype .eq. 10) then
        if (pgen .eq. 0.0) then
          array(10) = 0.0         ! For Pgen = 0.0
        else
          array(10) = array(9)    ! Set Qmin = Qmax
          ntype = 7               ! Change to type BQ
          pti_type = bus_type(ntype)
        endif
      endif

C     Bus type BE:
C       1. Non-zero QLIMITS changed to type "BQ".
C       2. Zero QLIMITS changed to type "B " and the voltage hold
C          is blanked out.

      if (ntype .eq. 2) then
        if (array(9) .ne. 0.0 .or. array(10) .ne. 0.0) then
          ntype = 7
          pti_type = bus_type(ntype)
          array(1) = ntype
        else
          ntype = 1
          pti_type = bus_type(ntype)
          array(1) = ntype
          array(11) = 0.0
          array(12) = 0.0
        endif
      endif

      array(6)  = badj + bsup_fix - bsw_usd
      array(17) = bsup_fix
      array(18) = badj

c     Convert BQ bus without scheduled P or Qgen range to non-gen bus

      if (ntype .eq. 7 .and. pgen .eq. 0.0 .and.
     &         array(9) .eq. 0.0 .and. array(10) .eq. 0.0) then
        pti_type = 1
      endif

c     Calculate Qgen for all PTI type 2 (gen) and 3 (slack) buses:

      if (pti_type .eq. 2 .or. pti_type .eq. 3)  then
        qk = qnetu(kt)
        call allocq (nb, qk, qgen, qgnmax, qgnmin, qld, totcap,
     &               usecap, totrek, userek, unsked, qerr)
      endif

C     Bus type "BQ": When both variable shunt (0 -> B_sh) and Qgen
C                    (Qmax > Qmin) are present to control bus V,
C                    convert shunt into Qgen

      if (ntype .eq. 7 .and. array(9) .gt. array(10)) then
        if (abs (badj) .gt. 0.1) then
          write (dbug, 10020) bus(nb), base(nb)
10020     format (' Type BQ bus ', a8, f7.1, ' will hold voltage',
     &            ' with augmented Generator/Synchronous Condenser')
          vksq = array(11) ** 2
          if (badj .gt. 0.0) then
            array(9) = array(9) + (badj * vksq)
          else
            array(10) = array(10) + (badj * vksq)
          endif
          array(6) = bsup_fix
          usetot = usecap + userek
          qgen = qgen + usetot
          if (qgen .gt. array(9))   qgen = array(9)
          if (qgen .lt. array(10))  qgen = array(10)
        endif
      endif

900   return
      end
