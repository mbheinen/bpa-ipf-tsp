C    %W% %G%
      subroutine turbinp(ign, line)
c     reads the turbine cards

      implicit none

      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/turb.inc'

c     local variables
c     ign       -  generator number
c     itb       -  turbine number
c     itb       -  index to turbdat array
c     bmva is the base mva for the run (defaults to 100.0)

      real temp(18), bmva
      integer ign, itb, j
      character line*80, cardtype*2

c     assign turbdat array offset index for this turbine
      turbindx(ign) = turboffset

c     assign turbine offset for local use
      itb = turbindx(ign)

c     check for near overflow of turbdat array
      if (turboffset+25.gt.TURBSLOTS*MAXGEN) then
        errbuf(1) = 'Overflow of turbdat array.  Increase TURBSLOTS.'
        call prterr('E', 1)
        stop
      endif

c     clear out residual values in temp vector
      do j = 1, 18
        temp(j) = 0.0
      enddo

c     get cardtype
      read(line, '(a2)') cardtype

      if (cardtype.eq.'TA') then

        read (line, 10010)  (temp(j), j = 1, 12)
10010   format(bz, 16x, 12f5.3)
c       turbdat values for TA:
c        1 2*Tch/dt + 1          2 hx12

c       save turbine type by index 
        turbtyp(ign) = 1

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =

c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 2

c       save data to turbdat
        turbdat(itb+1) = temp(1)

      else if (cardtype.eq.'TB') then
        read (line, 10010)  (temp(j), j = 1, 12)
c       turbdat values for TB:
c        1 2*Tch/dt + 1          2 2*Trh/dt + 1          3 2*Tco/dt + 1
c        4 hx12                  5 hx23                  6 hx34
c        7 Fhp                   8 Fip                   9 Flp    

c       save turbine type by index 
        turbtyp(ign) = 2

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =
      
c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 9

c       save data to turbdat
        turbdat(itb+1) = temp(1)
        turbdat(itb+7) = temp(2)
        turbdat(itb+2) = temp(4)
        turbdat(itb+8) = temp(5)
        turbdat(itb+3) = temp(7)
        turbdat(itb+9) = temp(8)

      else if (cardtype.eq.'TC') then
        read (line, 10010)  (temp(j), j = 1, 12)
c       turbdat values for TC:
c        1 2*Tch/dt + 1          2 2*Trh1/dt + 1         3 2*Trh2/dt + 1
c        4 2*Tco/dt + 1          5 hx12                  6 hx23
c        7 hx34                  8 hx45                  9 Fvhp
c       10 Fhp                  11 Fip                  12 Flp

c       save turbine type by index 
        turbtyp(ign) = 3

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =
      
c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 12

c       save data to turbdat
        turbdat(itb+ 1) = temp( 1)
        turbdat(itb+ 9) = temp( 2)
        turbdat(itb+ 2) = temp( 4)
        turbdat(itb+10) = temp( 5)
        turbdat(itb+ 3) = temp( 7)
        turbdat(itb+11) = temp( 8)
        turbdat(itb+ 4) = temp(10)
        turbdat(itb+12) = temp(11)

      else if (cardtype.eq.'TD') then
        read (line, 10010)  (temp(j), j = 1, 12)
c       turbdat values for TD:
c        1 2*Tch/dt + 1          2 2*Trh/dt + 1          3 2*Tco/dt + 1
c        4 hx12                  5 hx23                  6 hx34
c        7 Fhp                   8 Fip                   9 Flp/2.0    
c       10 Flp/2.0'

c       save turbine type by index 
        turbtyp(ign) = 4

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =
      
c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 10

c       save data to turbdat
        turbdat(itb+ 1) = temp(1)
        turbdat(itb+ 7) = temp(2)
        turbdat(itb+ 2) = temp(4)
        turbdat(itb+ 8) = temp(6)
        turbdat(itb+ 3) = temp(7)
        turbdat(itb+ 9) = temp(8)
        turbdat(itb+10) = temp(9)

      else if (cardtype.eq.'TE') then
        read (line, 10010)  (temp(j), j = 1, 12)
c       turbdat values for TE:
c        1 2*Tch/dt + 1          2 2*Trh/dt + 1          3 2*Tco/dt + 1
c        4 hx12                  5 hx23                  6 hx34
c        7 Fhp                   8 Fip                   9 Flp    

c       save turbine type by index 
        turbtyp(ign) = 5

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =
      
c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 9

c       save data to turbdat
        turbdat(itb+1) = temp(1)
        turbdat(itb+7) = temp(2)
        turbdat(itb+2) = temp(4)
        turbdat(itb+8) = temp(5)
        turbdat(itb+3) = temp(7)
        turbdat(itb+9) = temp(9)

      else if (cardtype.eq.'TF') then
        read (line, 10010)  (temp(j), j = 1, 12)
c       turbdat values for TF:
c        1 2*Tch/dt + 1          2 2*Trh1/dt + 1         3 2*Trh2/dt + 1
c        4 2*Tco/dt + 1          5 hx12                  6 hx23       
c        7 hx34                  8 hx45                  9 Fvhp
c       10 Fhp                  11 Fip/2                12 Fip/2'
c       13 Flp/2                14 Flp/2'

c       save turbine type by index 
        turbtyp(ign) = 6

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =
      
c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 14

c       save data to turbdat
        turbdat(itb+ 1) = temp( 1)
        turbdat(itb+ 2) = temp( 4)
        turbdat(itb+ 3) = temp( 7)
        turbdat(itb+ 4) = temp(10)
        turbdat(itb+ 9) = temp( 2)
        turbdat(itb+10) = temp( 6)
        turbdat(itb+11) = temp( 8)
        turbdat(itb+12) = temp( 9)
        turbdat(itb+13) = temp(11)
        turbdat(itb+14) = temp(12)

      else if (cardtype.eq.'TG') then
        read (line, 10010)  (temp(j), j = 1, 12)
c       turbdat values for TG:
c        1 2*T4/dt + 1           2 2*T5/dt + 1           3 2*T6/dt + 1
c        4 2*T7/dt + 1           5 hx12                  6 hx23       
c        7 hx34                  8 hx45                  9 K1
c       10 K2                   11 K3                   12 K4
c       13 K5                   14 K6                   15 K7
c       16 K8

c       save turbine type by index 
        turbtyp(ign) = 7

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =
      
c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 16

c       save data to turbdat
        turbdat(itb+ 1) = temp(1)
        turbdat(itb+ 9) = temp(2)
        turbdat(itb+10) = temp(3)
        turbdat(itb+ 2) = temp(4)
        turbdat(itb+11) = temp(5)
        turbdat(itb+12) = temp(6)
        turbdat(itb+ 3) = temp(7)
        turbdat(itb+13) = temp(8)
        turbdat(itb+14) = temp(9)
        turbdat(itb+ 4) = temp(10)
        turbdat(itb+15) = temp(11)
        turbdat(itb+16) = temp(12)

      else if (cardtype.eq.'TW') then
        read (line, 10010)  (temp(j), j = 1, 12)
c       turbdat values for TW:
c        1 2*(Tw/2)/dt + 1       2 hx12

c       save turbine type by index 
        turbtyp(ign) = 8

c       save source of turbine data by index: (NDY)
c       turbsource(ign) =
      
c       save turboffset for next turbine.  This number may
c       be increased as needed.
        turboffset = itb + 2

c       save data to turbdat
        turbdat(itb+ 1) = temp(4)

      endif

      return 
      end



















c	    if (mgov .le. 3) then
c	      write (errbuf(1), 10890) nbname, bkv, nid
c	      call prterr('E', 1)
c10890         format (3x, a8, 2x, f5.1, 2x, a1, 5x,
c     &         ' TURBINE CARD NOT NEEDED FOR G    overnor type --- error
c     & flag set'
c     &         )
c	      imchn = 2
c	    elseif (imchn .ne. 2 .and. notbl .eq. 0) then
C             DECODING TURBINE CARD FOR IEEE TYPE GOVERNORS
c	      read (work80(icard), 10900) subtyp, id, (temp(i), i = 1,
c     &         12)
c10900         format (bz, 1x, a1, 13x, a1, 12f5.3)
c	      if (.not. (subtyp .eq. 'W' .and. mgov .eq. 4)) then
c		if (.not. (subtyp .ne. 'W' .and. mgov .eq. 5)) then
c		  imchn = 2
c		  write (errbuf(1), 10910) nbname, bkv, nid
c		  call prterr('E', 1)
c10910             format (3x, a8, 2x, f5.1, 2x, a1, 5x,
c     &             ' GOVNR AND TURBINE CARDS ARE NO    t properly paired
c     &'
c     &             )
c		endif
c	      endif
c	    else
c	      goto 110
c	    endif
c	    igvtb = igvtb - 1
c	    if (.not. (subtyp .ne. 'D' .and. subtyp .ne. 'E' .and.
c     &       subtyp .ne. 'F')) then
c	      if (id .ne. 'H') then
c		imchn = 2
c		write (errbuf(1), 10920) nbname, bkv, nid
c		call prterr('E', 1)
c10920           format (3x, a8, 2x, f5.1, 2x, a1, 5x,
c     &           ' GEN. ID MUST BE H FOR TURBINE     types d, e, or f. '
c     &           )
c	      endif
c	      frcton = temp(2) + temp(5) + temp(8) + temp(11)
c	      if (abs(frcton-frctnh) .ge. 0.005) then
c		write (errbuf(1), 10930) nbname, bkv, nid
c		call prterr('E', 1)
c10930           format (3x, a8, 2x, f5.1, 2x, a1, 5x,
c     &           'SUM OF THE HP FRACTIONS MUST =     p on the mc/mf card
c     &--same for lp'
c     &           )
c		imchn = 2
c	      endif
c	    endif
c 
cC           CONVERT TIME CONSTANTS TO CYCLES FOR TURBINE DATA
c 
c	    do i = 1, 10, 3
c	      temp(i) = check_tc(temp(i), tlim, i, 'Turbine', nbname,
c     &         bkv, nid)
c	      if (temp(i) .gt. 0.0) temp(i) = temp(i)*frqbse
c	    enddo
c 
c	    cgov(5) = temp(1)
c	    cgov(6) = temp(4)
c	    if (subtyp .ne. 'A') then
c	      if (subtyp .ne. 'W') then
c		frcton = temp(2) + temp(3) + temp(5) + temp(6) + temp
c     &           (8) + temp(9) + temp(11) + temp(12)
c		if (abs(1.0-frcton) .ge. 0.01) then
c		  write (errbuf(1), 10940) nbname, bkv, nid
c10940             format (3x, a8, 2x, f5.1, 2x, a1, 5x,
c     &             ' Sum of all fractions on the t urbine card must add
c     &up to 1.0 '
c     &             )
c		  call prterr('E', 1)
c		  imchn = 2
c		  goto 110
c		else
c		  igv = 32
c		  do i = 20, igv
c		    cgov(i) = 0.0
c		  enddo
c		  cgov(20) = temp(2)    ! K1
c		  cgov(21) = temp(5)    ! K3
c		  cgov(22) = temp(7)    ! T6
c		  cgov(23) = temp(8)    ! K5
c		  cgov(24) = temp(10)   ! T7
c		  cgov(25) = temp(11)   ! K7
c		  if (subtyp .eq. 'D') then
c		    cgov(21) = temp(6)  ! K4 overwrites K3
c		    cgov(31) = temp(9)  ! K6
c		    mgov = 8
c		    igov = 10
c		  elseif (subtyp .eq. 'F') then
c		    cgov(21) = temp(6)  ! K4 overwrites K3
c		    cgov(31) = temp(9)  ! K6
c		    cgov(32) = temp(12) ! K8
c		    mgov = 10
c		    igov = 12
c		  elseif (subtyp .eq. 'E') then
c		    cgov(31) = temp(9)  ! K6 overwrites K5 [csw changed f
c		    mgov = 9
c		    igov = 10
c		  elseif (subtyp .eq. 'B') then
c		    mgov = 6
c		    igov = 9
c		  else
cC                   NOW SUBTYPE IS C
c		    mgov = 7
c		    igov = 11
c		  endif
c		endif
c	      endif
c	    endif
