C    %W% %G%
      subroutine nout2
 
C     Purpose: This subroutine reads requested data from the swing
C     solution  history file (for008) and forms the initial output
C     files for the requested output data.
 
C     Called from swingm.
C     External subroutines/functions: busout, genout, ndcout, linout,
C     gdifot, wstfeq, wstvlt, prtwst, and badz.
 
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/wcom.inc'
      include 'tspinc/rddat.inc'
      include 'tspinc/gdif.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/dc.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/wga.inc'
      include 'tspinc/wgv.inc'
      include 'tspinc/wgf.inc'
      include 'tspinc/wgs.inc'
      include 'tspinc/worst.inc'
      include 'tspinc/wfeq.inc'
      include 'tspinc/hivlt.inc'
      include 'tspinc/fltim.inc'
      include 'tspinc/ibusd.inc'
      include 'tspinc/igend.inc'
      include 'tspinc/nwgntn.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/idctbl.inc'
      include 'tspinc/linmod.inc'
      include 'tspinc/indx2n.inc'
      include 'tspinc/ilind.inc'
      include 'tspinc/lindat.inc'
      include 'tspinc/kcnstx.inc'
      include 'tspinc/geno.inc'
      include 'tspinc/brnch.inc'
      include 'tspinc/nb.inc'
      include 'tspinc/kka.inc'
      include 'tspinc/space.inc'
      include 'tspinc/space1.inc'
      include 'tspinc/angle.inc'
      include 'tspinc/mtbl.inc'
      include 'tspinc/mgen.inc'
      include 'tspinc/igend1.inc'
      include 'tspinc/kbusno.inc'
      include 'tspinc/dcname.inc'
      include 'tspinc/room.inc'
      include 'tspinc/wstequ.inc'
      include 'tspinc/jbusdt.inc'
      include 'tspinc/lodbus.inc'
      include 'tspinc/spare1.inc'
      include 'tspinc/spare2.inc'
      include 'tspinc/vymn.inc'
      include 'tspinc/znox.inc'
 
      common /tcscdata/ tcscdata(MAXSTP,2,VYMNMAX)

      common /kngenn/ kngenn(2, MAXGEN)
      character*1 kngenc
      common /kngenc/ kngenc(MAXGEN)

      dimension iecsp(120000)
      equivalence (ecsp, iecsp)
 
      character*8 kpc, idc, ipar, dumc
      character*8 dcbus1, dcbus2, ibus1, ibus2, bus1, bus2, busnam(5),
     &            genid(5), ewbus
      dimension dat13(500)
      dimension msub1(2), msub2(2), msub4(2), msub7(2), msub10(2),
     &          msub12(2), msub13(2)
      equivalence (noepq, nogen(4)), (nofsat, nogen(5)), 
     &            (noxsat, nogen(8)), (nopacc, nogen(10)), 
     &            (nogenp, nogen(7)), (nogenq, nogen(11)), 
     &            (notqdp, nogen(13)), (nofcur, nogen(14)), 
     &            (nompa, nogen(15))
      equivalence (nobus, nobust), (noangl, nogen), (noangp, nogen(2)),
     &            (novfl, nogen(3)), (nogov, nogen(6)), (noreg, nogen
     &            (9)), (nosup, nogen(12))
      equivalence (mbust, mbus)
      dimension kcount(15)
      equivalence (kcount(1), ktang), (kcount(2), ktanp), 
     &            (kcount(3), ktvfl), (kcount(6), ktgov),
     &            (ksub(1), ksub1), (ksub(2), ksub2), (ksub(3), ksub4),
     &            (ksub(4), ksub4b), (ksub(5), ksub1a), 
     &            (ksub(6), ksub7), (ksub(7), ksub7a), 
     &            (ksub(8), ksub1b), (ksub(9), ksb12b), 
     &            (ksub(10), ksub7b), (ksub(11), ksub2b),
     &            (ksub(12), ksub12), (ksub(13), ksub2a), 
     &            (ksub(14), ksub4a)
      equivalence (msub(1), msub1), (msub(3), msub2), (msub(7), msub4),
     &            (msub(13), msub7), (msub(19), msub10), 
     &            (msub(23), msub12), (msub(25), msub13)
 
      equivalence (nolin1, noline(1)), (nolin2, noline(2)), 
     &            (nozapp, noline(3))
      dimension itable(5)
      dimension icl(20), iop(20)
      dimension ktemp(20), temp(20), kxtra(20)
      equivalence (kxtra, ktemp, temp)
      dimension ksrt(MAXBUS)
      dimension busbas(5), intnum(5)
      dimension swngdc(2, 50)
      equivalence (work(1001), dcang(1)), (work(1021), dci(1)), 
     &            (work(1041), dcv(1)), (work(1061), dcext(1)), 
     &            (work(1081), dcva(1)), (work(1101), dcvap(1))
      dimension dcang(20), dci(20), dcv(20), imodc(50), dcmods(20),
     &          dcext(20), dcva(20), dcvap(20), kndx(2,8)
 
C     Functions:
 
      logical dbghere
 
C     Local variables:
 
      dimension vfdint(MAXGEN), angint(MAXGEN), eyrint(MAXBUS), 
     &          eyiint(MAXBUS)
 
C     Separate PELEC array temporary until I can figure out this
C     ECS storage scheme.
 
      dimension pelec(MAXGEN)
      logical complet, any_dc
      logical debug
 
      data kndx / 5, 1, 6, 1, 7, 1, 5, 2, 6, 2, 7, 2, 4, 1, 4, 2 /

C     Begin   Begin   Begin   Begin   Begin   Begin   Begin
 
      debug = dbghere('NOUT2   ')
      if (debug) lstp = 0
      complet = .false.
      call mpost('NOUT2a')
      call prtout(1)
      call skipln(1)
      kmodec = knext - 6*25
      ltot = ltoto
      ibgd = 0
      idcbs = 0
      idcbr = 0
      iggd = 0
      ilgd = 0
      kzero = 0
      do i = 1, 7
        noline(i) = 0
      enddo
 
C     INITIALZE WORST VOLTAGE AND FREQUENCY TABLES
 
      do i = 1, nmx
        busfeq(i) = 0.0
      enddo

      call vfhist (1, 0.0)
 
      do i = 1, 20
        wfeqmg(i) = 10000.
        wfeqtm(i) = 0.0
        wfeqnm(i) = ' '
        wfeqkv(i) = 0.0
        wrstvt(i) = 1.0
        wrsttm(i) = 0.0
        wrstnm(i) = ' '
        wrstkv(i) = 0.0
        iwrsno(i) = 0
        hivvt(i) = .95
        hivtim(i) = 0.0
        hivnm(i) = ' '
        hivkv(i) = 0.0
        ihivno(i) = 0
        wgfval(i) = 0.0
        wgfnam(i) = ' '
        wgfkv(i) = 0.0
        wgftim(i) = 0.0
        wgfid(i) = ' '
        wgvval(i) = 0.0
        wgvnam(i) = ' '
        wgvkv(i) = 0.0
        wgvtim(i) = 0.0
        wgvid(i) = ' '
        wgaval(i) = 0.0
        wganam(i) = ' '
        wgakv(i) = 0.0
        wgatim(i) = 0.0
        wgaid(i) = ' '
      enddo
 
      igmx = 16*MAXGEN
      do i = 1, igmx
        mgen(i) = 0
      enddo
 
      do i = 1, nmx
        mbus(i) = 0
      enddo
      do i = 1, 15
        nogen(i) = 0
      enddo
 
      do j = 1, nmx
        do i = 1, 5
          jbusdt(i, j) = 0
        enddo
      enddo
 
C     Count the number of requests for real and reactive bus loads
 
C     IBUSD(4,n) is output code for n_th "B " card real load
C     IBUSD(5,n) is output code for n_th "B " card reactive load
 
      itotmw = 0
      itotvr = 0
      do ktrr = 1, ibdatt
        if (ibusd(4, ktrr) .gt. 0) itotmw = itotmw + 1
        if (ibusd(5, ktrr) .gt. 0) itotvr = itotvr + 1
      enddo
 
C     Limit num of load output requests to 25
 
      if (itotmw .gt. 25) then
        itotmw = 25
        write (errbuf(1), 10000)
10000   format (5x, 'MORE THAN 25 REAL BUS LOAD REQUESTS HAVE BEEN',
     &   ' ENTERED.  ONLY THE FIRST 25 WILL BE PROCESSED.')
        call prterr('W', 1)
      endif
      if (itotvr .gt. 25) then
        itotvr = 25
        write (errbuf(1), 10010)
10010   format (5x, 'MORE THAN 25 REACTIVE BUS LOAD REQUESTS HAVE BEEN'
     &   , ' ENTERED.  ONLY THE FIRST 25 WILL BE PROCESSED.')
        call prterr('W', 1)
      endif
 
C     Store bus/plot combo requests
C     First index of IBUSD is
C     1: Bus_num
C     2: |V|angle
C     3: Freq
C     4: Load(MW)
C     5: Load(MVAR)  
C     6: Grp_code
 
      ibecs = knext
      kdiff = ibecs - ksave
 
C     IBDATT is number of bus plot cards
 
      if (ibdatt .ne. 0) then
        do i = 1, ibdatt
          k = 1
          ino = ibusd(1, i)
          do j = 2, 5
            jb = ibusd(j, i)
            if (jb .lt. 1) then
              icl(k) = 0
              iop(k) = 0
            else
 
C             ICL means bus will be output
C             IOP is code for where output goes (prt,plot,aux)
 
              icl(k) = 1
              iop(k) = jb
 
C             MBUS means bus data will be fetched from solution
 
              if (j .lt. 4) mbus(ino) = jb
            endif
            k = k + 1
          enddo
          if ((ibusd(2, i) .ne. 0) .or. (ibusd(3, i) .ne. 0)) 
     &     nobust(1) = nobust(1) + 2
          ibgd = ibgd + 1
          igrp = ibusd(6, i)
          if (nogrp .ne. 0) then
 
C           Count and save new group codes
 
            do ind = 1, nogrp
              if (igrp .eq. igrup(ind)) goto 100
            enddo
          endif
 
          nogrp = nogrp + 1
          igrup(nogrp) = igrp
  100     intno = ibusd(1, i)
 
C         Store output vars and codes by bus number
 
          jbusdt(1, intno) = igrp
          jbusdt(2, intno) = iop(1)
          jbusdt(3, intno) = iop(2)
          jbusdt(4, intno) = iop(3)
          jbusdt(5, intno) = iop(4)
        enddo
      endif
 
C     Reading generator data from ecs or generator identification
C     from NWGNTC table and building IGEND1 and IGEND2 for
C     interrogation during output
 
      if (igdatt .ne. 0) then
 
C       First clear out arrays
 
        do i = 1, isg
          do j = 1, 31
            igend1(j, i) = 0
          enddo
        enddo
 
C       First subscript of gen plot req array IGEND
C       1: Gen_num
C       2: Angle
C       3: Freq_dev
C       4: E_fd
C       5: Flux_E_pq
C       6: Mn_fld_sat
C       7: P_mech
C       8: P_elec
C       9: Exc_sat
C       10: V_reg
C       11: P_accel
C       12: Q_gen
C       13: V_pss_out
C       14: T_damp
C       15: I_fd
C       16: Area_P_accel
C       17: Grp_code
 
        do i = 1, igdatt
          k = 1
          intno = igend(1, i)
 
C         Establish plotting group
 
          igrp = igend(17, i)
          do ind = 1, nogrp
            if (igrp .eq. igrup(ind)) goto 110
          enddo
          nogrp = nogrp + 1
          igrup(nogrp) = igrp
          if ((igend(16, i) .ne. 0) .and. (igrp .eq. 0)) goto 120
 
C         Save plot specs in array for any machine
C         Note mapping to indiv parm arrays
 
  110     continue
          do j = 2, 16
            jg = igend(j, i)
            if (jg .lt. 1) then
              icl(k) = 0
              iop(k) = 0
            else
              icl(k) = 1
              iop(k) = jg
              ind = (j-2)*MAXGEN
              mgen(ind+intno) = jg
              nogen(j-1) = nogen(j-1) + 1
              if (j .eq. 10) then
 
C               If V_reg desired, must also fetch PSS output
 
                if (msup(intno) .eq. 0) nosup = nosup + 1
                msup(intno) = jg
              endif
            endif
            k = k + 1
          enddo
 
C         Save plotting specs in by-gen array IGEND1
 
          iggd = iggd + 1
          igend1(1, intno) = igrp
          do k = 1, 15
            igend1(k+1, intno) = icl(k)
            igend1(k+16, intno) = iop(k)
          enddo
        enddo
        goto 130
 
  120   write (errbuf(1), 10020)
        call prterr('E', 1)
10020   format ('0 Area accelerating power has been requested but no are
     &a ident specified.  Program stopped. ')
        call erexit()
      endif
 
C     Sorting identifying data for the line data cards
 
  130 continue
 
C     ILDATT is number of line plotting cards
 
      kldatt = ildatt
      if (ildatt .ne. 0) then
        j = 0
        do i = 1, ildatt
          j = j + 1
          ksrt(i) = i
        enddo
        if (ildatt .ne. 1) then
          lim = ildatt + 1
          do while (.true.)
            kdone = 1
            lim = lim - 1
            n = 1
            if (ildatt .ne. 1) then
 
C             Perform bubble sort on line end busses (high-end fill)
C             KSRT is array of pointers to indiv line plot cards
 
              do i = 2, lim
                if (ilindn(1, 1, n) .le. ilindn(1, 1, i)) then
                  if (ilindn(1, 2, n) .le. ilindn(1, 2, i)) then
                    if (kompr(ilindc(1, n), ilindc(1, i), kdum) .le. 0)
     &               then
                      if (ilindn(1, 3, n) .le. ilindn(1, 3, i)) then
                        n = i
                        goto 140
                      endif
                    endif
                  endif
                endif
                kdone = 2
                il = n
                it = ksrt(il)
                ksrt(il) = ksrt(i)
                ksrt(i) = it
  140           continue
              enddo
            endif
            if (kdone .eq. 1) goto 150
          enddo
        endif
  150   continue
 
C       Reading line data from ecs and building LINDAT table. This
C       table is interrogated during the output of the data
 
C       ILINDN array for line nn in line plot set
C
C       1,1,nn: Bus_1_num
C       1,2,nn: Bus_2_num
C       1,3,nn: Sectn
C       2,2,nn: TCSC_index
C       2,3,nn: Z-Z_dot_R_max c
C       3,3,nn: Z-Z_dot_R_min
C       4,1,nn: TCSC_Xc_plot
C       4,2,nn: TCSC_Th_plot
C       4,3,nn: Z-Z_dot_X_max
C       5,1,nn: MW_flow_plot
C       5,2,nn: Amp_flow_plot
C       5,3,nn: Z-Z_dot_R_X_min
C       6,1,nn: MVar_flow_plot
C       6,2,nn: Z-Z_dot_plot
C       7,1,nn: Z_app_plot
C       7,2,nn: R-R_dot_plot
C       7,3,nn: R-R_dot_R_max
C       8,1,nn: Group_code
C       8,2,nn: Amps_group_code  
C       8,3,nn: R-R_dot_R_min
C       9,1,nn: Z_app_R_max
C       9,2,nn: Z_app_R_min
C       9,3,nn: R-R_dot_X_maxc
C       10,1,nn: Z_app_X_max
C       10,2,nn: Z_app_X_min
C       10,3,nn: R-R_dot_R_X_min

        i1 = 0
        do i = 1, ildatt
          l = ksrt(i)
          k = 1
          ib1 = indx2n(ilindn(1, 1, l))
          ib2 = indx2n(ilindn(1, 2, l))
          do j = 1, 8
            i1x = kndx(1,j)
            j1x = kndx(2,j)
            jl = ilindn(i1x, j1x, l)
            if (jl .lt. 1) then
              icl(j) = 0
              iop(j) = 0
            else
              icl(j) = 1
              iop(j) = jl
              noline(j) = noline(j) + 1
 
C             MBUS nn is flag that bus must be read from solution file
 
              if (mbus(ib2) .eq. 0) then
                nobus = nobus + 2
                mbus(ib2) = 1
              endif
              if (mbus(ib1) .eq. 0) then
                nobus = nobus + 2
                mbus(ib1) = 1
              endif
            endif
          enddo
          ilgd = ilgd + 1
          i1 = i1 + 1
 
C         L is the new position of plotted line I in the sort
C         I1 has the same value as I
C         Store branch ID fields
 
          lndatn(1, 1, i1) = ilindn(1, 1, l)
          lndatn(1, 2, i1) = ilindn(1, 2, l)
          lndatn(1, 3, i1) = ilindn(1, 3, l)
          lndatc(i1) = ilindc(1, l)
 
C         ILINDN(4,1,L) contains 3 pieces of information.
C         Store R,X,Z plot boundaries
 
          lndatn(4, 3, i1) = ilindn(2, 2, l)  ! TCSC index
          bndatn(8, 1, i1) = brndn(9, 1, l)
          bndatn(8, 2, i1) = brndn(9, 2, l)
          bndatn(9, 1, i1) = brndn(10, 1, l)
          bndatn(9, 2, i1) = brndn(10, 2, l)
          bndatn(8, 3, i1) = brndn(7, 3, l)
          bndatn(8, 4, i1) = brndn(8, 3, l)
          bndatn(9, 3, i1) = brndn(9, 3, l)
          bndatn(9, 4, i1) = brndn(10, 3, l)
          bndatn(8, 5, i1) = brndn(2, 3, l)
          bndatn(8, 6, i1) = brndn(3, 3, l)
          bndatn(9, 5, i1) = brndn(4, 3, l)
          bndatn(9, 6, i1) = brndn(5, 3, l)
          igrp = ilindn(8, 1, l)
          if (nogrp .ne. 0) then
 
C           Establish plot groupings
 
            do ind = 1, nogrp
              if (igrp .eq. igrup(ind)) goto 160
            enddo
          endif
          nogrp = nogrp + 1
          igrup(nogrp) = igrp
 
C         Store plot flags and destination codes
 
  160     lndatn(2, 2, i1) = icl(1)
          lndatn(2, 3, i1) = iop(1)
          lndatn(2, 4, i1) = icl(2)
          lndatn(2, 5, i1) = iop(2)
          lndatn(2, 6, i1) = icl(3)
          lndatn(2, 7, i1) = iop(3)
          lndatn(2, 8, i1) = igrp
          lndatn(3, 2, i1) = icl(4)
          lndatn(3, 3, i1) = iop(4)
          lndatn(3, 4, i1) = icl(5)
          lndatn(3, 5, i1) = iop(5)
          lndatn(3, 6, i1) = icl(6)
          lndatn(3, 7, i1) = iop(6)
          lndatn(4, 4, i1) = icl(7)
          lndatn(4, 5, i1) = iop(7)
          lndatn(4, 6, i1) = icl(8)
          lndatn(4, 7, i1) = iop(8)
        enddo
      endif
 
C     Skip over all records on power flow tape until branch data is
C     encountered
 
c dlc      read (l3) scratch
c dlc      read (l3) scratch
c dlc      lntoto = ntot
c dlc      do while (.true.)
c dlc        read (l3) scratch
c dlc        lntoto = lntoto - 100
c dlc        if (lntoto .le. 0) goto 170
c dlc      enddo
c dlc  170 lntot2 = ntot2
c dlc      if (ntot2 .ne. 0) then
c dlc        do while (.true.)
c dlc          read (l3) scratch
c dlc          lntot2 = lntot2 - 100
c dlc          if (lntot2 .le. 0) goto 180
c dlc        enddo
c dlc      endif
c dlc  180 if (ntotc .ne. 0) then
c dlc        do k = 1, 9
c dlc          read (l3) scratch
c dlc        enddo
c dlc      endif
c dlc      if (kxtot .gt. 0) read (l3) scratch
c dlc 
C     Matching identification from LINDAT table with similar
C     identification in branch data record from power flow
 
      if (ilgd .eq. 0) then
        ltoto = ltot
        if (ltoto .eq. 0) goto 230
c dlc        do while (.true.)
c dlc          read (l3) scratch
c dlc          ltoto = ltoto - 100
c dlc          if (ltoto .le. 0) goto 220
c dlc        enddo
      else
        iflag = 0
        il = 0
        il1 = 0
c dlc        do i1 = 1, ltot, 100
c dlc          i2 = min0(i1+99, ltot)
c dlc          read (l3) ((brnch(j, i), j = 1, 18), i = i1, i2)
c dlc        enddo
        iauto = 0
        do while (.true.)
          il = il + 1
          il1 = il1 + 1
          if (il1 .gt. ilgd) then
            il1 = il1 + 1
            il = il + 1
            if (il1 .gt. ilgd) goto 220
          else
            ij = lndatn(1, 1, il)
            jj = lndatn(1, 2, il)
            kpc = lndatc(il)
            ks = lndatn(1, 3, il)
            call brnchz(ij, jj, kpc, ierr, gkm, bkm, gk1, bk1, gk2,
     &       bk2)
            if (ierr .ge. 0) then
              ib1 = indx2n(lndatn(1, 1, il))
              ib2 = indx2n(lndatn(1, 2, il))
              igrp = lndatn(2, 8, il)
 
C             Store branch admittance data
 
              lndatn(1, 1, il) = ib1
              lndatn(1, 2, il) = ib2
              bndatn(5, 1, il) = gk1
              bndatn(6, 1, il) = bk1
              bndatn(3, 1, il) = gkm
              bndatn(4, 1, il) = bkm
              bndatn(7, 1, il) = gkm**2 + bkm**2
              goto 210
            elseif (imod .ne. 0) then
              do ii = 1, imod
                if (ij .eq. lnmodn(1, ii) .and. jj .eq. lnmodn(2, ii)
     &           .and. kpc .eq. lnmodc(ii)) goto 190
              enddo
              goto 200
  190         lndatn(1, 1, il) =  - ii
              lndatn(1, 2, il) =  - ii
              goto 210
            endif
          endif
  200     continue
          iflag = 1
          lndatn(1, 1, il) = 0
          lndatn(1, 2, il) = 0
          lndatn(1, 3, il) = 0
          lndatc(il) = ' '
          ildatt = ildatt - 1
  210     continue
        enddo
      endif
 
C     Reading dc line identification from ecs.
 
  220 continue

c dlc move statement to catch logic commented out
      if (ltoto .eq. 0) goto 230
      kstart = ibecs
      ibecs = ibecs - kdiff
      igecs = igecs - kdiff
      igecs1 = igecs
 
  230 continue
      if (nop .ne. 0) then
        write (outbuf, 10030) nmx
        call prtout(1)
        write (outbuf, 10040)
        call prtout(1)
10030   format ('0BUS LISTING FOR ', i4, ' BUSSES')
10040   format (
     &   '0LISTING OF INTERNAL SWING NUMBERS AND ASSOCIATED EXT    rnal
     &bus identification.'
     &   )
        ix = 0
        do i = 1, nmx
          ix = ix + 1
          busnam(ix) = newtbc(i)
          kbase = inwtb(i)
          busbas(ix) = basekv(kbase)
          intnum(ix) = i
          if (ix .eq. 5) then
            do jjj = 1, ix, 5
              kkk = min0(jjj+4, ix)
              write (outbuf, 10050) (intnum(j), busnam(j), busbas(j), j
     &         = jjj, kkk)
10050         format (1x, 5(3x, i4, 2x, a8, f6.1))
              call prtout(1)
            enddo
            ix = 0
          endif
        enddo
        if (mod(nmx, 5) .gt. 0) then
          do jjj = 1, ix, 5
            kkk = min0(jjj+4, ix)
            write (outbuf, 10050) (intnum(j), busnam(j), busbas(j), j =
     &       jjj, kkk)
            call prtout(1)
          enddo
        endif
 
        write (outbuf, 10060)
        call prtout(1)
10060   format (
     &   '0External bus names and associated internal swing numbers')
        ix = 0
        do i = 1, ntotd
          ix = ix + 1
          busnam(ix) = exnamc(i)
          kbase = ixnamn(i)
          busbas(ix) = basekv(kbase)
          intnum(ix) = indx2n(i)
          if (ix .eq. 5) then
            do jjj = 1, ix, 5
              kkk = min0(jjj+4, ix)
              write (outbuf, 10070) (busnam(j), busbas(j), intnum(j), 
     &         j = jjj, kkk)
10070         format (1x, 5(3x, a8, f6.1, 2x, i4))
              call prtout(1)
            enddo
            ix = 0
          endif
        enddo
        if (mod(ntotd, 5) .gt. 0) then
          do jjj = 1, ix, 5
            kkk = min0(jjj+4, ix)
            write (outbuf, 10070) (busnam(j), busbas(j), intnum(j), 
     &       j = jjj, kkk)
            call prtout(1)
          enddo
        endif
 
        write (outbuf, 10080) isg
10080   format ('0SWING LISTING FOR ', i4, ' GENERATORS')
        call prtout(1)
        write (outbuf, 10090)
10090   format (
     &   '0Listing of internal generator numbers and associated external
     & bus identification' )
        call prtout(1)
        call skipln(1)
        ix = 0
        do k = 1, isg
          ix = ix + 1
          busnam(ix) = nwgntc(1, k)
          kbase = nwgntn(k)
          busbas(ix) = basekv(kbase)
          genid(ix) = nwgntc(2, k)
          intnum(ix) = k
          if (ix .eq. 5) then
            do jjj = 1, ix, 5
              kkk = min0(jjj+4, ix)
              write (outbuf, 10100) (intnum(j), busnam(j), busbas(j),
     &         genid(j), j = jjj, kkk)
10100         format (1x, 5(3x, i4, 2x, a8, f6.1, 2x, a1))
              call prtout(1)
            enddo
            ix = 0
          endif
        enddo
        if (mod(isg, 5) .gt. 0) then
          do jjj = 1, ix, 5
            kkk = min0(jjj+4, ix)
            write (outbuf, 10100) (intnum(j), busnam(j), busbas(j),
     &       genid(j), j = jjj, kkk)
            call prtout(1)
          enddo
        endif
      endif
 
C     KNGENN(1,I) IS THE BUS NUMBER (INTERNAL ORDER) FOR THE ITH GENER
C     KNGENN(2,I) IS THE STARTING ECS ADDRESS AND IS NOT USED
C     KNGENC(I) IS THE GENERATOR ID FOR THE ITH GENERATOR
 
      target = 0.0
 
C     Fetch gen bus nums from hist file
 
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT2 - reading gen bus numbers from ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 81) then
        call gethisi(kngenn, irecln)
      else
        call puterr(1, 'NOUT2 - record of gen bus nums missing.')
        call prterr('E', 1)
        call erexit()
      endif
 
C     Fetch gen ID's from hist file
 
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT2 - reading gen ID''s from ', 'history file.'
     &   )
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 91) then
        call gethisc(kngenc, irecln)
      else
        call puterr(1, 'NOUT2 - record of gen ID''s missing.')
        call prterr('E', 1)
        call erexit()
      endif
 
C     READ (L8) ((KNGENN(J,I),J=1,2),KNGENC(I),I=1,ISG)
 
C     KCNSTX CONTAINS 11 ENTRIES FOR EACH GENERATOR. IND = 11*ISG
C     KCNSTX(1) = X'd   
C           (2) = DAMPING FACTOR   
C           (3) = Ra
C           (4) = Xd
C           (5) = X'q 
C           (6) = Xp
C           (7) = ESAT
C           (8) = CSAT
C           (9) = ESATX  
C          (10) = CSATX  
C          (11) = Xd"
 
C     Read gen constants from hist file
 
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT2 - reading gen constants from ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 94) then
        ind = irecln
        call gethisi(constx, irecln)
      else
        call puterr(1, 'NOUT2 - record of gen constants missing.')
        call prterr('E', 1)
        call erexit()
      endif
 
C     Note: KCNSTX equivalenced to CONSTX
C     READ (L8) IFIRST,IND,(KCNSTX(J), J=1, IND)
 
      nocur = 0
 
C     COUNT THE NUMBER OF OUTPUT VARIABLES THAT NEED TO BE SAVED
C     FROM THE SWING DATA TAPE L8
 
C     Calculated output values must fetch input gen parms
 
      do i = 1, isg
        ibus = kngenn(1, i)
        isw = mpacc(i)
        isw1 = mpa(i)
 
C       For machine or area P_acc, must have:
C       bus data,P-elec,d_freq,T_damp & P_mech
 
        if (.not. ((isw .eq. 0) .and. (isw1 .eq. 0))) then
          if (mbus(ibus) .eq. 0) then
            nobus = nobus + 2
            mbus(ibus) = 1
          endif
          mcur(i) = 1
          if (mgenp(i) .eq. 0) then
            nogenp = nogenp + 1
            mgenp(i) = 1
          endif
          if (mangp(i) .eq. 0) then
            noangp = noangp + 1
            mangp(i) = 1
          endif
          if (mtqdp(i) .eq. 0) then
            notqdp = notqdp + 1
            mtqdp(i) = 1
          endif
          if (mgov(i) .eq. 0) then
            nogov = nogov + 1
            mgov(i) = 1
          endif
        endif
        isw = mexsat(i)
 
C       For exciter saturation, must fetch E_fd
 
        if (isw .ne. 0) then
          if (mvfld(i) .eq. 0) then
            novfl = novfl + 1
            mvfld(i) = 1
          endif
        endif
 
C       For field current, must fetch E_fd
 
        if (mfcur(i) .ne. 0) then
          if (mvfld(i) .eq. 0) then
            novfl = novfl + 1
            mvfld(i) = 1
          endif
        endif
        isw = mtqdp(i)
 
C       For damping torque, must fetch must fetch d_freq
 
        if (isw .ne. 0) then
          if (mangp(i) .eq. 0) then
            noangp = noangp + 1
            mangp(i) = 1
          endif
        endif
        isw = mfsat(i) + mgenp(i) + mgenq(i)
 
C       For field_sat, P_gen or Q_gen, must fetch terminal V & I
 
        if (isw .ne. 0) then
          if (mbus(ibus) .eq. 0) then
            nobus = nobus + 2
            mbus(ibus) = 1
          endif
          mcur(i) = 1
        endif
        isw = mepq(i) + mfcur(i)
 
C       For field current, fetch V_term, I_term, and rotor angle.
 
        if (isw .ne. 0) then
          if (mbus(ibus) .eq. 0) then
            nobus = nobus + 2
            mbus(ibus) = 1
          endif
          mcur(i) = 1
          if (mangl(i) .eq. 0) then
            noangl = noangl + 1
            mangl(i) = 1
          endif
        endif
 
C       Gen injectn current needed for calcs but not itself plotted
 
        if (mcur(i) .ne. 0) nocur = nocur + 1
      enddo
 
C     OPEN MASS STORAGE FILES AND INITIALIZE ECS STORAGE ADDRESSES
 
C     THIS IS THE BEGINNING OF THE CODE THAT UTILIZES ECP.
C     THEREFORE, WE START AT LOCATION 1 BY SETTING KUSB1 =1.

C     Rotor angle
 
      ksub1 = 1
 
C     Rotor frequency deviation
 
      ksub2 = ksub1 + noangl
      if (noangl .lt. maxcyc) ksub2 = ksub1 + maxcyc
      if (noangl .eq. 0) ksub2 = ksub1
 
C     Field voltage
 
      ksub4 = ksub2 + noangp
      if (noangp .lt. maxcyc) ksub4 = ksub2 + maxcyc
      if (noangp .eq. 0) ksub4 = ksub2
 
C     Governor power
 
      ksub7 = ksub4 + novfl
      if (novfl .lt. maxcyc) ksub7 = ksub4 + maxcyc
      if (novfl .eq. 0) ksub7 = ksub4
 
C     Terminal current
 
      ksub10 = ksub7 + nogov
      if (nogov .lt. maxcyc) ksub10 = ksub7 + maxcyc
      if (nogov .eq. 0) ksub10 = ksub7
 
C     PSS output
 
      ksub12 = ksub10 + nocur
      if (nocur .lt. maxcyc) ksub12 = ksub10 + maxcyc
      if (nocur .eq. 0) ksub12 = ksub10
 
C     Bus voltage real
 
      ksub13 = ksub12 + nosup
      if (nosup .lt. maxcyc) ksub13 = ksub12 + maxcyc
      if (nosup .eq. 0) ksub13 = ksub12
      nobus2 = nobus/2
 
C     Bus voltage imag
 
      ksb13c = ksub13 + nobus2
      if (nobus2 .lt. maxcyc) ksb13c = ksub13 + maxcyc
      if (nobus2 .eq. 0) ksb13c = ksub13
 
C     DC data
 
      kdc1 = ksb13c + nobus2
      kdc2 = kdc1 + maxcyc + 1
      kdc3 = kdc2 + maxcyc + 1
      kdc4 = kdc3 + maxcyc + 1
      kdc5 = kdc4 + maxcyc + 1
      kdc6 = kdc5 + maxcyc + 1
      kdc7 = kdc6 + maxcyc + 1
 
C     Start of volatile area
 
      kwork = kdc7 + maxcyc + 1
      if (ldc .eq. 0 .and. ldc1 .eq. 0) kwork = kdc1
      knbold = kwork
      knbnew = knbold + ntotd
      kzij = knbnew + ntot
      kpij = kzij + kmbrch
      ktbl = kpij + kmbrch
      kkk = ktbl + kmbrch
      kkk1 = kkk + kmbus
      klecs = kkk1 + kmbus + 1
      kemout = klecs
c dlc      read (l3) (nbnew(i), i = 1, ntot)
c dlc      read (l3) (nbold(i), i = 1, ntotd)
c dlc      read (l3) skip
c dlc      read (l3) skip
c dlc      read (l3) skip
c dlc      if (jphno .ne. 0) read (l3) skip
 
C     READING SECOND ENTRY IN KK TABLE
 
c dlc      read (l3) ((kka(i, j), i = 1, 2), j = 1, ntot)
 
C     Calculate impedance magnitude and angle for all branches that
C     exist in low to high alpha order.
C     MTBL = packed external power flow numbers.
C     ZIJ = impedance magnitude.
C     PIJ = polar angle in degrees.
 
c dlc      kecsy = kecsym
c dlc      kecst = klecs - 1
c dlc      is = 1
c dlc      if = 4000
c dlc      do while (kecsy .gt. 4000)
c dlc        read (l3) (buf4k(i), i = is, if)
c dlc        klecs = klecs + 4000
c dlc        kecsy = kecsy - 4000
c dlc        is = is + 4000
c dlc        if = if + 4000
c dlc      enddo
c dlc      read (l3) (buf4k(i), i = is, kecsym)
c dlc      kecmax = klecs + kecsy
c dlc      kecsy = kecsym
      it = 0
      if (nop .le. 0) then
        do i = 1, ntot

C         WE ARE NOT SURE IF KP AND KK ARE CORRECT.
C         THE REVERSE ASSIGNMENT MAY BE NEEDED.

          iold = nbold(i)
          if (iold .le. ntot) then
            kp = kka(1, iold)
            if (kp .ge. 0) then
              kk = kka(2, iold)
 
C             SKIP IF A PASSIVE DC BUS
 
              if (kk .gt. 12) then
                do jj = kp + 12, kp + kk - 1, 3
                  iwk = ibuf4k(jj)
                  if (iwk .le. nmx) then
                    if (iwk .ge. 0) then
                      j = nbnew(iwk)
                      if (j .gt. i) then
                        gij = buf4k(jj+1)
                        bij = buf4k(jj+2)
                        if (gij .eq. 0.0 .and. bij .eq. 0.0) 
     &                    bij = 0.001
                        it = it + 1
                        mtbl(1, it) = i
                        mtbl(2, it) = j
                        mtbl(3, it) = 0
                        zij(it) = 1.0/sqrt(gij*gij+bij*bij)
                        pij(it) = degrad*atan2(bij, -gij)
                      endif
                    endif
                  endif
                enddo
              endif
            endif
          endif
        enddo
        itbld = it
      endif
 
 
C     Building an index table to rearrange the internal swing bus
C     names in NEWTAB in alpha order
 
      do i = 1, nmxp
        ixnamn(i) = innm8a(exnamc(i), ixnamn(i))
      enddo
      jnear = 0
      if (nop .le. 0) then
        do i1 = 1, itbld
          inear = mtbl(1, i1)
          ifar = mtbl(2, i1)
          mtbl(1, i1) = ixnamn(inear)
          mtbl(2, i1) = ixnamn(ifar)
        enddo
      endif
      target = 0.0
      icoun1 = 0
      icoun2 = 0
 
C     Read bus data for dc lines.
C     NDC1 = the number of dc buses
C     DCNME1 = bus name
C     IDCBK1 = base kv code
C     IMDOC = modulation code
 
C     Read 2-term DC bus names.  If no DC at all, skip all history
C     records down to TCSC (type 121).
 
      any_dc = (ldc .gt. 0 .or. ldc1 .gt. 0)
      call gethisw(irectp)
      if (any_dc) then
 
C       IF (LDC.EQ.0 .AND. LDC1.EQ.0) GO TO 4640
C       if there are some DC links
C       read record of DC counters
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading DC counters from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        if (irectp .eq. 97) then
          call gethisi(ndc1, 1)
          call gethisi(ndc2, 1)
        else
          call puterr(1, 'NOUT2 - record of DC counters missing.')
          call prterr('E', 1)
          call erexit()
        endif
 
C       Read record of 2-term DC names
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading 2-term DC bus names from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        if (irectp .eq. 103) then
          call gethisc(dcnme1, irecln)
        else
          call puterr(1,
     &     'NOUT2 - record of 2-term DC bus names missing.')
          call prterr('E', 1)
          call erexit()
        endif
 
C       Read record of 2-term DC base KV codes
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading 2-term DC base KV codes from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        if (irectp .eq. 105) then
          call gethisi(idcbk1, irecln)
        else
          call puterr(1,
     &     'NOUT2 - record of 2-term DC base KV''s missing.')
          call prterr('E', 1)
          call erexit()
        endif
 
C       Read 2-term DC modulation codes
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading 2-term DC modultn codes from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        if (irectp .eq. 109) then
          call gethisi(imodc, irecln)
        else
          call puterr(1,
     &     'NOUT2 - record of 2-t DC modulation codes missing.')
          call prterr('E', 1)
          call erexit()
        endif
 
C       READ (L8) NDC1,NDC2,((DCNME1(I),I=1,NDC1),
C       1  (IDCBK1(I),I=1,NDC1),(IMODC(I),I=1,NDC1))
 
        ndc3 = 2*ndc2
 
C       Read in multi-term DC constant stuff (if none skip down to
C       first time record (type 121))
 
        call gethisw(irectp)
        if (ndc2 .le. 0) then
          do while (irectp .gt. 0 .and. irectp .lt. 121)
 
C           Not at time record yet, so skip to start of next record
 
            call gethisi(irectp, 1)
            call gethisi(idesc, 1)
            call gethisi(irecln, 1)
            call skiphis(irecln)
            call gethisw(irectp)
          enddo
        else
 
C         Read in m-term DC bus names
 
          call gethisi(irectp, 1)
          call gethisi(idesc, 1)
          ndc3 = idesc/256
          call gethisi(irecln, 1)
          if (debug) then
            call dbgeko2('NOUT2 - reading m-term DC bus names from ',
     &       'history file.')
            call dbgwri('  IRECTP /record type/ = ', irectp)
            call dbgwri('  IDESC /rec descrip/  = ', idesc)
            call dbgwri('  IRECLN /rec length/  = ', irecln)
          endif
          if (irectp .eq. 115) then
            call gethisc(dcnme2, irecln)
          else
            call puterr(1,
     &       'NOUT2 - record of m-term DC bus names missing.')
            call prterr('E', 1)
            call erexit()
          endif
 
C         Read in m-term DC base KV codes
 
          call gethisi(irectp, 1)
          call gethisi(idesc, 1)
          call gethisi(irecln, 1)
          if (debug) then
            call dbgeko2(
     &       'NOUT2 - reading m-term DC base KV codes from ',
     &       'history file.')
            call dbgwri('  IRECTP /record type/ = ', irectp)
            call dbgwri('  IDESC /rec descrip/  = ', idesc)
            call dbgwri('  IRECLN /rec length/  = ', irecln)
          endif
          if (irectp .eq. 117) then
            call gethisi(idcbk2, irecln)
          else
            call puterr(1,
     &       'NOUT2 - record of m-term DC base KVs missing.')
            call prterr('E', 1)
            call erexit()
          endif
        endif
 
C       IF (NDC2 .GT. 0) READ (L8) ((DCNME2(I), I = 1,NDC3),
C       1  (IDCBK2(I), I=1,NDC3))
C       STORE DC BRANCH TABLE IN ECS TEMPORARILY AFTER KECMAX
 
        ndc3 = ndc3 + ndc1
        if (iddat .ne. 0) then
 
C         LOCATE THE DC BUS OUTPUT CARD IN THE R1 RECORD OF SWING FILE
 
          do j = 1, iddat
            bus1 = idctlc(1, j)
            dcbus1 = idctlc(1, j)
            bus2 = idctlc(2, j)
            dcbus2 = idctlc(2, j)
            k1 = idctln(2, 1, j)
            k2 = idctln(2, 2, j)
            idcxn(1, j) = 0
            idcxn(2, j) = 0
            idcxn(3, j) = 0
            idcgrp(j) = 0
            idclin(j) = idctln(1, 5, j)
            if (idclin(j) .eq. 1) then
 
C             LOCATE DC LINE OUTPUT CARD IN R2 RECORD OF SWING FILE
 
              do i = 1, ndc2
                k = 2*i
                ik = k - 1
                kb1 = idctln(1, 1, j)
                kb2 = idctln(1, 2, j)
                if (dcnme2(ik) .eq. bus1 .and. dcnme2(k) .eq. bus2
     &           .and. idcbk2(ik) .eq. kb1 .and. idcbk2(k) .eq. kb2)
     &           goto 250
                if (dcnme2(ik) .eq. bus2 .and. dcnme2(k) .eq. bus1
     &           .and. idcbk2(ik) .eq. kb2 .and. idcbk2(k) .eq. kb1)
     &           goto 240
              enddo
              b1 = basekv(kb1)
              b2 = basekv(kb2)
              write (errbuf(1), 10110) bus1, b1, bus2, b2
              call prterr('W', 1)
10110         format ('0 Output request branch ( ', a8, 1x, f5.1, 5x,
     &         a8, 1x, f5.1,
     &         ' ) is not in dc branch table. Request ignored')
              goto 300
  240         kdum = ik
              ik = k
              k = kdum
 
C             STORE BRANCH OUTPUT INFO IN IDINDN,IDCXN,IDCGRP
 
  250         continue
              do m = 1, 10
                idindn(m, j) = idctln(m, 3, j)
              enddo
              idcxn(1, j) = ik
              idcxn(2, j) = k
              idcxn(3, j) = i
              if (ik .ne. 0 .and. k .ne. 0 .and. i .ne. 0) idcbr =
     &         idcbr + 1
              idcgrp(j) = idctln(1, 4, j)
              if ((idcbs+idcbr) .ge. 21) goto 330
            else
 
C             PROCESS DC TERMINAL OUTPUT REQUESTS
 
              if (k1 .gt. 0) then
                kb1 = idctln(1, 1, j)
                b1 = basekv(kb1)
              endif
              if (k2 .gt. 0) then
                kb2 = idctln(1, 2, j)
                b2 = basekv(kb2)
              endif
              j1l = 0
              j1r = 0
              if (k1 .gt. 0) then
                do j1 = 1, ndc1
                  if (.not. ((dcnme1(j1) .ne. bus1) .or. (idcbk1(j1)
     &             .ne. kb1))) goto 260
                enddo
                if (j1l .le. 0) then
 
C                 PRINT OUT BUS NAMES THAT ARE NOT PART OF DC BUSSES
 
                  write (errbuf(1), 10120) bus1, b1
                  call prterr('W', 1)
10120             format ('0  THE BUS NAME ', a8, 2x, f5.1,
     &             ' IS NOT A DC BUS.', ' IT WILL BE IGNORED.')
                endif
                goto 270
  260           j1l = j1
                idcbs = idcbs + 1
 
C               IDCBS IS NUMBER OF 'GOOD' TERMNL OUTPUT REQUESTS.  IDCBR
C               NUMBER OF 'GOOD' BRANCH OUTPUT REQUESTS.  IDCBS+IDCBR .L
C               DUE TO DIMENSION LIMITS OF ARRAYS DCANG, DCI, DCV.
 
                if ((idcbs+idcbr) .ge. 21) goto 310
              endif
  270         if (k2 .gt. 0) then
                do j1 = 1, ndc1
                  if (.not. ((dcnme1(j1) .ne. bus2) .or. (idcbk1(j1)
     &             .ne. kb2))) goto 280
                enddo
                if (j1r .le. 0) then
                  write (errbuf(1), 10120) bus2, b2
                  call prterr('E', 1)
                endif
                goto 290
  280           j1r = j1
                idcbs = idcbs + 1
                if ((idcbs+idcbr) .ge. 21) goto 320
              endif
 
C             STORE TERMINAL OUTPUT INFO IN IDINDN,IDCXN,IDCGRP
 
  290         continue
              do m = 1, 10
                idindn(m+2, j) = idctln(m, 3, j)
              enddo
              idindn(1, j) = idctln(2, 1, j)
              idindn(2, j) = idctln(2, 2, j)
              do m = 1, 8
                idindn(12+m, j) = idctln(m+2, 2, j)
              enddo
              if (j1l .ne. 0) idcmd1(j) = imodc(j1l)
              if (j1r .ne. 0) idcmd2(j) = imodc(j1r)
              idcxn(1, j) = j1l
              idcxn(2, j) = j1r
              idcxn(3, j) = 0
              idcgrp(j) = idctln(1, 4, j)
            endif
  300       continue
          enddo
          goto 350
  310     idadt = j - 1
          goto 340
  320     idadt = j - 1
          goto 340
  330     idadt = j - 1
 
C         WHEN IDCBS+IDCBR EXCEEDS 20 IDDAT IS REDUCED SUCH THAT
C         (IDCBS+IDCBR) .LE. 20
 
  340     write (errbuf(1), 10130) iddat, idadt
          call prterr('E', 1)
10130     format ('0 Output cards have been reduced from ', i3, ' to ',
     &     i3, ' to limit total output requests to 20')
          iddat = idadt
  350     continue
        endif
      endif

      call gethisw(irectp)  ! Get value of next pointer w/o advancing

      do while (irectp .gt. 0 .and. irectp .lt. 121)
 
C       Align to start of TCSC record
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        call skiphis(irecln)
        call gethisw(irectp)    
      enddo

C     Read TCSC counter
 
      call gethisi(irectp, 1)
      call gethisi(idesc, 1)
      call gethisi(irecln, 1)
      if (debug) then
        call dbgeko2('NOUT2 - reading TCSC counter ',
     &   'history file.')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      if (irectp .eq. 121) then
        call gethisi(iznmax, 1)
      else
        call puterr(1,
     &   'NOUT2 - record of TCSC counter missing.')
        call prterr('E', 1)
        call erexit()
      endif
      if (iznmax .gt. 0) then
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading TCSC data ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        if (irectp .eq. 125) then
          call gethisi(ksrt, irecln)
        else
          call puterr(1,
     &     'NOUT2 - record of TCSC data missing.')
          call prterr('E', 1)
          call erexit()
        endif
        do i = 1, iznmax
          iznbus(i) = ksrt(5*i-4)
          jznbus(i) = ksrt(5*i-3)
          iznpar(i) = char (ksrt(5*i-2))
          iznsec(i) = ksrt(5*i-1)
          iranityp(i) = ksrt(5*i)
        enddo
      endif
 
C     START OF LOOP TO READ MACHINE, BUS, AND DC QUANTITIES FOR
C     EACH TIME STEP
 
      ktbus = 0
 
C     READ GENERATOR OUTPUT VARIABLES FROM SOLUTION HISTORY FILE
C     ANGL          INTERNAL MACHINE ANGLE IN RADIANS
C     ANGP2         MACHINE FREQUENCY DEVIATION IN RADIANS/CYCLE
C     VFLDT(1,I)    MACHINE FIELD VOLTAGE
C     VFLDT(2,I)    MACHINE FIELD CURRENT
C     GOVP          MACHINE MECHANICAL POWER IN PU
C     PELECT        MACHINE ELECTRICAL POWER IN PU
C     EPQ           GENERATOR FLUX IN PU
C     CUR(1,I)      REAL PART OF MACHINE INJECTION CURRENT
C     CUR(2,I)      IMAGINARY PART OF MACHINE INJECTION CURRENT
C     SUP(1,I)      EXCITER REGULATOR OUTPUT
C     SUP(2,I)      EXCITER SUPPLEMENTAL SIGNAL OUTPUT
C     BUSMW         BUS LOADS (MW) IN PU
C     BUSVAR        BUS LOADS (MVAR) IN PU
C     SPARPT        SPARE POINTS
C     EYR           BUS VOLTAGES (REAL) IN PU
C     EYI           BUS VOLTAGES (IMAGINARY) IN PU
C     DCBUS         D-C BUS DATA
C     DCLINE        D-C LINE DATA
C     TCSC          TCSC REACTANCE (OHMS) AND SYNTHENTIC ANGLE (DEGREE)
 
      do while (.true.)
        target = 0.0
 
C       Have all constants.  Now reading time records
C       Check for end-of file
 
        call gethisw(irectp)
        if (irectp .le. 0) goto 360
 
C       Read time itself
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading simulation time from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        if (irectp .eq. 130) then
          call gethisf(to, 1)
 
C         Do a skip to next b-o-r in case future versions of hist file
C         have more data in this record
 
          if (irecln .gt. 1) call skiphis(irecln-1)
          if (debug) call dbgwrf('  TO /simul time/ = ', to)
        else
          call puterr(1, 'NOUT2 - record of simulation time missing.')
          call prterr('E', 1)
          call erexit()
        endif
 
C       Negate flag indicating if we have all data for this time step
 
        complet = .false.
 
C       Read gen rotor angles (type 134)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading gen rotor angles from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call gethisf(angl, irecln)
 
C       Read gen rotor freq dev (type 138)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading gen frequency devs from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call gethisf(angp2, irecln)
 
C       Read gen field volts & currents (type 142)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading gen field volt & curr from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call gethisf(vfldt, irecln)
 
C       Read gen mechanical powers (type 146)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading gen mechanical powers from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call gethisf(govp, irecln)
 
C       Read gen electrical powers (type 150)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading gen elctrical powers from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call gethisf(pelec, irecln)
 
C       Read gen flux (type 10150)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading gen flux ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call gethisf(epq, irecln)
 
C       Read gen terminal currents (type 154)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading gen terminal currents from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        call gethisf(cur, irecln)
 
C       Read gen exc voltage regultr outputs (type 158)
C       NOTE:  V_reg_out and PSS_out are intermixed as SUP[1,LA] and
C       SUP[2,LA]
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading voltage reg outputs from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        laa = 0
        do while (laa .lt. irecln)
          laa = laa + 1
          call gethisf(sup(1, laa), 1)
        enddo
 
C       Read gen PSS outputs (type 162)
 
        call gethisi(irectp, 1)
        call gethisi(idesc, 1)
        call gethisi(irecln, 1)
        if (debug) then
          call dbgeko2('NOUT2 - reading PSS outputs from ',
     &     'history file.')
          call dbgwri('  IRECTP /record type/ = ', irectp)
          call dbgwri('  IDESC /rec descrip/  = ', idesc)
          call dbgwri('  IRECLN /rec length/  = ', irecln)
        endif
        if (irectp .ne. 162) then
          call puterr(1,
     &     'NOUT2 - gen simulation history data corupted.')
          call prterr('E', 1)
          call erexit()
        endif
        laa = 0
        do while (laa .lt. irecln)
          laa = laa + 1
          call gethisf(sup(2, laa), 1)
        enddo
        if (debug) call dbgeko(
     &   'NOUT2 - finished reading last of gen data.')
 
C       READ(L8, END=5120) IFIRST, JFIRST, TO, (ANGL(I), ANGP2(I),
C       $        (VFLDT(J, I), J = 1, 2), GOVP(I), (CUR(J, I), J = 1, 2)
C       $        (SUP(J, I), J = 1, 2), I = 1, ISG)
 
C       START READING SOLUTION HISTORY FILE AT TIME STEP TSTRT
 
C       If we're not yet at the plot start time, skip to the next new
C       time record (type 130)
 
        if (to .lt. tstrt) then
          irectp = 129
          do while (irectp .gt. 0 .and. irectp .ne. 130)
 
C           Read solution history file only to time step TMAXP
 
            call gethisi(irectp, 1)
            call gethisi(idesc, 1)
            call gethisi(irecln, 1)
            call skiphis(irecln)
            call gethisw(irectp)
            if (irectp .eq. -1) goto 360
          enddo
        elseif (to .gt. tmaxp) then
          goto 360
        else
 
C         IF (IFIRST.NE.5) GO TO 5120
 
          icoun1 = icoun1 + 1
          if (icoun1 .eq. 1) then
            do i = 1, isg
              vfdint(i) = vfldt(1, i)
              angint(i) = angl(i)
              if (iref .ne. 0) angint(i) = angint(i) - angl(iref)
            enddo
          endif
 
C         CALL WGFCAL TO CALCULATE WORST GENERATOR FREQUENCY DEVIATIONS
 
          if (iwgfsw .gt. 0) call wgfcal(to)
 
C         CALL WGSCAL TO CALCULATE WORST GENERATOR PSS DEVIATIONS
 
          if (iwgssw .gt. 0) call wgscal(to)
 
C         CALL WGVCAL TO CALCULATE WORST GENERATOR FIELD VOLT DEVIATIONS
 
          if (iwgvsw .gt. 0) call wgvcal(to, vfdint)
 
C         CALL WGACAL TO CALCULATE WORST GENERATOR ANGLE DEVIATIONS
 
          if (iwgasw .gt. 0) call wgacal(to, angint)
          if (iref .ne. 0) refag(icoun1) = angl(iref)*degrad
          ktang = 0
          ktanp = 0
          ktvfl = 0
          ktgov = 0
          ktcur = 0
          ktsup = 0
 
C         Store needed gen parms into named arrays for this step
 
          do i = 1, isg
            if (ktang .ne. noangl) then
              if (mangl(i) .ne. 0) then
                ktang = ktang + 1
                anglwk(ktang) = angl(i)
                if (angl(i) .eq. -40000.) anglwk(ktang) =  - 174.516
              endif
            endif
            if (ktanp .ne. noangp) then
              if (mangp(i) .ne. 0) then
                ktanp = ktanp + 1
                angpwk(ktanp) = angp2(i)
              endif
            endif
            if (ktvfl .ne. novfl) then
              if (mvfld(i) .ne. 0) then
                ktvfl = ktvfl + 1
                vfldwk(1, ktvfl) = vfldt(1, i)
                vfldwk(2, ktvfl) = vfldt(2, i)
              endif
            endif
            if (ktgov .ne. nogov) then
              if (mgov(i) .ne. 0) then
                ktgov = ktgov + 1
                govwk(1, ktgov) = govp(i)
                govwk(2, ktgov) = epq(i)
              endif
            endif
            if (ktcur .ne. nocur) then
              if (mcur(i) .ne. 0) then
                ktcur = ktcur + 1
                curwk(1, ktcur) = cur(1, i)
                curwk(2, ktcur) = cur(2, i)
              endif
            endif
            if (ktsup .ne. nosup) then
              if (msup(i) .ne. 0) then
                ktsup = ktsup + 1
                supwk(1, ktsup) = sup(1, i)
                supwk(2, ktsup) = sup(2, i)
              endif
            endif
          enddo
 
C         Store data from solution into 2-D SIMMS array
 
          if (ktang .gt. 0) call writmp (1, anglwk, ktang, 1)
          if (ktanp .gt. 0) call writmp (2, angpwk, ktanp, 1)
          if (ktvfl .gt. 0) call writmp (4, vfldwk, ktvfl*2, 1)
          if (ktgov .gt. 0) call writmp (7, govwk, ktgov*2, 1)
          if (ktcur .gt. 0) call writmp (10, curwk, ktcur*2, 1)
          if (ktsup .gt. 0) call writmp (12, supwk, ktsup*2, 1)
          i1 = icoun1 - 1
 
C         Store data pointers (returned by WRITMP) into 1-D ECP array
 
C         IF (NOANGL.NE.0) CALL RITECP (MSUB1(2),KSUB1+I1,1)
          if (noangl .ne. 0) iecsp(ksub1+i1) = msub1(2)
C         IF (NOANGP.NE.0) CALL RITECP (MSUB2(2),KSUB2+I1,1)
          if (noangp .ne. 0) iecsp(ksub2+i1) = msub2(2)
C         IF (NOVFL.NE.0) CALL RITECP (MSUB4(2),KSUB4+I1,1)
          if (novfl .ne. 0) iecsp(ksub4+i1) = msub4(2)
C         IF (NOGOV.NE.0) CALL RITECP (MSUB7(2),KSUB7+I1,1)
          if (nogov .ne. 0) iecsp(ksub7+i1) = msub7(2)
C         IF (NOCUR.NE.0) CALL RITECP (MSUB10(2),KSUB10+I1,1)
          if (nocur .ne. 0) iecsp(ksub10+i1) = msub10(2)
          if (nosup .ne. 0) iecsp(ksub12+i1) = msub12(2)
C         IF(NOSUP.NE.0)CALL RITECP(MSUB12(2),KSUB12+I1,1)

          junk = 1 + 6*isg
          junk7 = 1 + 7*isg
          junk8 = 1 + 8*isg
          junk9 = 1 + 9*isg
          icoun2 = icoun2 + 1
 
C         Fatal error if asked for too many time steps
 
C         if (icoun2 .gt. maxstp) then
C         write (errbuf,'(a,i4,a)') ' NOUT2 - can''t plot more than ',
C         +    maxstp,' time steps.'
C         call prterr ('E',1)
C         endif
 
C         READ BUS LOAD FROM SWING TAPE IF THEY ARE AVAILABLE
 
          call gethisw(irectp)
          if (debug) then
            call dbgeko('NOUT2 - expecting a bus load (166) record')
            call dbgwri('  irectp = ', irectp)
          endif
          if (irectp .eq. 166) then
 
C           Have bus loads to read (real)
 
            call gethisi(irectp, 1)
            call gethisi(idesc, 1)
            call gethisi(irecln, 1)
            nmx = irecln
            if (debug) then
              call dbgeko2('NOUT2 - reading bus loads (real) from ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            call gethisf(bmw, irecln)
 
C           Bus loads (reactive)
 
            call gethisi(irectp, 1)
            call gethisi(idesc, 1)
            call gethisi(irecln, 1)
            if (debug) then
              call dbgeko2('NOUT2 - reading bus loads (reactive) from '
     &         , 'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            call gethisf(bmvar, irecln)
 
C           IF (JFIRST .EQ. JUNK7) THEN
C           READ (L8, END = 5120) (BMW(I), BMVAR(I), I = 1,NMX)
 
            mwkt = 0
            mvarkt = 0
            do ktrr = 1, nmx
              if (jbusdt(4, ktrr) .gt. 0) then
                mwkt = mwkt + 1
                if (mwkt .le. itotmw) busmw(mwkt, icoun2) = bmw(ktrr)
              endif
 
              if (jbusdt(5, ktrr) .gt. 0) then
                mvarkt = mvarkt + 1
                if (mvarkt .le. itotvr) busvar(mvarkt, icoun2) = bmvar
     &           (ktrr)
              endif
            enddo
          elseif (to .eq. 0.0) then
            call puterr(1, 'NOUT2 - No bus loads in this history file.'
     &       )
            call prterr('W', 1)
          endif
 
C         READ SPARE DATA POINTS IF THEY EXIST AND WRITE TO DATA TABLE
 
          call gethisw(irectp)
          if (debug) then
            call dbgeko('NOUT2 - expecting a spare point (258) record')
            call dbgwri('  irectp = ', irectp)
          endif
          if (irectp .eq. 258) then
 
C           Have spare points to read
 
            call gethisi(irectp, 1)
            call gethisi(idesc, 1)
            call gethisi(irecln, 1)
            ispknt = irecln
            if (debug) then
              call dbgeko2('NOUT2 - reading spare points from ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            call gethisf(sparpt, irecln)
 
C           IF(JFIRST .EQ. JUNK8)THEN
C           READ(L8, END = 5120) (SPARPT(I),I=1,MAXSP)
 
C           Transfer spare point data to SPDATA, recompute counter
C           ISPKNT.
 
            if (ispknt .gt. 0) then
              do itrr = 1, MAXSP
                jtrr = ispcde(itrr)
                if (jtrr .gt. 0) then
                  ispknt = jtrr
                  spdata(icoun2, jtrr) = sparpt(jtrr)
                endif
              enddo
            endif
          elseif (to .eq. 0.0) then
            call puterr(1,
     &       'NOUT2 - No spare points in this history file.')
            call prterr('W', 1)
          endif
 
C         READ BUS VOLTAGES FROM SWING TAPE
C         EYR IS THE REAL PORTION AND EYI IS THE IMAGINARY PORTION
 
C         Read bus voltages (real) (type 174)
 
          call gethisi(irectp, 1)
          call gethisi(idesc, 1)
          call gethisi(irecln, 1)
          nmx = irecln
          if (debug) then
            call dbgeko2('NOUT2 - reading bus voltages (real) from ',
     &       'history file.')
            call dbgwri('  IRECTP /record type/ = ', irectp)
            call dbgwri('  IDESC /rec descrip/  = ', idesc)
            call dbgwri('  IRECLN /rec length/  = ', irecln)
          endif
          if (irectp .eq. 174) then
            call gethisf(eyr, irecln)
          else
            call puterr(1, 'NOUT2 - record of bus voltages missing.')
            call prterr('E', 1)
            call erexit()
          endif
 
C         Read bus voltages (reactive) (type 178)
 
          call gethisi(irectp, 1)
          call gethisi(idesc, 1)
          call gethisi(irecln, 1)
          if (debug) then
            call dbgeko2('NOUT2 - reading bus voltages (imag) from ',
     &       'history file.')
            call dbgwri('  IRECTP /record type/ = ', irectp)
            call dbgwri('  IDESC /rec descrip/  = ', idesc)
            call dbgwri('  IRECLN /rec length/  = ', irecln)
          endif
          call gethisf(eyi, irecln)
 
C         READ (L8, END=5120) (EYR(I),EYI(I),I=1,NMX)
 
          t(icoun2) = to
          if (icoun2 .eq. 1) then
            do itr = 1, nmx
              eyrint(itr) = eyr(itr)
              eyiint(itr) = eyi(itr)
            enddo
          endif
          i1 = icoun2 - 1
          ktbus =  - 1
          ts = abs(to)
 
C         CALL WSTVLT TO SAVE THE 20 LOWEST BUS VOLTAGES
 
          call wstvlt(to)
 
C         CALL WSTFEQ TO SAVE THE 20 LOWEST BUS FREQUENCIES
 
          call wstfeq(to)
 
C         CALL HIVOLT TO SAVE THE 20 HIGHEST BUS VOLTAGES
 
          call hivolt(to)
c
c         Call VFHIST to store voltage dip information
c
          call vfhist (2, to)

          tlast = to
 
C         CALL BADZ IF ABNORMAL APPARENT IMPEDANCE AND BAD VOLTAGE
C         OUTPUT IS REQUESTED
 
          if (nopz .le. 0) call badz(ts)
 
C         STORE REQUESTED BUS VOLTAGES IN DAT13
 
          do iz = 1, nmx
            if (mbus(iz) .ne. 0) then
              ktbus = ktbus + 2
              dat13(ktbus) = eyr(iz)
              dat13(ktbus+1) = eyi(iz)
            endif
          enddo
          if (nobus .gt. 0) call writmp (13, dat13, nobus, 1)
 
C         IF (NOBUS2 .NE. 0) CALL RITECP (MSUB13(2),KSUB13+I1,1)
 
          if (nobus2 .ne. 0) iecsp(ksub13+i1) = msub13(2)
 
C         OBTAIN DC INFORMATION FROM SWING TAPE FOR EACH TIME STEP
C         AND WRITING THIS INFORMATION TO MASS STORAGE AND THERE ADDRESS
C         TO E.C.S. THERE ARE 9 OUTPUT QUANTITES FOR EACH TERMINAL
 
          target = 0.0
 
C         Skip to TCSC counter record if there are no DC links.
 
          call gethisw(irectp)
          if (irectp .eq. -1) goto 360
          if (any_dc) then
 
C           IF (LDC.EQ.0 .AND. LDC1.EQ.0) GO TO 5105
 
C           There is some DC stuff
 
            call gethisi(irectp, 1)
            call gethisi(idesc, 1)
            call gethisi(irecln, 1)
            ir3 = irecln
            if (debug) then
              call dbgeko2('NOUT2 - reading DC bus data from ',
     &         'history file.')
              call dbgwri('  IRECTP /record type/ = ', irectp)
              call dbgwri('  IDESC /rec descrip/  = ', idesc)
              call dbgwri('  IRECLN /rec length/  = ', irecln)
            endif
            if (irectp .eq. 182) then
              call gethisf(work, irecln)
            else
              call puterr(1, 'NOUT2 - record of DC bus data missing.')
              call prterr('E', 1)
              call erexit()
            endif
            ir3 = 9*ndc1
            ir4 = 3*ndc2
            ldco = ir3 + ir4
 
C           READ IN BRANCH OUTPUT QUANTITIES FROM SWING FILE
C           THERE ARE NINE ENTRIES FOR EACH DC TERMINAL
C           WORK(1)  COSINE OF FIRING ANGLE
C           (2)  DC LINE CURRENT
C           (3)  DC TERMINAL VOLTAGE
C           (4)  EXTINCTION ANGLE
C           (5)  MODULATION SIGNAL
C           (6)  V ALPHA FROM CURRENT REGULATOR
C           (7)  V' ALPHA FROM CURRENT REGULATOR
C           (8) AND (9) ARE SPARE POINTS NOT USED PRESENTLY
 
C           READ (L8, END = 5120) (WORK(I),I=1,IR3)
 
C           Read DC branch data if expecting any
 
            call gethisw(irectp)
            if (ndc2 .ne. 0) then
              if (irectp .eq. 186) then
                call gethisi(irectp, 1)
                call gethisi(idesc, 1)
                call gethisi(irecln, 1)
                ir4 = irecln
                if (debug) then
                  call dbgeko2('NOUT2 - reading DC branch data from ',
     &             'history file.')
                  call dbgwri('  IRECTP /record type/ = ', irectp)
                  call dbgwri('  IDESC /rec descrip/  = ', idesc)
                  call dbgwri('  IRECLN /rec length/  = ', irecln)
                endif
                call gethisf(work(ir3+1), irecln)
              else
                call puterr(1,
     &           'NOUT2 - record of DC branch data missing.')
                call prterr('E', 1)
                call erexit()
              endif
            endif
C           IF(NDC2 .NE. 0) READ(L8, END=5120)(WORK(IR3 + I), I=1,IR4)
 
            j = 0
 
C           IN THE FOLLOWING, WE PROCESS OUTPUT REQUESTS 1 THRU IDDAT.
C           RETRIEVE DATA FROM SWING FILE ONLY FOR TERMNL/BRANCH CONCERN
C           IN EACH DCANG,DCI,DCV DATA IS STACKED IN THE ORDER OF OUTPUT
C           REQUESTS.  THERE ARE NO GAPS
 
            do k = 1, iddat
              k1 = idcxn(1, k)
              k2 = idcxn(2, k)
              ibrn = idcxn(3, k)
              if (ibrn .gt. 0) then
                j = j + 1
                k123 = 9*ndc1 + (ibrn-1)*3
 
C               REARRANGE STORAGE FOR DC BRANCHES SUCH THAT BRANCH CURRE
C               FOLLOWED BY THE TWO BUS VOLTAGES
C               FOR BRANCHES0 DCI=BRANCH CURRENT! DCANG=L.H. BUS VOLTS!
C               DCV = R.H. BUS VOLTAGE
 
                dcang(j) = work(k123+2)
                dci(j) = work(k123+1)
                dcv(j) = work(k123+3)
                dcmods(j) = 0.0
              else
                if (k1 .ne. 0) then
                  j = j + 1
                  k13 = (k1-1)*9
 
C                 FOR TERMNLS0  DCANG = COS ALPHA! DCI = CURRENT! DCV =
 
                  dcang(j) = work(k13+1)
                  dci(j) = work(k13+2)
                  dcv(j) = work(k13+3)
                  dcmods(j) = work(k13+4)
                  dcext(j) = work(k13+5)
                  dcva(j) = work(k13+6)
                  dcvap(j) = work(k13+7)
                endif
                if (k2 .ne. 0) then
                  j = j + 1
                  k23 = (k2-1)*9
                  dcang(j) = work(k23+1)
                  dci(j) = work(k23+2)
                  dcv(j) = work(k23+3)
                  dcmods(j) = work(k23+4)
                  dcext(j) = work(k23+5)
                  dcva(j) = work(k23+6)
                  dcvap(j) = work(k23+7)
                endif
              endif
            enddo
 
C           NDDAT = ( IDCBS + IDCBR )
 
            nddat = j
 
C           WRITMS DCANG,DCI,DCV ARRAYS (20 EACH) EVERY TIME STEP OR DIS
 
            call writmp (1, dcang(1), 20, 1)
C           CALL RITECP(MSUB1(2),KDC1+ICOUN2,1)
            iecsp(kdc1+icoun2) = msub1(2)
            call writmp (1, dci(1), 20, 1)
C           CALL RITECP(MSUB1(2),KDC2+ICOUN2,1)
            iecsp(kdc2+icoun2) = msub1(2)
            call writmp (1, dcv(1), 20, 1)
            call ritecp(msub1(2), kdc3+icoun2, 1)
            iecsp(kdc3+icoun2) = msub1(2)
            call writmp (1, dcmods(1), 20, 1)
C           CALL RITECP (MSUB1(2),KDC4+ICOUN2,1)
            iecsp(kdc4+icoun2) = msub1(2)
            call writmp (1, dcext(1), 20, 1)
C           CALL RITECP(MSUB1(2),KDC5+ICOUN2,1)
            iecsp(kdc5+icoun2) = msub1(2)
            call writmp (1, dcva(1), 20, 1)
C           CALL RITECP(MSUB1(2),KDC6+ICOUN2,1)
            iecsp(kdc6+icoun2) = msub1(2)
            call writmp (1, dcvap(1), 20, 1)
C           CALL RITECP(MSUB1(2),KDC7+ICOUN2,1)
            iecsp(kdc7+icoun2) = msub1(2)
          endif
          do while (irectp .gt. 0 .and. irectp .ne. 130 .and. 
     &              irectp .ne. 190)
 
C           Skip to TCSC record
 
            call gethisi(irectp, 1)
            call gethisi(idesc, 1)
            call gethisi(irecln, 1)
            call skiphis(irecln)
            call gethisw(irectp)
          enddo
 
C         READ TCSC DATA POINTS IF THEY EXIST AND WRITE TO DATA TABLE
 
          if (iznmax .gt. 0) then
            call gethisw(irectp)
            if (debug) then
              call dbgeko('NOUT2 - expecting a TCSC (190) record')
              call dbgwri('  irectp = ', irectp)
            endif
            if (irectp .eq. 190) then
 
C             Have TCSC data to read
 
              call gethisi(irectp, 1)
              call gethisi(idesc, 1)
              call gethisi(irecln, 1)
              if (debug) then
                call dbgeko2('NOUT2 - reading TCSC from ',
     &         'history file.')
                call dbgwri('  IRECTP /record type/ = ', irectp)
                call dbgwri('  IDESC /rec descrip/  = ', idesc)
                call dbgwri('  IRECLN /rec length/  = ', irecln)
              endif
              call gethisf(dat13, irecln)
 
C             Transfer TCSC data to TCSCDATA
 
              if (irecln .gt. 0) then
                do i = 1, iznmax
                  tcscdata(icoun2, 1, i) = dat13(2*i-1)
                  tcscdata(icoun2, 2, i) = dat13(2*i)
                enddo
              endif
            elseif (to .eq. 0.0) then
              call puterr(1,
     &         'NOUT2 - No TCSC data in this history file.')
              call prterr('W', 1)
            endif
          endif
 
C         Here we have read all data for this time step
 
          call gethisw(irectp)

C         IF NSKP IS NONZERO, SKIP OVER NSKP TIME STEPS ON THE SOLUTION
C         HISTORY FILE
 
          target = 0.0
          if (nskp .ne. 0) then
 
C           At this point might already be at the start of a new time.
C           If so, force record type to a crazy value.
 
            irectp = 129
            junk7 = 1 + 7*isg
            junk8 = 1 + 8*isg
            do iz = 1, nskp
              do while (irectp .gt. 0 .and. irectp .ne. 130)
 
C               Not at time record yet, so skip to start of next record
 
                call gethisi(irectp, 1)
                call gethisi(idesc, 1)
                call gethisi(irecln, 1)
                call skiphis(irecln)
                call gethisw(irectp)
              enddo
              endo = 0.0
            enddo
 
C           Done at this time step, read next step
 
            complet = .true.
          endif
        endif
      enddo
 
C     Store generator output variables in mass storage
 
  360 k8000 = MAXPTS
 
C     ICOUNT is lesser of number of time steps (ICOUN1) or time
C     steps + discontinuities.
 
      icount = min0(icoun1, icoun2)
      if (.not. complet) icount = icount - 1
      if (icount .gt. maxcyc) then
        write (errbuf(1), 10140)
        write (errbuf(2), 10150)
        call prterr('W', 2)
10140   format (
     &   '0 The number of time steps on the swing file is greater than M
     &AXCYC on the MH card.'
     &   )
10150   format (' Some of your calculated values may be incorrect.')
      endif
 
C     CALL PRTWST TO PRINT OUT THE LOWEST 20
C     BUS VOLTAGES AND FREQUENCIES
 
      call prtwst(eyrint, eyiint)
 
C     REDUCE ECS FIELD LENGTH
 
C     CALL REQECS(MINECS)

      if (noangl .eq. 0) goto 400
      noknt = noangl
      msind = 1
      ms = 1
      iadr = ksub1
  370 icore = k8000 - noknt
      nostor = icore/noknt
      numtot = nostor
      if (numtot .gt. icount) numtot = icount
      kntwr = 0
      itotal = 0
 
C     Fetch this parm for all gens, one time step at a time, then
C     distribute values to blocks devoted for each gen with this
C     parm.
C     Loop for each time step
 
      do while (.true.)
        do i = 1, numtot
          itotal = itotal + 1

C         CALL REDECP (MSUB(MSIND*2),IADR+ITOTAL-1,1)

          msub(msind*2) = iecsp(iadr+itotal-1)
          if (noknt .gt. 0) call readmp(msind, worksp, noknt, 1)
 
C         Loop for each gen that has this parm.
 
          do ii = 1, noknt
            j = (ii-1)*numtot + noknt
            worksp(j+i) = worksp(ii)
          enddo
          if (itotal .eq. icount) goto 380
        enddo
 
  380   kntwr = kntwr + 1
        n = 0
        iwktot = (noknt-1)*numtot + noknt + 1
        istart = noknt + 1
 
C       Set up ECP blocks spaced by NUMTOT
 
        do j = istart, iwktot, numtot
          n = n + 1
          iecs = kwork + (n-1)*icount + (kntwr-1)*numtot
C         CALL RITECP (WORKSP(J),IECS,I)
          lb = 0
          do la = 1, i
            ecsp(iecs+lb) = worksp(j+lb)
            lb = lb + 1
          enddo
        enddo
        if (itotal .ge. icount) goto 390
      enddo
 
C     For each gen with this parm
 
  390 continue
      do i = 1, noknt
        iecs = kwork + (i-1)*icount
C       CALL REDECP (WORKSP,IECS,ICOUNT)
        lb = 0
        do la = 1, icount
          worksp(la) = ecsp(iecs+lb)
          lb = lb + 1
        enddo
        if (icount .gt. 0) call writmp(msind, worksp, icount, 1)
C       CALL RITECP (MSUB(MSIND*2),IADR+I-1,1)
        iecsp(iadr+i-1) = msub(msind*2)
      enddo
 
C     Loop back to process next gen parm
 
      if (ms .eq. 1) then
        goto 400
      elseif (ms .eq. 2) then
        goto 410
      elseif (ms .eq. 3) then
        goto 420
      elseif (ms .eq. 4) then
        goto 430
      elseif (ms .eq. 5) then
        goto 440
      else
        goto 450
      endif

  400 if (noangp .ne. 0) then
        noknt = noangp
        msind = 2
        ms = 2
        iadr = ksub2
        goto 370
      endif
  410 if (novfl .ne. 0) then
        noknt = novfl*2
        msind = 4
        ms = 3
        iadr = ksub4
        goto 370
      endif
  420 if (nogov .ne. 0) then
        noknt = nogov*2
        ms = 4
        msind = 7
        iadr = ksub7
        goto 370
      endif
  430 if (nocur .ne. 0) then
        noknt = nocur*2
        msind = 10
        ms = 5
        iadr = ksub10
        goto 370
      endif
  440 if (nosup .ne. 0) then
        noknt = nosup*2
        msind = 12
        ms = 6
        iadr = ksub12
        goto 370
      endif
  450 continue
      icnt2 = icount*2
      if (nobus .ne. 0) then
        icore = k8000 - nobus
        nostor = icore/nobus
        numtot = nostor
        if (nostor .gt. icount) numtot = icount
        numto2 = numtot*2
        kntwr = 0
        itotal = 0
        do while (.true.)
          i1 =  - 1
          i2 = 0
          do i = 1, numtot
            i1 = i1 + 2
            i2 = i2 + 2

C           CALL REDECP(MSUB13(2),KSUB13+ITOTAL,1)

            msub13(2) = iecsp(ksub13+itotal)
            itotal = itotal + 1
            if (nobus .gt. 0) call readmp (13, worksp, nobus, 1)
            i3 =  - 1
            i4 = 0
            do ii = 1, nobus2
              j = (ii-1)*numto2 + nobus
              i3 = i3 + 2
              i4 = i4 + 2
              worksp(j+i1) = worksp(i3)
              worksp(j+i2) = worksp(i4)
            enddo
            if (itotal .eq. icount) goto 470
          enddo
  470     kntwr = kntwr + 1
          i2 = 2*i
          n = 0
          iwktot = (nobus2-1)*numto2 + nobus + 1
          istart = nobus + 1
          do j = istart, iwktot, numto2
            n = n + 1
            iecs = kwork + (n-1)*icnt2 + (kntwr-1)*numto2
 
C           CALL RITECP (WORKSP(J),IECS,I2)
 
            lb = 0
            do la = 1, i2
              ecsp(iecs+lb) = worksp(j+lb)
              lb = lb + 1
            enddo
          enddo
          if (itotal .eq. icount) goto 480
        enddo
 
C       TRANSFER VOLTAGES FROM ECS TO SIMULATED MASS STORAGE
 
  480   ktbus = 0
        ktbus1 = 0
 
C       RESET WRITMP ARRAY COUNT TO 1
 
        maxu(7) = 1
        do j = 1, nmx
          kbusno(j) = 0
          if (mbus(j) .ne. 0) then
            ktbus = ktbus + 1
            kbusno(j) = ktbus
            iecs = kwork + (ktbus-1)*icnt2

C           CALL REDECP(WORKSP,IECS,ICNT2)

            lb = 0
            do la = 1, icnt2
              worksp(la) = ecsp(iecs+lb)
              lb = lb + 1
            enddo
            if (icnt2 .gt. 0) call writmp (13, worksp, icnt2+4, 1)

C           CALL RITECP(MSUB13(2),KSUB13+KTBUS-1,1)

            iecsp(ksub13+ktbus-1) = msub13(2)
          endif
        enddo
        if (ildatt .ne. 0) then
          if (kldatt .gt. ildatt) ildatt = kldatt
          call linout()
          if (irdknt .ne. 0) then
            write (outbuf, 10160)
10160       format (5x, ' SUMMARY OF R - RDOT RELAY TRIP MARGINS')
            call prtout(1)
            iwckt = iwckt + 1
            write (wrscom(iwckt), 10160)
            if (wtim2 .ne. -1) then
              write (outbuf, 10170) wtim1, wtim2
10170         format (5x, 'DURING TIME WINDOW ', f8.2, ' CYCLES TO ',
     &         f8.2, ' CYCLES.')
              call prtout(1)
              iwckt = iwckt + 1
              write (wrscom(iwckt), 10170) wtim1, wtim2
            endif
            do ltrr = 1, irdknt
              write (outbuf, 10180) rname1(ltrr), rbkv1(ltrr), rname2
     &         (ltrr), rbkv2(ltrr), rpar(ltrr), rzero(ltrr), rslope
     &         (ltrr)
10180         format (5x, a8, 1x, f5.1, 1x, a8, 1x, f5.1, 1x, a1,
     &         ' ZERO INTERCEPT = ', f6.2, ' SLOPE = ', f6.2)
              call prtout(1)
              iwckt = iwckt + 1
              write (wrscom(iwckt), 10180) rname1(ltrr), rbkv1(ltrr),
     &         rname2(ltrr), rbkv2(ltrr), rpar(ltrr), rzero(ltrr),
     &         rslope(ltrr)
              write (outbuf, 10190) rrdmax(ltrr), rtmax(ltrr), rrdmin
     &         (ltrr), rtmin(ltrr)
10190         format (5x, ' MAXIMUM MARGIN = ', f8.2, ' AT ', f7.2,
     &         ' CYCLES ', 'MINIMUM MARGIN = ', f8.2, ' AT ', f7.2,
     &         ' CYCLES. ')
              call prtout(1)
              iwckt = iwckt + 1
              write (wrscom(iwckt), 10190) rrdmax(ltrr), rtmax(ltrr),
     &         rrdmin(ltrr), rtmin(ltrr)
            enddo
          endif
          call mpost('NOUT2b')
        endif
      endif
      if (igdatt .ne. 0) call genout()
 
C     CALL GDIFOT TO OUTPUT GENERATOR DIFFERENCE QUANTITIES
 
      if (idifkt .ne. 0) call gdifot()
      call mpost('NOUT2c')
 
      if (ibdatt .ne. 0) call busout()
      call mpost('NOUT2d')
 
      if (ispknt .ne. 0) call sptout()
      call mpost('NOUT2e')
c
c     Call VFHIST to write final reports of voltage dip information
c
      call vfhist (3, 9999.0)

      t(icount+1) = t(icount)
      t(icount+2) = t(1)
 
C     REDEFINE 'KWORK' SUCH THAT NETWORK INFO STORED IN ECS IS NOT
C     CLOBBERED.  'KWORK' IS ECS ADDRESS OF WORK AREA
 
      if (kwork .lt. (knewt+nmx)) kwork = knewt + nmx
      if (ktbus .lt. 0) ktbus = 0
      ktot = ktbus + ktbus1 + ktnol1 + ktnol2 + ktzpp + ktnol4 + ktnol5
     & + ktnol6
      jtot = 0
      do ind = 1, 14
        jtot = jtot + jcount(ind)
      enddo
      ktot = jtot + ktot
      if (iddat .ne. 0) call ndcout()
      call mpost('NOUT2f')
      ktot = ktot + iddat
 
C     SET LINK = 6 SO THAT SWINGM CALLS CALPLT REGARDLESS OF
C     OF WHAT PLOTS HAVE BEEN REQUESTED SO THAT COMMENTS ARE
C     ARE PLOTTED.
 
      link = 6
      write (outbuf, 10200)
      call prtout(1)
10200 format ('0SUBROUTINE NOUT2 HAS BEEN PROCESSED.')
 
C     TOSS OUT THE AUXILIARY OUTPUT FILE IF NOT USED.
 
      if (nus11 .gt. 0) then
        if (auxfmt .eq. 'STD') then
          write (l11, 10210)
10210     format ('END')
        else
          write (l11, '(A)') '/END'
        endif
        call closts(l11, 0)
      else
        call closts(l11, 1)
 
      endif
      return
      end
 
