C    %W% %G%
C****************************************************************
C
C       File: rdbsemvs.f
C       Purpose: Routine to transfer data loaded into IPF commons into
C                TSP commons for use with the solution routines.
C
c       Notes: Some IPF include files are required.
C
C       Author: Dan Clark    Date: 12 Sept 1994
C                            Modified:  3 Mar 1998
C       Called by: rdbse
C
C****************************************************************
       subroutine rdbsemvs
       implicit none

      include 'tspinc/blkcom1.inc'
c
c     constants MAXBUS, MAXGEN, MXLSHD, MAXMAC
c     see conflicts MAXBUS,MAXGEN,MAXBRN + all uses of maxbrn
      include 'tspinc/params.inc'
      include 'tspinc/pf_parametr.inc'
      include 'tspinc/titles.inc'
      include 'tspinc/pf_dtaioptsp.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/param.inc'
      include 'tspinc/pf_blanktsp.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pf_bus.inc'
      include 'tspinc/in1n.inc'
      include 'tspinc/areanz.inc'
      include 'tspinc/pf_arcntl.inc'
      include 'tspinc/brnch.inc'
      include 'tspinc/pf_branchtsp.inc'
      include 'tspinc/pf_phase.inc'
      include 'tspinc/bypass.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/rddtai_mv.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/brtype.inc'
      include 'tspinc/pf_alpha.inc'
      include 'tspinc/pf_dc2t.inc'
      include 'tspinc/pf_dcmt.inc'
c
c     local variables
c
      integer dbg, i, j, k, nb, nbx, iptr1, iptr2, iaptr, brtype,
     &        maxzone, maxarea, icase_vers, is, isx, ie, ibridx, 
     &        ibridx2, js, jf, l, mt, iecs_space(4000), keybrd,
     &        jjj, kkk, k1
      real xbase, ecs_space(4000)
      equivalence (ecs_space,iecs_space)
      character dc_code * 1

      dbg = 0
      pfcase = pf_cspare(30)
      pfdate = pf_cspare(31)
      icase_vers = pf_count(2)
      if (pf_count(1) .ne. 1) then
        write (errbuf(1), 10060)
        call prterr('E', 1)
10060   format (' THE POWER FLOW SOLUTION FAILED.')
        call erexit()
      endif

      if (pf_ntot .ne. pf_ntot_alf) then
        write(*,'(a, 2i6)') ' * Warning ntot != ntot_alf', 
     1    pf_ntot, pf_ntot_alf
      else
        write(*,'(a, 2i6)') ' * OK ntot = ntot_alf', 
     1    pf_ntot, pf_ntot_alf
      endif
      ntot = pf_ntot_alf
      ntotd = pf_ntot_alf
      ltot = pf_ltot * 2
      ntota = pf_ntota
      ntotb = pf_ntotb
      ntotc = pf_ntotc
      kdtot = pf_kdtot
      kmdcbs = pf_mtdcbs
      kmdcbr = pf_mtdcln
      kxtot = pf_kxtot
      bmva = pf_bmva
      jtie = pf_jtie

      kbsknt = pf_kbsknt
      jphno = pf_jphno
      lphase = jphno
      kslack = pf_nslkxx(2,1)

      do i = 1, MAXBUS
        exbase(i)=0.0
      end do
      call ritecs (exbase,1,2100)

      do i = 1,ntot
        exnamc(i) = pf_bus(pf_alf2inp(i))
        exbase(i) = pf_base(pf_alf2inp(i))
        exzonc(i) = pf_zone(pf_alf2inp(i))
      end do 
      ibxyz = 0
      do i = 1, ntotd
        xbase = exbase(i)
        if (ibxyz .ne. 0) then
          do k = 1, ibxyz
            if (basekv(k) .eq. xbase) goto 120
          enddo
        endif
        ibxyz = ibxyz + 1
        basekv(ibxyz) = xbase
  120   continue
      enddo
c
c     Set the maximum zones and areas based on the program version
c
      if(icase_vers .gt. 4) then
        maxzone = PF_MAXCAZ
        maxarea = PF_MAXCAR
        write (*,'(a,i6,i6)') 'Note new base case max zone, area ', 
     1    maxzone, maxarea 
        maxzone = 10
        maxarea = 60
        write (*,'(a,i6,i6)') ' -->reset to tsp max zone, area ', 
     1    maxzone, maxarea
      else
        maxzone = 10
        maxarea = 60
      endif
      if(ntotc .lt. maxarea) then
        maxarea = ntotc
        write (*,'(a,i6)') ' -->use max area ntotc ', 
     1    maxarea 
      else
        write (*,'(a,i6)') 'Error ipf areas > tsp maxarea', 
     1    ntotc, maxarea
        call erexit
      endif
c
c     Transfer the area data  
c
      do i = 1,maxarea
        areanc(i) = pf_arcnam(i)
      end do
c
c     Transfer zone data to tsp commons
c
      do i = 1,maxzone
        do j = 1,maxarea
          areazc(i,j) = pf_arczns(i,j)
        end do
      end do
c
c     Transfer branch data records:
c       loop through all the buses in alphabetic order (do nb loop)
c       get the pointer to the first branch (iptr)
c       loop through all the branches for this bus (while iptr loop)
c       move the data from ipf to tsp commons.
c       if the branch type is negative, move the data for 
c       transposed branches from the single entry ipf common to the 
c       double entry tsp common.
c       must make ibrid an integer from a character
c     iptr2 is a pointer to the double entry table for branch data
c     iptr1 is a pointer to the single entry branch data table.
c
      i = 0
      do nbx = 1, pf_ntot_alf
        nb = pf_alf2inp(nbx)
        iptr2 = pf_kbsdta(16,nb)
        do while (iptr2 .gt. 0)
          iptr1 = pf_brnch_ptr(iptr2)
          if(iptr1 .lt. 0) then
            iaptr = -iptr1
          else
            iaptr = iptr1
          endif
          i = i + 1

c         Move branch data
          do j = 1,18
            brnch(j,i) = pf_brnch(j,iaptr)
          end do
          jbrnch(1,i)  = pf_brtype(iptr2)
          jbrnch(2,i)  = pf_inp2alf(pf_kx(iptr2))
          jbrnch(12,i) = pf_inp2alf(pf_ky(iptr2))
          call putchr(1, pf_brid(iptr2), jbrnch(13,i))
          jbrnch(14,i) = pf_brsect(iptr2)

c         Fix up transposed branch data
          if(iptr1 .lt. 0) then
            brtype = pf_brtype(iptr2)
            if(brtype .eq. BRTYP_T) then
              brnch(9,i) = pf_brnch(10,iaptr)
              brnch(10,i) = pf_brnch(9,iaptr)
            else if (brtype .eq. BRTYP_TP) then
              brnch(9,i) = -pf_brnch(9,iaptr)
            else if (brtype .eq. BRTYP_E) then
              brnch(7,i) = pf_brnch(9,iaptr)
              brnch(8,i) = pf_brnch(10,iaptr)
              brnch(9,i) = pf_brnch(7,iaptr)
              brnch(10,i) = pf_brnch(8,iaptr)
            else if (brtype .eq. BRTYP_PEQ) then
              brnch(4,i) = pf_brnch(10,iaptr)
              brnch(5,i) = pf_brnch(11,iaptr)
              brnch(6,i) = pf_brnch(8,iaptr)
              brnch(7,i) = pf_brnch(9,iaptr)
              brnch(8,i) = pf_brnch(6,iaptr)
              brnch(9,i) = pf_brnch(7,iaptr)
              brnch(10,i) = pf_brnch(4,iaptr)
              brnch(11,i) = pf_brnch(5,iaptr)
            endif
          endif
          iptr2 = pf_brnch_nxt(iptr2)
        end do
      end do

c     save the total number of branches in a tsp variable
      if(i .ne. pf_ltot * 2) then
        write(*,'(a,i6,a,i6)') 'Warning line cnt ', i, ' != 2* ', 
     &    pf_ltot
      endif
      ltot = i
c
c     Arrays of index values: 
c       indo2x - given a optimal order index find the external order 
C                index (which is alpha order for tsp)
c       indx2o - given an alpha order index find the optimal order index
c
c     The ipf translation is sticky since the table is from
c     optimal order to input order NOT alpha order like tsp wants to use
c
c
c     To setup the tsp table: 
c        Given optimal order turn in 2 external alpha) order must use 
c        ipf tables.  
C        Given an input order find the alpha order index of.
c        Given an optimal order find the input order index ie.
C
c                [1] [2] [3] [4] [5]
c         input   D   B   A   C   E
c         inp2opt 
c
c         exnamc  A   B   C   D   Slack
c         indo2x  5   2   3   4   1
c         indx2o  5   2   3   4   1
c
      do i = 1,ntot
        indo2x(i) = pf_inp2alf(pf_opt2inp(i)) 
        indx2o(pf_inp2alf(pf_opt2inp(i))) = i
      end do
      do i = 1,ntotd
        if(indx2o(i) .ne. pf_inp2opt(pf_alf2inp(i)) ) then
        write(*,'(a,i6,a)') 'rddtamv: indx2o failed check ', 
     &    i, exnamc(i)
        endif
      end do
c
c     Write out the table of external and optimal bus/base data
c
      if(dbg.eq.1) then
        write(*, '(a)')
     &    'No Ext Bus   Base   indx2o   indo2x   Bus   Base'
        do i = 1, ntot
          write(*, '(i6, a10, f6.1, i4, i4, a10, f6.1)')
     &      i, exnamc(i), exbase(i), indx2o(i), indo2x(i), 
     &      exnamc(indx2o(i)), exbase(indx2o(i))
        end do
      endif
c
c     Move the solution e,f and capcor values. A better design is to 
c     move ipf e&f directly to tsp eyr&eyi variables for now. Run initl1 
c     subroutine to do the nasty work
c
      do i = 1,ntot
        vold1(i) = pf_e(i)
        vold2(i) = pf_f(i)
      end do
      do j = 1,2
        do i = 1,ntot
          capold(j,i) = pf_capcor(j,i)
        end do
      end do
c
c     Get the bus and branch data solution values from ipf and move to 
c     tsp ecs commons. See logic in ldoldbse.f:
c       Loop through all the busses (do intot)
c       Move the ipf bus solution data to ecs
c       Loop through all the branches for this bus ??? and setup the 
c       index arrays for this bus (kk) tsp
c
      if (jphno.ne.0) then
        do j = 1,jphno
            jphid(1,j) = pf_inp2alf(pf_jphid(1,j))
            jphid(2,j) = pf_inp2alf(pf_jphid(2,j))
          do i = 3,8
            jphid(i,j) = pf_jphid(i,j)
          end do
        end do
      endif
      if(dbg.eq.1) then
        write(*,'(a)') 'rddtamv.f: write out the y-mtx'
        write(*,'(a)') 'No Length Pnetu Qnetu GKKu BKKu ntypu nspar'
        write(*,'(a)') ' vlimn vlimx ploadu qloadu inetr ineti'
        write(*,'(a)') '   ikmu - gkmu - bkmu'
      endif

      ibridx = 0
      kecsx = 80000
      kecst = kecsx-1
      kzrecs = 1
      isx = 1
      do i = 1, pf_ntot_alf
        is = 1
        ecs_space(is) = pf_pnetu(i) 
        ecs_space(is+1) = pf_qnetu(i) 
        ecs_space(is+2) = pf_gkku(i) 
        ecs_space(is+3) = pf_bkku(i)
        iecs_space(is+4) = pf_ntypu(i)
        iecs_space(is+5) = pf_nspar(i)
        ecs_space(is+6) = pf_vlimn(i)
        ecs_space(is+7) = pf_vlimx(i) 
        ecs_space(is+8) = pf_ploadu(i)
        ecs_space(is+9) = pf_qloadu(i)
        ecs_space(is+10) = pf_inetr(i)
        ecs_space(is+11) = pf_ineti(i)
        ie = 12 + 3 * pf_kmlen(i)
        if(dbg.eq.1) then
          write(*,'(3i4,2f8.3, 2f8.4, 2i4, 2f10.4, 4f8.3)')
     &      i, isx, ie , ecs_space(1), ecs_space(2), ecs_space(3), 
     &      ecs_space(4), iecs_space(5), iecs_space(6), ecs_space(7), 
     &      ecs_space(8), ecs_space(9), ecs_space(10), ecs_space(11), 
     &      ecs_space(12)
        endif
        js = pf_km(i)
        jf = pf_km(i)-1+pf_kmlen(i)
        j = is+12
        do l = js, jf
          iecs_space(j) = pf_ikmu(l)
          ecs_space(j+1) = pf_gkmu(l)
          ecs_space(j+2) = pf_bkmu(l) 
        if(dbg.eq.1) then
          write(*, '(6x,i4, i4, 2f8.4)')
     &      j, iecs_space(j), ecs_space(j+1), ecs_space(j+2)
        endif
          j = j + 3
        end do
        kk(1,i) = isx
        kk(2,i) = ie
        call ritecs(ecs_space,kecst+isx,ie)
        isx = isx + ie
      enddo
      kecsx = kecsx + isx
      kecsy = isx
      kecsym = isx
c
c     Move the dc data (see dcinp)
c
      if (kdtot .eq. 0) then
        ldc = 0
      else
        do i = 1, pf_kdtot
           k1 = pf_dc2t(1,i)
           kdxtan(1,i) = pf_inp2alf(k1)
           dcdtan(2,i) = pf_dc2t(2,i)
           k1 = pf_dc2t(3,i)
           kdxtan(3,i) = pf_inp2alf(k1)
           do k = 4, 6
              dcdtan(k,i) = pf_dc2t(k,i)
           enddo
           kdxtan(7,i) = pf_dc2t(7,i)
           do k = 8, 19
              dcdtan(k,i) = pf_dc2t(k,i)
           enddo
           kdxtan(20,i) = pf_dc2t(20,i)
           do k = 21, 32
              dcdtan(k,i) = pf_dc2t(k,i)
           enddo
           k1 = pf_dc2t(33,i)
           kdxtan(33,i) = pf_inp2alf(k1)
           k1 = pf_dc2t(34,i)
           kdxtan(34,i) = pf_inp2alf(k1)
           do k = 35, 36
              dcdtan(k,i) = pf_dc2t(k,i)
           enddo
           kdxtan(37,i) = pf_dc2t(37,i)
           kdxtan(38,i) = pf_dc2t(38,i)
           do k = 39, 45
              dcdtan(k,i) = pf_dc2t(k,i)
           enddo
        enddo
        if (keybrd(22) .ne. 0) then
          do k = 1, pf_kdtot
            do jjj = 1, 45, 8
              kkk = min0(jjj+7, 45)
              write (outbuf, 10010) (j, dcdtan(j, k), j = jjj, kkk)
              call prtout(1)
            enddo
          enddo
10010     format (8(i5, e11.4))
        endif
      endif
c
c     Mdcinp loads
c      
      do i = 1,kmdcbs
         k1 = pf_dcmtbs(1,i)
         kdcx(1,i) = pf_inp2alf(k1)
         dcx(2,i) = pf_dcmtbs(2,i) 
         k1 = pf_dcmtbs(3,i)
         kdcx(3,i) = pf_inp2alf(k1)
         call getchr_8 (1, dc_code, pf_dcmtbs(4,i))
         call putchr (1, dc_code, kdcx(4,i))
         do k = 5, 14
            dcx(k,i) = pf_dcmtbs(k,i)
         enddo
         kdcx(15,i) = pf_dcmtbs(15,i)
         do k = 16, 20
            dcx(k,i) = pf_dcmtbs(k,i)
         enddo
         kdcx(21,i) = pf_dcmtbs(21,i)
         kdcx(22,i) = pf_dcmtbs(22,i)
         do k = 23, 26
            dcx(k,i) = pf_dcmtbs(k,i)
         enddo
         kdcx(27,i) = pf_dcmtbs(27,i)
         kdcx(28,i) = pf_dcmtbs(28,i) 
         do k = 29, 30
            dcx(k,i) = pf_dcmtbs(k,i)
         enddo
         do k = 31, 36
            kdcx(k,i) = pf_dcmtbs(k,i)
         enddo
      end do
      do j = 1,10
        do i = 1,kmdcbr
          if(j.eq.1 .or. j.eq.2) then
            k1 = pf_dcmtln(j,i)
            kdclin(j,i) = pf_inp2alf(k1)
          else
            dcline(j,i) = pf_dcmtln(j,i)
          endif
        end do
      end do

      return
      end
