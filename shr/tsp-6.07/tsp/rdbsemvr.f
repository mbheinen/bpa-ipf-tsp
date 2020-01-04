C    %W% %G%
C****************************************************************
C
C       File: rdbsemvr.f
C       Purpose: Routine to transfer data loaded into IPF commons into
C                TSP commons for use with the output routines.
C
c       Notes: Some IPF include files are required.
C
C       Author: Dan Clark    Date: 12 Sept 1994
C                            Modified: 11 Dec 1997
C       Called by: rdbse
C
C****************************************************************
c
       subroutine rdbsemvr
       implicit none
c
      include 'tspinc/blkcom1.inc'
c
c     Constants MAXBUS, MAXGEN, MXLSHD, MAXMAC
c     See conflicts MAXBUS,MAXGEN,MAXBRN + all uses of maxbrn

      include 'tspinc/params.inc'
      include 'tspinc/pf_parametr.inc'
c
c     pfcase, pfdate
      include 'tspinc/titles.inc'
      include 'tspinc/pf_dtaioptsp.inc'

c     ntot, mtdcbs, ...
      include 'tspinc/out512.inc'

c     bmva, ...
      include 'tspinc/pf_blanktsp.inc'

c     exnamc, exzonec, alf2inp etc
      include 'tspinc/pf_bus.inc'

c     tsp: brnch, jbrnch ipf: ibrnch, ikbrnch
      include 'tspinc/brnch.inc'
      include 'tspinc/pf_branchtsp.inc'

c     errbuf
      include 'tspinc/prt.inc'

c     y matrix variables: pnetu, qnetu, gkku, bkku, etc
      include 'tspinc/pf_alpha.inc'

c     line data lndatn, etc
      include 'tspinc/lindat.inc'

c     ildatt
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'

c     lnmodn
      include 'tspinc/linmod.inc'

c     nbnew, nbold
      include 'tspinc/nb.inc'

c     buf4k
      include 'tspinc/space.inc'

c     kka
      include 'tspinc/kka.inc'

c     kk,vold1,vold2 - share these with reformat modules
      include 'tspinc/rddtai_mv.inc'

c     constants defining branch types brtyp_xx
      include 'tspinc/brtype.inc'

c     area, zone
      include 'tspinc/areanz.inc'
      include 'tspinc/pf_arcntl.inc'

c     counters shared by nout1
      include 'tspinc/rddtamvrpt.inc'
c
c     local variables
c
      character*10 pfcas,clbel1,clbel2,pversn,usrnam,bdate
      character*10 auxpf, clabl1, clabl2
      character*8 pflbl,plotd
      integer ntota, ntotb, kdtot, kmdcbr, jtie, kbrknt, nztot, natot, 
     &        nbslck, ltot, kecst, kecsy, lntoto, lntot2, dbg, i, j, k,
     &        nb, nbx, iptr1, iptr2, iaptr, brtype, maxzone, maxarea,
     &        icase_vers, is, isx, ie, ibridx, ibridx2, js,jf, l, mt
      real xbase

      integer keybrd
      integer jjj,kkk

       data pflbl, plotd/'PF DATA', 'VRSND001'/
       dbg = 0
c
c      move the ipf header info & counts used by tsp
c
c      read (datai, end=200, err=300) type, case, bdate, clabl1,
c    1      clabl2, pversn, usrnam, count
c      read (l3, end = 100) auxpf, pfcas, bdate, clabl1, clabl2, pversn,
c     & usrnam, count
c

       pfcas = pf_cspare(30)
       bdate = pf_cspare(31)
       icase_vers = pf_count(2)
       if (pf_count(1) .ne. 1) then
         write (errbuf(1), 10060)
         call prterr('E', 1)
10060    format (' THE POWER FLOW SOLUTION FAILED.')
         call erexit()
       endif
       ntot = pf_ntot_alf
       ntotd = pf_ntot_alf
       ntot2 = pf_ntot2
       ltot = pf_ltot * 2
       ntota = pf_ntota
       ntotb = pf_ntotb
       ntotc = pf_ntotc
       kdtot = pf_kdtot
c      mtdcbs = pf_mtdcbs
       kmdcbs = pf_mtdcbs
c      mtdcln = pf_mtdcln
       kmdcbr = pf_mtdcln
       kxtot = pf_kxtot
       bmva = pf_bmva
       jtie = pf_jtie
c      note: these two values are computed below
c tsp  kecsy = pf_kecsy
c      kecsy = pf_yptr
c ts   kecsym = pf_kecsy
c      kbsknt = pf_kbsknt
c      kbrknt = pf_kbrknt
       jphno = pf_jphno
       nztot = pf_nztot
       natot = pf_natot
       ntotcs = pf_ntotcs
       nbslck = pf_nbslck
c      nslkxx = pf_nslkxx
c not used
c      kslloc = pf_nslkxx(1,1) 
c      kslack = pf_nslkxx(2,1)
c      kount = pf_kount
       ntotd = pf_ntot_alf
       ltoto = ltot
c      kecsym = kecsy
c
c transfer branch data records
c   loop through all the buses in alphabetic order (do nb loop)
c   get the pointer to the first branch (iptr)
c   loop through all the branches for this bus (while iptr loop)
c   move the data from ipf to tsp commons.
c   if the branch type is negative, move the data for 
c   transposed branches from the single entry ipf common to the 
c   double entry tsp common.
c   must make ibrid an integer from a character
c   iptr2 is a pointer to the double entry table for branch data
c   iptr1 is a pointer to the single entry branch data table.
c
c tsp
c        do i1 = 1, ltot, 100
c          i2 = min0(i1+99, ltot)
c          read (l3) ((brnch(j, i), j = 1, 18), i = i1, i2)
c        enddo
c ipf
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
c         move branch data
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
     1   pf_ltot
      endif
      ltot = i
c
c get the indices for changing between optimal and alpha order
c tsp
c     read (l3) (nbnew(i), i = 1, ntot)
c     read (l3) (nbold(i), i = 1, ntotd)
c
c arrays of index values 
c indo2x 
c   given a optimal order index 
c     find the external order index (which is alpha order for tsp)
c indx2o 
c   given an alpha order index find the optimal order index
c
c the ipf translation is sticky since the table is from
c optimal order to input order NOT alpha order like tsp wants to use
c
c
c to setup the tsp table: given optimal order turn in 2 external (alpha) order
c must use ipf tables
c given an input order find the alpha order index
c  OF given an optimal order find the input order index
cie.
c        [1] [2] [3] [4] [5]
c input   D   B   A   C   E
c inp2opt 
c
c exnamc  A   B   C   D   Slack
c indo2x  5   2   3   4   1
c indx2o  5   2   3   4   1
c
      do i = 1,pf_ntot_alf
        nbnew(i) = pf_inp2alf(pf_opt2inp(i)) 
        nbold(pf_inp2alf(pf_opt2inp(i))) = i
      end do
      do i = 1,pf_ntot_alf
        if(nbold(i) .ne. pf_inp2opt(pf_alf2inp(i)) ) then
        write(*,'(a,i6)') 'rddtamv: indx2o failed check ', i
        endif
      end do
c
c get the bus and branch data solution values 
c from ipf and move to tsp ecs commons
c     see logic in ldoldbse.f
c     loop through all the busses (do intot)
c     move the ipf bus solution data to ecs
c     loop through all the branches for this bus ???
c     and setup the index arrays for this bus (kk)
c tsp
c      if (jphno.eq.0) go to 1180
c      read(l3) ((jphid(i,j),i=1,8),j=1,jphno)
c from tapewk 
c    kecsx = 80000
c    
c1180 read (l3) ((kk(i,j),i=1,2),j=1,ntot) 
c1210 if(kecsy.le.4000)go to 1220
c      read(l3) (store(i),i = 1,4000)     
c     call ritecs(store,kecsx,4000)
c     kecsx=kecsx+4000
c     kecsy=kecsy-4000
c     go to 1210
c1220 read(l3) (store(i),i=1,kecsy)
c     call ritecs(store,kecsx,kecsy)
c     kecsx=kecsx+kecsy
c     kecst=kecst-1
c1235 kecsy=kecsym
c
c
      if(dbg.eq.1) then
        write(*,'(a)') 'rddtamv.f: write out the y-mtx'
        write(*,'(a)') 'No Length Pnetu Qnetu GKKu BKKu ntypu nspar'
        write(*,'(a)') ' vlimn vlimx ploadu qloadu inetr ineti'
        write(*,'(a)') '   ikmu - gkmu - bkmu'
      endif

c     kecsy = kecsym
c     kecst = klecs - 1

      is = 1
      isx = 1
      do i = 1,pf_ntot_alf
        buf4k(isx) = pf_pnetu(i) 
        buf4k(isx+1) = pf_qnetu(i) 
        buf4k(isx+2) = pf_gkku(i) 
        buf4k(isx+3) = pf_bkku(i)
        ibuf4k(isx+4) = pf_ntypu(i)
        ibuf4k(isx+5) = pf_nspar(i)
        buf4k(isx+6) = pf_vlimn(i)
        buf4k(isx+7) = pf_vlimx(i) 
        buf4k(isx+8) = pf_ploadu(i)
        buf4k(isx+9) = pf_qloadu(i)
        buf4k(isx+10) = pf_inetr(i)
        buf4k(isx+11) = pf_ineti(i)
        ie = 12 + 3 * pf_kmlen(i)
        js = pf_km(i)
        jf = pf_km(i)-1+pf_kmlen(i)
        j = isx+12
        do l = js, jf
          ibuf4k(j) = pf_ikmu(l)
          buf4k(j+1) = pf_gkmu(l)
          buf4k(j+2) = pf_bkmu(l) 
          j = j + 3
        end do
        kka(1,i) = isx
        kka(2,i) = ie
        isx = isx + ie
      enddo
      kecmax = isx
      kecsy = isx
      kecsym = isx
      kecst = isx - 1
c     kecst = klecs - 1

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
      return
      end
