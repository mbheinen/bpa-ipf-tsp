C    %W% %G%
       subroutine rdbsetr
c
c      read data from old format ipf base file into tsp variables
c      read logic was consolidated into a single module
c      from many different tsp modules.
c
       implicit none

c     constants maxbus, maxgen, mxlshd, maxmac 
      include 'tspinc/params.inc'
c     l3, swcas
       include 'tspinc/blkcom1.inc'
c     pfcase, ...
       include 'tspinc/titles.inc'
c     ntot, mtdcbs, ...
      include 'tspinc/out512.inc'
c     bmva, ...
c      include 'tspinc/param.inc'
c     line data lndatn, etc
      include 'tspinc/lindat.inc'
c     ildatt
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
c     lnmodn
      include 'tspinc/linmod.inc'
c     brnch, kbrnch
      include 'tspinc/brnch.inc'
c     ibxyz
c      include 'tspinc/pointr.inc'
c     errbuf
      include 'tspinc/prt.inc'
c     kk,vold1,vold2 - share these with reformat modules
      include 'tspinc/rddtai_mv.inc'
c     nbnew, nbold
      include 'tspinc/nb.inc'
c     buf4k
      include 'tspinc/space.inc'
c     kka
      include 'tspinc/kka.inc'
c     counters used by nout1.f
      include 'tspinc/rddtamvrpt.inc'

c
c     local variables
c     nout1.f, nout2.f 
c
      integer iflerr
      dimension nslkxx(4,10)
      integer nslkxx
      character*10 pfcas,clbel1,clbel2,pversn,usrnam,bdate
      character*10 auxpf, clabl1, clabl2
      integer count(100), kount
      dimension kount(100)
      character ch80*80
      character*8 pflbl,plotd
c   seems like nout1,nout2 should have include comn34.inc for ntota, etc
      integer ntota, ntotb, kdtot, kmdcbr, jtie, 
     & kbrknt, nztot, natot, nbslck, ltot
      integer kecst, kecsy, lntoto, lntot2
      integer i,j,k
      integer iflag
      integer il, il1, i1, i2 
      integer ii, ij, jj, ks, ierr, iauto, ib1, ib2
      character kpc
      integer is, if

      real exdum(MAXBUS)
      integer ixdum(MAXBUS)
      common /scratch/ scratch
      real scratch
      real gkm, bkm, gk1, gk2, bk1, bk2
      integer junk

c  external functions 
      integer indx2n

      data pflbl, plotd/'PF DATA', 'VRSND001'/

c
c  code to read the input data from a base file
c  for generating output report information
c       rewind(l3)
c 
c dlc nout1.f
c
      iflerr = 1
      read (l3, end = 100) auxpf, pfcas, bdate, clabl1, clabl2, pversn,
     & usrnam, count
  100 continue
      iflerr = 0
      if (iflerr .gt. 0) then
        ch80 = 'NOUT1 - Requested Powerflow is not a valid base file'
        call puterr(1, ch80)
        call prterr('E', 1)
        call erexit()
      endif
      call upcase(pfcas)
      call upcase(pfcase)
C     call dbgeko ('NOUT1 - Comparison of case names')                !dem
C     call dbgwrc ('  PFCAS  = ',PFCAS)                               !dem
C     call dbgwrc ('  PFCASE = ',PFCASE)                              !dem
C     endif                                                             !dem
      if (auxpf .ne. 'PF DATA') then
        write (errbuf(1), 10020)
10020   format (' THE REQUESTED FILE IS NOT A DATA HISTORY FILE ',
     &   'FROM THE POWER FLOW.')
        call prterr('E', 1)
        call erexit()
      endif
      if (pfcas .ne. pfcase) then
        if (pfcas .eq. 'END') then
          write (errbuf(1), 10030)
10030     format (' POWER FLOW CASE NOT FOUND')
          call prterr('E', 1)
          call erexit()
        else
          write (errbuf(1), 10040) pfcase
10040     format (' CASE ( ', a10, ' ) IS NOT ON POWER FLOW.')
          call prterr('E', 1)
          write (outbuf, 10050) auxpf, pflbl
10050     format ('0', 5x, a10, 2x, a10)
          call prtout(1)
          call erexit()
        endif
      endif
      if (count(1) .ne. 1) then
        write (errbuf(1), 10060)
        call prterr('E', 1)
10060   format (' THE POWER FLOW SOLUTION FAILED.')
        call erexit()
      endif

c
      read (l3) ntot,ntot2,ltot,ntota,ntotb,ntotc,kdtot,kmdcbs, 
     & kmdcbr,kxtot,bmva,jtie,kecsy,kbsknt,kbrknt,jphno,nztot,
     & natot,nbslck,nslkxx,kount

      ntotd = ntot
      ltoto = ltot
      kecsym = kecsy
c
C     EXNAMC AND IXNAME HAVE BEEN PREVIOUSLY READ FROM L8  **
c      read (l3) exdum
c      read (l3) ixdum
      read (l3) scratch
      read (l3) scratch
c

c
c dlc nout2.f
c

C     Skip over all records on power flow tape until branch data is
C     encountered

      read (l3) scratch
      read (l3) scratch
      lntoto = ntot
      do while (.true.)
        read (l3) scratch
        lntoto = lntoto - 100
        if (lntoto .le. 0) goto 170
      enddo
  170 lntot2 = ntot2
      if (ntot2 .ne. 0) then
        do while (.true.)
          read (l3) scratch
          lntot2 = lntot2 - 100
          if (lntot2 .le. 0) goto 180
        enddo
      endif
  180 if (ntotc .ne. 0) then
        do k = 1, 9
          read (l3) scratch
        enddo
      endif
      if (kxtot .gt. 0) read (l3) scratch

c    Matching identification from LINDAT table with similar
C     identification in branch data record from power flow

c      if (ilgd .eq. 0) then
c        ltoto = ltot
c        if (ltoto .eq. 0) goto 230
c        do while (.true.)
c          read (l3) scratch
c          ltoto = ltoto - 100
c          if (ltoto .le. 0) goto 220
c        enddo
c      else
        do i1 = 1, ltot, 100
          i2 = min0(i1+99, ltot)
          read (l3) ((brnch(j, i), j = 1, 18), i = i1, i2)   
        enddo
c      endif
c 220  continue
      ltoto = ltot
c 230  continue

      read (l3) (nbnew(i), i = 1, ntot)                      
      read (l3) (nbold(i), i = 1, ntotd)                    
      read (l3) scratch
      read (l3) scratch
      read (l3) scratch
      if (jphno .ne. 0) read (l3) scratch
c
c     READING SECOND ENTRY IN KK TABLE
c
      read (l3) ((kka(i, j), i = 1, 2), j = 1, ntot)        
c
c
C     Calculate impedance magnitude and angle for all branches that
C     exist in low to high alpha order.
C     MTBL = packed external power flow numbers.
C     ZIJ = impedance magnitude.
C     PIJ = polar angle in degrees.

      kecsy = kecsym
      kecst = klecs - 1
      is = 1
      if = 4000
      do while (kecsy .gt. 4000)
       read (l3) (buf4k(i), i = is, if)                   
       klecs = klecs + 4000
       kecsy = kecsy - 4000
       is = is + 4000
       if = if + 4000
      enddo
      read (l3) (buf4k(i), i = is, kecsym)
      kecmax = klecs + kecsy
      kecsy = kecsym

 240  continue
c     write(*,'(a)') 'Error - invalid read of bse file...exit'
c 
c     wrapup
c 
 9090 continue

      return
      end
