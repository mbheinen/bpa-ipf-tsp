C    %W% %G%
      subroutine dcsort
C     
c     This subroutine reads all the dc data cards from scratch
c     file1, performs some error checking, and stores the
c     card images in data tables.  It is called by INPUT2.
C     
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/dcinfo.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/reread.inc'

C     Local variables

      character*1 sbtypc, poric
      character*8 name1c, name2c, nam22c, nme23c
      logical debug
 
C     Begin     Begin     Begin     Begin     Begin     Begin

      debug = .false.
      jdctot = jdc
      if (jdctot .ne. 0) then
        kwords = 8*jdctot
        call whrec1('DC ', ilo, ihi, isz)
        ihi = ihi + 1
        if (debug) then
          call dbgeko('DCSORT - reading DC stuff from temp file # 1')
          call dbgwri('  record # = ', ihi)
        endif
        read (l1, rec = ihi) ((workx(ll, nn), ll = 1, 8), nn = 1,
     &   jdctot)

C       read (l1,REC=188)((WORKX(LL,NN),LL=1,8),NN=1,JDCTOT)

        l = 0
        lb = 0
        i = 0
        iswt = 0
        do while (.true.)
          i = i + 1
          if (i .gt. jdctot) goto 140
          i3b = 0
          i2b = 0
          i1b = 0
          l = l + 1
          j = i
          k = i
          kk = 0
          kkk = 0
          lbv = 0
          lbc = 0
          lbp = 0
          k4 = 0
          k5 = 0
          k6 = 0
          read (work80(j), 10000) sbtypc, name1c, base1
10000     format (bz, 1x, a1, 1x, a8, f4.0)
          buffer = work80(j)
          kb1 = nambas(base1)
          i1b = inam(name1c, kb1)
          if (i1b .eq. 0) then
            write (outbuf, 10010) (workx(ir, j), ir = 1, 8)
            call prtout(1)
          endif
10010     format (1x, 8a10)
          if (sbtypc .ne. ' ' .and. sbtypc .ne. 'A' .and. sbtypc .ne.
     &     'B') then
            write (errbuf(1), 10020) name1c, base1
            call prterr('E', 1)
10020       format ('0 THE D (BLANK) CARD IS MISSING FOR (', a8, 2x,
     &       f5.1, ')')
            iabort = 1
            iswt = 1
          else
            if (sbtypc .eq. 'B') then
              read (work80(j), 10030) poric
10030         format (16x, a1)
            else
              read (work80(j), 10040) poric
10040         format (45x, a1)
            endif
            if (poric .ne. 'P' .and. poric .ne. 'I') then
              write (errbuf(1), 10050) (workx(jx, j), jx = 1, 8)
              write (errbuf(2), 10060)
              call prterr('E', 2)
10050         format (1x, 8a10)
10060         format (
     &         ' DC DATA CARD REQUIRES CONSTANT P OR I SPECIFICTAION')
              iabort = 1
            endif
            do while (.true.)
              jj = i + 1
              i = i + 1
              if (jj .gt. jdctot) goto 110
              read (work80(jj), 10000) sbtypc, name2c, base2
              if (sbtypc .eq. ' ' .or. sbtypc .eq. 'A' .or. sbtypc .eq.
     &         'B') goto 100
              buffer = work80(jj)
              kb2 = nambas(base2)
              ixb = inam(name2c, kb2)
              if (ixb .eq. 0) then
                write (outbuf, 10010) (workx(ir, jj), ir = 1, 8)
                call prtout(1)
              endif
              if (name1c .ne. name2c) goto 120
              if (kb1 .ne. kb2) goto 120
C             
C              'DC' CARD IS THE DETAILED VDCL AND CHANGE CARD
C             
              if (sbtypc .eq. 'C') then
                k4 = jj
                lbc = lbc + 1
C               
C                'DV' CARD IS VOLTAGE CONTROL
C               
              elseif (sbtypc .eq. 'V') then
                k5 = jj
                lbv = lbv + 1
C               
C                'DP' CARD APPLIES PULSE TO GAMMA OR ALPHA
C               
              elseif (sbtypc .eq. 'P') then
                k6 = jj
                lbp = lbp + 1
              else
 
                read (work80(jj), 10070) lohi, nam22c, base22, nme23c,
     &           base23
10070           format (bz, 53x, i1, 1x, a8, f4.0, 1x, a8, f4.0)
                kk = jj
C               
C                THIS IS A GAMMA MODULATION CARD
C               
                if (lohi .eq. 5) then
                  i2b = 0
                  kkk = 0
                  if (.not. (nme23c .eq. ' ' .and. base23 .eq. 0.0))
     &             then
                    kb23 = nambas(base23)
                    i3b = inam(nme23c, kb23)
                    if (i3b .eq. 0) then
                      write (outbuf, 10010) (workx(ir, jj), ir = 1, 8)
                      call prtout(1)
                    endif
                  endif
                else
                  if (.not. (nam22c .eq. ' ' .and. base22 .eq. 0.0))
     &             then
                    buffer = work80(jj)
                    kb22 = nambas(base22)
                    i2b = inam(nam22c, kb22)
                    if (i2b .eq. 0) then
                      write (outbuf, 10010) (workx(ir, jj), ir = 1, 8)
                      call prtout(1)
                    endif
                  endif
                  if (.not. (nme23c .eq. ' ' .and. base23 .eq. 0.0))
     &             then
                    kb23 = nambas(base23)
                    i3b = inam(nme23c, kb23)
                    if (i3b .eq. 0) then
                      write (outbuf, 10010) (workx(ir, jj), ir = 1, 8)
                      call prtout(1)
                    endif
                    lb = lb + 1
                  endif
                  kkk = 0
                endif
              endif
            enddo
  100       i = i - 1
C           
C            IDCBUC CONTAINS THE BUS NAMES OF ALL DC CONVERTERS
C            IDCBUN CONTAINS THE BASE KVS OF ALL DC CONVERTERS
C            IDCINN(1,NN) CONTAINS THE SECOND REMOTE BUS NUMBER
C            ON THE DS CARD
C            IDCINN(2,NN) CONTAINS THE BUS NUMBER OF THE FIRST REMO
C            ON THE MODULATION CARD (DS)
C            IDCINN(3,NN) CONTAINS THE BUS NUMBER OF THE CONVERTER
C            IDCECN(1,NN) CONTAINS THE INDEX TO THIS CONVERTER IN T
C            DCCARD TABLE. IDCECN(2,NN) CONTAINS THE INDEX TO THE F
C            CARD FOR THIS CONVERTER IN THE DCCARD TABLE.
C            IDCECN(3,NN) CONTAINS THE INDEX TO THE SECOND DS CARD
C            CONVERTER IN THE DCCARD TABLE
C            IDCECN(4,NN) INDEX TO D(C) CARD IN DCCARD TABLE
C            IDCECN(5,NN) INDEX TO D(V) CARD IN DCCARD TABLE
C            IDCECN(6,NN) INDEX TO D(P) CARD IN DCCARD TABLE
C           
  110       idcbuc(l) = name1c
            idcbun(l) = kb1
            idcinn(1, l) = i3b
            idcinn(2, l) = i2b
            idcinn(3, l) = i1b
            idcecn(1, l) = k
            idcecn(2, l) = kk
            idcecn(3, l) = kkk
            idcecn(4, l) = k4
            idcecn(5, l) = k5
            idcecn(6, l) = k6
            goto 130
  120       write (errbuf(1), 10080)
            call prterr('E', 1)
10080       format (
     &       '0 THE NAMES ON THE D(BLANK) AND DS CARDS DO NOT AGREE.')
            iabort = 1
            iswt = 1
          endif
  130     continue
        enddo
  140   if (iswt .eq. 0) then
          jdc = l
          jdcs = lb
          if (jdcs .gt. jdc) then
            write (errbuf(1), 10090)
            call prterr('E', 1)
10090       format ('0 YOU HAVE MORE DS CARDS THAN D(BLANK) CARDS.')
            iabort = 1
          endif
          if (lbc .gt. jdc) then
            write (outbuf, 10100) lbc, jdc
10100       format (1x, 'YOU HAVE MORE D(C) CARDS THAN D(BLANK) CARDS',
     &       i2, 1x, i2)
            iabort = 1
            call prterr('E', 1)
          endif
 
          if (lbv .gt. jdc) then
            write (outbuf, 10110) lbv, jdc
10110       format (1x, 'YOU HAVE MORE D(V) CARDS THAN D(BLANK)CARDS',
     &       i2, 1x, i2)
            iabort = 1
            call prterr('E', 1)
          endif
        else
          do k = 1, jdctot
            write (outbuf, 10010) (workx(j, k), j = 1, 8)
            call prtout(1)
          enddo
          jdc = 0
          jdctot = 0
          write (errbuf(1), 10120)
          write (errbuf(2), 10130)
          call prterr('E', 2)
10120     format (
     &     '0  EVEN THOUGH SOME DC CARDS MAY EXIST,THEIR COUNT HAS',
     &     ' BEEN SET TO 0')
10130     format ('  HENCE, AN ERROR MESSAGE IN SUBROUTINE ',
     &     ' INITAL2 WILL APPEAR.')
        endif
      endif
      return
      end
