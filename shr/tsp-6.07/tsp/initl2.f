C    %W% %G%
      subroutine initl2
C    
C     THIS SUBROUTINE REDORDERS VARIOUS DATA TABLES FROM THE
C     EXTERNAL ORDER TO THE INTERNAL SWING ORDER.  IT SERVES AS A
C     CALLING PROGRAM FOR OTHER SUBROUTINES WHICH INITIALIZE THE
C     DC, LOAD, AND RELAY DATA TABLES. IT IS CALLED BY SWINGM.
C     IT CALLS DCINP, LODNUM, MPOST, PRTERR, PRTOUT, R1INT, RDINT,
C     RGINT, RMINT, RRINT, RSINT, RUNIT, RVINT, AND SKIPLN.
C    
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/rrcom.inc'
      include 'tspinc/kntrly.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/ldrep.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/nsavdc.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/shdlod.inc'
      character*8 name1, name2
      character*1 idc
      call mpost('INITL2')
C    
C     CONVERT BUS NUMBERS IN IGENT TO NEW NUMBERS
C    
      do i = 1, isgg
        ibusno = igentn(1, i)
        ibusno = indx2n(ibusno)
        igentn(1, i) = ibusno
      enddo
C    
C     CALL RRINT TO INITIALIZE REMOTE RELAY TABLES
C    
      if (kntrr .ne. 0) call rrint()
C    
C     IF UNDERFREQUENCY GENERATOR RELAYS EXIST CALL RMINT TO
C     INITIALIZE TABLES
C    
      if (ngenf .ne. 0) call rmint()
C    
C     IF VOLTAGE DIFFERENCE LOAD DROPPING RELAYS EXIST CALL RVINT
C     TO INITIALIZE TABLES
C    
      if (kntrv .ne. 0) call rvint()
C    
C     IF UNDERFREQUENCY LOAD SHEDDING (CF1) RELAYS EXIST
C     CALL RSINT TO INITIALIZE TABLES
C    
      if (nufreq .ne. 0) call rsint()
C    
C     INITIALIZE DC CONVERTER BUS NO.TABLE
C    
      ndc = 1
      do i = 1, 200
        nsavdc(i) = 0
      enddo
C    
C     CALL DCINP TO INITIALIZE DC DATA TABLES
C    
      call dcinp()
C    
C     SORT DC CONVERTER BUS NO. TABLE FROM LO TO HI
C    
      if (ndc .ne. 1) then
        ndc1 = ndc - 1
        do while (.true.)
          do n = 1, ndc1
            if (nsavdc(n) .gt. nsavdc(n+1)) then
              nn = nsavdc(n)
              nsavdc(n) = nsavdc(n+1)
              nsavdc(n+1) = nn
            endif
          enddo
          if (ndc1 .eq. 1) goto 100
          ndc1 = ndc1 - 1
        enddo
      endif
C    
C     CALL R1INT TO INITIALIZE POWER RATE RELAY DATA TABLES
C    
  100 if (kntr1 .ne. 0) call r1int()
C    
C     CALL RDINT TO INITIALIZE DISTANCE RELAYS
C    
      if (kntrd .ne. 0) call rdint()
C    
C     CALL RUINT TO INITIALIZE UNDERFREQUENCY LINE TRIPPING RELAYS
C    
      if (kntru .ne. 0) call ruint()
C    
C     CALL RGINT TO INITIALIZE SERIES CAPACITOR GAP MODEL
C    
      if (kntrg .ne. 0) call rgint()
C    
C     CHECK REMOTE RELAY DATA TABLE TO MAKE SURE ALL RR CARDS
C     HAVE BEEN ASSIGNED TO A LOCAL RELAY
C    
      do ktrr = 1, kntrr
        if (rltprr(ktrr) .ne. -1.0) then
          ibus = ibs1rr(ktrr)
          jbus = jbs1rr(ktrr)
          idc = ipr1rr(ktrr)
          name1 = exnamc(ibus)
          name2 = exnamc(jbus)
          kv1 = ixnamn(ibus)
          kv2 = ixnamn(jbus)
          bkv1 = basekv(kv1)
          bkv2 = basekv(kv2)
          write (errbuf(1), 10000) name1, bkv1, name2, bkv2, idc
10000     format (5x, ' CONTROLLING RELAY ', 2(a8, 1x, f5.1), 1x, a1,
     &     'CANNOT BE ', 'FOUND IN THE RELAY DATA TABLES.')
          write (errbuf(2), 10010)
10010     format (5x, ' CHECK REMOTE RELAY DATA CARD COLUMN 5.')
          call prterr('E', 2)
        endif
      enddo
 
C     CONVERT INTEGERS ASSOCIATED WITH LINE SWITCHING TO NEW INTEGERS
C     
      if (iline .gt. lst-1) then
        do i = lst, iline
          ii = ijsln(1, i)
          jj = ijsln(2, i)
          ii = indx2n(ii)
          jj = indx2n(jj)
          ijsln(1, i) = ii
          ijsln(2, i) = jj
        enddo
      endif
C    
C     INDO2X IS EQUIVALENT TO NBNEW ON THE AUXOUT TAPE 
C     INDX2N CONVERTS EXTERNAL TO INTERNAL SWING 
C     CALL LODNUM TO REORDER THE LOAD REPRESENTATION NUMBERS
C    
      if (lrep .ne. 0) call lodnum()
C    
C     CONVERT LOAD SHEDDING BUS NUMBERS
C    
      if (ls .ne. 0) then
        do i = 1, ls
          ii = lshdno(i)
          ii = indx2n(ii)
          lshdno(i) = ii
        enddo
      endif
      write (outbuf, 10020)
      call prtout(1)
      call skipln(1)
10020 format ('0SUBROUTINE I N I T L 2 HAS BEEN PROCESSED')
      return
      end
