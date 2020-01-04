C    %W% %G%
      subroutine lschk
C     
C     THIS SUBROUTINE CHECKS THE LS CARDS FOR ERRORS IN LOAD
C     OR GENERATION MODIFICATION , FAULT DAMPING, FAST VALVING,
C     OR TSL TRIGGER. IT IS CALLED BY INITL4.
C     
      include 'tspinc/params.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/ldidxn.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/int3.inc'

      character*1 idm

      call mpost('LSCHK')
C     
C     VERIFYING GENERATORS MODIFIED BY THE LS CARD
C     EXIST IN THE GENERATOR TABLE
C     
      do i = 1, ifcd
        if (mflt(i) .eq. 4) then
          if (dmpg(i) .ne. 0.0) then
            if (cyc(i) .le. -8000.0) then
              cyc(i) = cyc(i) + 10000.0
            else
              iabort = 1
              j = iftabn(i)
              id = idgnc(i)
              name = bname(j)
              base = buskv(j)
              write (errbuf(1), 10000) name, base, id
              call prterr('E', 1)
10000         format ('0', 'THE LS CARD FOR GENERATOR DROPPING (', a8,
     &         2x, f6.1, 2x, a1, 2x,
     &         ') CAN NOT BE LOCATED IN LIST OF GENERATORS.')
            endif
          endif
        endif
      enddo
C     
C     VERIFY THAT PROPER LOAD REPRESENTATION EXISTS FOR A BUS WITH
C     MANUAL LOAD MODIFICATION.  CONVERT REACTIVE POWER FROM
C     FROM PER UNIT AT 1.0 PU VOLTAGE TO PER UNIT AT SOLUTION VOLT
C     
      do l = 1, ifcd
        if (mflt(l) .eq. 4) then
          igl = iftab(l)
          name = bname(igl)
          base = buskv(igl)
          iecsl = ldidxn(6, igl)
          vsq = (eyr(igl)*eyr(igl)+eyi(igl)*eyi(igl))
          pp = dmpln(1, l)
          pq = dmpln(2, l)
C         * * *
C         * * * CHECK FOR CONSTANT POWER REPRESENTATION
C         * * *
          if (pp .ne. 0.0 .or. pq .ne. 0.0) then
            ityp3 = ldidxn(2, igl)
            if (ityp3 .ne. 3 .or. iecsl .eq. 0) then
              write (errbuf(1), 10010) name, base
              call prterr('E', 1)
10010         format ('0', 5x, 'ERROR IN LOAD MODIFICATION CARD.  BUS '
     &         , a8, 1x, f5.1, ' HAS NO CONSTANT POWER REPRESENTATION.'
     &         )
              iabort = 1
            endif
C           
C           IF SHUNT LOAD CONVERT TO SOLUTION VOLTAGE VALUE
C           
            if (idgnc(l) .eq. 'S') then
              dmpln(1, l) = dmpln(1, l)*vsq
              dmpln(2, l) = dmpln(2, l)*vsq
            endif
          endif
          cp = dmcln(1, l)
          cq = dmcln(2, l)
C         
C         CHECK FOR CONSTANT CURRENT REPRESENTATION
C         
          if (cp .ne. 0.0 .or. cq .ne. 0.0) then
            ityp4 = ldidxn(1, igl)
            if (ityp4 .ne. 4 .or. iecsl .eq. 0) then
              write (errbuf(1), 10020) name, base
              call prterr('E', 1)
10020         format ('0', 5x, 'ERROR IN LOAD MODIFICATION CARD.  BUS '
     &         , a8, 1x, f5.1,
     &         ' HAS NO CONSTANT CURRENT REPRESENTATION.')
              iabort = 1
            endif
C           
C           IF SHUNT LOAD CONVERT TO SOLUTION VOLTAGE VALUE
C           
            if (idgnc(l) .eq. 'S') then
              dmcln(1, l) = dmcln(1, l)*vsq
              dmcln(2, l) = dmcln(2, l)*vsq
            endif
          endif
C         
C         CHECK FOR CONSTANT IMPEDANCE REPRESENTATION
C         
          pz = dmzg(l)
          qz = dmzb(l)
          if (pz .ne. 0.0 .or. qz .ne. 0.0) then
            if (idgnc(l) .eq. 'S') then
              dmzg(l) = dmzg(l)*vsq
              dmzb(l) = dmzb(l)*vsq
            elseif (ipwr .ne. 1) then
              if (iecsl .ne. 0) then
                ityp1 = ldidxn(4, igl)
                if (ityp1 .ne. 1) then
C                 
C                 IF SHUNT LOAD CONVERT TO SOLUTION VOLTAGE VALUE
C                 
                  write (errbuf(1), 10030) name, base
                  call prterr('E', 1)
10030             format ('0', 5x,
     &             'ERROR IN LOAD MODIFICATION CARD.  BUS ', a8, 1x,
     &             f5.1, ' HAS NO CONSTANT IMPEDANCE REPRESENTATION.')
                  iabort = 1
                endif
              endif
            endif
          endif
        endif
      enddo
      do i = 1, 5
        i2 = 2*i
        iflt(i) = 0
        ifvl(i) = 0
        dflt(i) = 0.0
        dfvl(i) = 0.0
        dfvl(i2) = 0.0
        ifvl(i2) = 0
      enddo
C     
C     STORE FAULT DAMPING AND FAST VALVING INFO IN TABLES
C     
      i7 = 0
      i8 = 0
      do i = 1, ifcd
        if (.not. (mflt(i) .lt. 7 .or. mflt(i) .gt. 8)) then
          ifvlt = iftab(i)
          ifind = 0
          name = bname(iftab(i))
          bkv = buskv(iftab(i))
          id = idgnc(i)
          do j = 1, isg
            igbn = igentn(1, j)
            iecs = igentn(2, j)
            idm = igentc(j)
            if (ifvlt .eq. igbn) then
              if (idm .eq. idgnc(i)) goto 100
            endif
          enddo
          if (ifind .eq. 0) then
            write (errbuf(1), 10040) name, bkv, id
10040       format (' GENERATOR ', a8, 1x, f5.1, 1x, a1,
     &       ' ON THE LS CARD ',
     &       'CANNOT BE FOUND IN THE GENERATOR TABLE.')
            call prterr('E', 1)
            iabort = 1
          endif
          goto 110
  100     ifind = 1
          if (mflt(i) .eq. 8) then
            dmpg(i) = dmpg(i)*govpwr(j)/100.
            do ll = 1, i8
              if (ifvl(ll) .eq. j) goto 110
            enddo
            i8 = i8 + 1
            if (i8 .gt. 10) goto 120
            ifvl(i8) = j
C           
C           DISABLE GOVERNOR AT GENERATOR WHICH HAS FAST VALVING
C           
            igndta(3, j) = 0
            igndta(4, j) = 0
            write (errbuf(1), 10050) name, bkv, id
            call prterr('W', 1)
10050       format ('0', ' GOVERNOR IF ANY IS DISABLED FOR GENERATOR ',
     &       a8, 1x, f5.1, 1x, a1, ' BECAUSE IT IS USING FAST VALVING')
          else
C           
C           CHECK TO SEE IF THIS BUS HAS ALREADY BEEN MENTIONED FO
C           OR IN FAST VALVING. IF YES THEN SKIP IT.
C           
            do kkk = 1, i7
              if (iflt(kkk) .eq. j) goto 110
            enddo
            i7 = i7 + 1
            if (i7 .gt. 5) goto 120
            iflt(i7) = j
          endif
        endif
  110   continue
      enddo
      i7max = i7
      i8max = i8
      goto 130
  120 write (errbuf(1), 10060)
      call prterr('E', 1)
10060 format ('0', '   ERROR    ONLY 5 AND 10 M/CS ALLOWED FOR FLT ',
     & 'DAMPING AND FAST VALVING RESPECTIVELY ')
      iabort = 1
C     
C     VERIFY THAT THE GENERATORS FOR ALL MANUAL TSL TRIGGER CARDS
C     HAVE BEEN FOUND.
C     
  130 continue
      do itrr = 1, ifcd
        if (mflt(itrr) .eq. 9) then
          if (ipcdtn(itrr) .eq. 0) then
            if (jftab(itrr) .eq. 0) then
              name = bname(iftab(itrr))
              bkv = buskv(iftab(itrr))
              idm = idgnc(itrr)
              write (errbuf(1), 10070)
10070         format (
     &         ' A MANUAL TRANSIENT STABILIZER DROPPING CARD WAS ',
     &         ' SUBMITTED FOR GENERATOR ')
              write (errbuf(2), 10080) name, bkv, idm
10080         format (5x, a8, 2x, f5.1, 2x, a1,
     &         ' THE GENERATOR CANNOT BE FOUND ', 'OR HAS NO TSL.')
              call prterr('E', 2)
              iabort = 1
            endif
          endif
        endif
      enddo
C     
C     VERIFY THAT THE GENERATORS FOR ALL MANUAL TSL TRIGGER CARDS
C     HAVE BEEN FOUND.
C     
      do itrr = 1, ifcd
        if (mflt(itrr) .eq. 9) then
          if (ipcdtn(itrr) .eq. 1) then
            if (jftab(itrr) .eq. 0) then
              name = bname(iftab(itrr))
              bkv = buskv(iftab(itrr))
              idm = idgnc(itrr)
              write (errbuf(1), 10090)
10090         format (' A MANUAL SVS FREEZE CARD WAS SUBMITTED FOR ')
              write (errbuf(2), 10100) name, bkv, idm
10100         format (5x, a8, 2x, f5.1, 2x, a1,
     &         ' THIS SVS CANNOT BE FOUND. ')
              call prterr('E', 2)
              iabort = 1
            endif
          endif
        endif
      enddo
      return
      end
