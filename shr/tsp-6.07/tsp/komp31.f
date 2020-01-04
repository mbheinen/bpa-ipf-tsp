C    %W% %G%
      function komp31(m, n)

c     Function to check the ascii sort order of word1 and word2. 
C     Returned value is:
c         0 -> (m) = (n)
c        -1 -> (m) < (n)
c        +1 -> (m) > (n)


      include 'tspinc/params.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/in1n.inc'
      include 'tspinc/in1c.inc'
      include 'tspinc/lnet.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/sort.inc'
      include 'tspinc/brakn.inc'

      common /lqiks/lqiks

      common /areac2/areac2(60)
      character*10 areac2
      common /ldnew/basnew(MAXKV)

C     -  Local variables

      character*8 namer
      character*8 nam1er, nam2er, nam3er, nam4er
      character*8 ipr3er, iparer
      logical debug

C     -     Begin     Begin     Begin     Begin     Begin     Begin
      debug = .false.
      iparer = ' '
      ksecer = 0
      komp31 = 0

      if (m .ne. n) then
          if (lqiks .eq. 2) then
            if (kompr(mach2c(1, m), mach2c(1, n), kdum) .lt. 0) goto 120
            if (kompr(mach2c(1, m), mach2c(1, n), kdum) .le. 0) then
              if (mach1n(3, m) .lt. mach1n(3, n)) goto 120
              if (mach1n(3, m) .le. mach1n(3, n)) then
                if (kompr(mach2c(2, m), mach2c(2, n), kdum) .lt. 0) 
     &            goto 120
                if (kompr(mach2c(2, m), mach2c(2, n), kdum) .le. 0) then
                  if (mach1n(2, m) .lt. mach1n(2, n)) goto 120
                  if (mach1n(2, m) .le. mach1n(2, n)) then
c
c                   Check for continuation records
c
                    kdum1 = index ('2', mach1c(1, m)(1:1))
                    kdum2 = index ('2', mach1c(1, n)(1:1))
                    if (kdum1 .lt. kdum2) go to 120
                    if (kdum1 .gt. kdum2) go to 100
                    basemm = basekv(mach1n(3, n))
                    write (errbuf(1), 10180)
                    write (errbuf(2), 10190) mach2c(1, m), basemm, 
     &               mach1c(2, m), m, n
10180               format ('0 THE MACHINE CARD WITH THE FOLLOWING IDENT
     &IFICATION IS A DUPLICATE.')
10190               format (1x, a8, f6.1, 2x, a1, 2i5)
                    call prterr ('E', 2)
                    goto 110
                  endif
                endif
              endif
            endif
          elseif (lqiks .eq. 3) then
            if (kompr(lrep2c(m), lrep2c(n), kdum) .lt. 0) goto 120
            if (kompr(lrep2c(m), lrep2c(n), kdum) .gt. 0) goto 100
            if (lrep2n(m) .lt. lrep2n(n)) goto 120
            if (lrep2n(m) .gt. lrep2n(n)) goto 100
            write (errbuf(1), 10000)
10000       format ('0', 
     &       'THE LOAD REPRESENTATION CARD WITH THE FOLLOWING IDENTICATI
     &ON IS A DUPLICATE.'
     &       )
            namer = lrep2c(m)
            kb = lrep2n(m)
          elseif (lqiks .eq. 4) then
            if (kompr(lnetc(m), lnetc(n), kdum) .lt. 0) goto 120
            if (kompr(lnetc(m), lnetc(n), kdum) .gt. 0) goto 100
            if (lnetn(m) .lt. lnetn(n)) goto 120
            if (lnetn(m) .gt. lnetn(n)) goto 100
            write (errbuf(1), 10010)
10010       format ('0', 
     &       'THE LOAD NETTING BUS WITH THE FOLLOWING IDENTIFICATION IS 
     &A DUPLICATE.'
     &       )
            namer = lnetc(m)
            kb = lnetn(m)
          elseif (lqiks .eq. 5) then
            if (kompr(lshd1c(m), lshd1c(n), kdum) .lt. 0) goto 120
            if (kompr(lshd1c(m), lshd1c(n), kdum) .gt. 0) goto 100
            if (lshd1n(m) .lt. lshd1n(n)) goto 120
            if (lshd1n(m) .gt. lshd1n(n)) goto 100
            if (kompr(lshd2c(m), lshd2c(n), kdum) .lt. 0) goto 120
            if (kompr(lshd2c(m), lshd2c(n), kdum) .gt. 0) goto 100
            write (errbuf(1), 10020)
10020       format ('0', 
     &       'THE LOAD SHEDDING CARD WITH THE FOLLOWING IDENTIFICATION I
     &S A DUPLICATE.'
     &       )
            namer = lshd1c(m)
            kb = lshd1n(m)
            base = basekv(kb)
            write (errbuf(2), 10030) namer, base
10030       format (' ', a8, f6.1)
            istope3 = 1
            call prterr('E', 2)
            goto 110
          elseif (lqiks .eq. 6) then
            if (kompr(lsrc1c(m), lsrc1c(n), kdum) .lt. 0) goto 120
            if (kompr(lsrc1c(m), lsrc1c(n), kdum) .gt. 0) goto 100
            if (lsrc1n(n) .lt. lsrc1n(n)) goto 120
            if (lsrc1n(n) .gt. lsrc1n(n)) goto 100
            if (kompr(lsrc2c(m), lsrc2c(n), kdum) .lt. 0) goto 120
            if (kompr(lsrc2c(m), lsrc2c(n), kdum) .gt. 0) goto 100
            if (lsrc2n(m) .lt. lsrc2n(n)) goto 120
            if (lsrc2n(m) .gt. lsrc2n(n)) goto 100
            if (kompr(lsrc3c(1, m), lsrc3c(1, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(lsrc3c(1, m), lsrc3c(1, n), kdum) .gt. 0) goto 
     &       100
            if (lsrc3n(1, m) .lt. lsrc3n(1, n)) goto 120
            if (lsrc3n(1, m) .gt. lsrc3n(1, n)) goto 100
            write (errbuf(1), 10040)
10040       format ('0', 
     &       'THE SERIES CAPACITOR CARD WITH THE FOLLOWING IDENTIFICATIO
     &N IS A DUPLICATE.'
     &       )
            nam1er = lsrc1c(m)
            nam2er = lsrc2c(m)
            kb1 = lsrc1n(m)
            kb2 = lsrc2n(m)
            if (kb1 .ne. 0) base1 = basekv(kb1)
            if (kb2 .ne. 0) base2 = basekv(kb2)
            write (errbuf(2), 10160) nam1er, base1, nam2er, base2, 
     &       iparer, ksecer
10160       format ('0', a8, f6.1, 2x, a8, f6.1, 2x, a1, 1x, i1)
            if (lrrswt .ne. 0) then
              write (errbuf(2)(41:), 10170) nam3er, base3, nam4er, 
     &         base4, ipr3er, ksc3er
10170         format (a8, f6.1, 2x, a8, f6.1, 2x, a1, 1x, i1)
              lrrswt = 0
            endif
            call prterr('W', 2)
            goto 110
          elseif (lqiks .eq. 7) then
            if (kompr(lrly1c(m), lrly1c(n), kdum) .lt. 0) goto 120
            if (kompr(lrly1c(m), lrly1c(n), kdum) .gt. 0) goto 100
            if (lrly1n(m) .lt. lrly1n(n)) goto 120
            if (lrly1n(m) .gt. lrly1n(n)) goto 100
            if (kompr(lrly2c(m), lrly2c(n), kdum) .lt. 0) goto 120
            if (kompr(lrly2c(m), lrly2c(n), kdum) .gt. 0) goto 100
            if (lrly2n(m) .lt. lrly2n(n)) goto 120
            if (lrly2n(m) .gt. lrly2n(n)) goto 100
            if (kompr(lrly3c(1, m), lrly3c(1, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(lrly3c(1, m), lrly3c(1, n), kdum) .gt. 0) goto 
     &       100
            if (lrly3n(1, m) .lt. lrly3n(1, n)) goto 120
            if (lrly3n(1, m) .gt. lrly3n(1, n)) goto 100
            write (errbuf(1), 10050)
10050       format ('0', 
     &       'THE RELAY CARD WITH THE FOLLOWING IDENTIFICATION IS A DUPL
     &ICATE.'
     &       )
            nam1er = lrly1c(m)
            nam2er = lrly2c(m)
            kb1 = lrly1n(m)
            kb2 = lrly2n(m)
            lrrswt = 0
            if (kb1 .ne. 0) base1 = basekv(kb1)
            if (kb2 .ne. 0) base2 = basekv(kb2)
            write (errbuf(2), 10160) nam1er, base1, nam2er, base2, 
     &       iparer, ksecer
            if (lrrswt .ne. 0) then
              write (errbuf(2)(41:), 10170) nam3er, base3, nam4er, 
     &         base4, ipr3er, ksc3er
              lrrswt = 0
            endif
            call prterr('W', 2)
            goto 110
          elseif (lqiks .eq. 8) then
            if (kompr(lrod1c(m), lrod1c(n), kdum) .lt. 0) goto 120
            if (kompr(lrod1c(m), lrod1c(n), kdum) .gt. 0) goto 100
            if (lrod1n(m) .lt. lrod1n(n)) goto 120
            if (lrod1n(m) .gt. lrod1n(n)) goto 100
            if (kompr(lrod2c(m), lrod2c(n), kdum) .lt. 0) goto 120
            if (kompr(lrod2c(m), lrod2c(n), kdum) .gt. 0) goto 100
            if (lrod2n(m) .lt. lrod2n(n)) goto 120
            if (lrod2n(m) .gt. lrod2n(n)) goto 100
            if (kompr(lrod3c(1, m), lrod3c(1, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(lrod3c(1, m), lrod3c(1, n), kdum) .gt. 0) goto 
     &       100
            if (lrod3n(1, m) .lt. lrod3n(1, n)) goto 120
            if (lrod3n(1, m) .gt. lrod3n(1, n)) goto 100
            if (kompr(lrod4c(m), lrod4c(n), kdum) .lt. 0) goto 120
            if (kompr(lrod4c(m), lrod4c(n), kdum) .gt. 0) goto 100
            if (lrod4n(m) .lt. lrod4n(n)) goto 120
            if (lrod4n(m) .gt. lrod4n(n)) goto 100
            if (kompr(lrod5c(m), lrod5c(n), kdum) .lt. 0) goto 120
            if (kompr(lrod5c(m), lrod5c(n), kdum) .gt. 0) goto 100
            if (lrod5n(m) .lt. lrod5n(n)) goto 120
            if (lrod5n(m) .gt. lrod5n(n)) goto 100
            if (kompr(lrod6c(1, m), lrod6c(1, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(lrod6c(1, m), lrod6c(1, n), kdum) .gt. 0) goto 
     &       100
            if (kompr(lrod6c(2, m), lrod6c(2, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(lrod6c(2, m), lrod6c(2, n), kdum) .gt. 0) goto 
     &       100
            if (lrod6n(m) .lt. lrod6n(n)) goto 120
            if (lrod6n(m) .gt. lrod6n(n)) goto 100
            nam3er = lrod4c(m)
            nam4er = lrod5c(m)
            kb3 = lrod4n(m)
            kb4 = lrod5n(m)
            if (kb3 .ne. 0) base3 = basekv(kb3)
            if (kb4 .ne. 0) base4 = basekv(kb4)
            ipr3er = lrod6c(1, m)
            ksc3er = lrod6n(m)
            write (errbuf(1), 10060)
10060       format ('0', 
     &       'THE REMOTE RELAY CARD WITH THE FOLLOWING IDENTIFICATION IS
     & A DUPLICATE.'
     &       )
            call prterr('E', 1)
            lrrswt = 1
            nam1er = lrod1c(m)
            nam2er = lrod2c(m)
          elseif (lqiks .eq. 9) then
            if (kompr(tempc(m), tempc(n), kdum) .lt. 0) goto 120
            if (kompr(tempc(m), tempc(n), kdum) .gt. 0) goto 100
            if (kompr(tempc(m+ifcd), tempc(n+ifcd), kdum) .lt. 0) goto 
     &       120
            if (kompr(tempc(m+ifcd), tempc(n+ifcd), kdum) .gt. 0) goto 
     &       100
            if (kompr(tempc(m+2*ifcd), tempc(n+2*ifcd), kdum) .lt. 0) 
     &       goto 120
            if (kompr(tempc(m+2*ifcd), tempc(n+2*ifcd), kdum) .gt. 0) 
     &       goto 100
            write (errbuf(1), 10070)
10070       format ('0', 
     &       'THE LINE SWITCHING OR MODIFICATION CARD WITH THE FOLLOWING
     & IDENTIFICATION IS A DUPLICATE.'
     &       )
            nam1er = tempc(m)
            nam2er = tempc(m+ifcd)
            lrrswt = 0
          elseif (lqiks .eq. 10) then
            if (kompr(dzonec(1, m), dzonec(1, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(dzonec(1, m), dzonec(1, n), kdum) .gt. 0) goto 
     &       100
            if (kompr(dzonec(2, m), dzonec(2, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(dzonec(2, m), dzonec(2, n), kdum) .gt. 0) goto 
     &       100
            if (idznen(m) .lt. idznen(n)) goto 120
            if (idznen(m) .gt. idznen(n)) goto 100
            write (errbuf(1), 10080) dzonec(1, m), dzonec(2, m), idznen
     &       (m)
            errbuf(2) = ' '
10080       format ('0', 2a4, i5, 
     &       ' THIS ZONE IS A DUPLICATE OF ANOTHER BELONGINGTHE LOAD REP
     &RESENTATION DATA.'
     &       )
            iabort = 1
            call prterr('E', 2)
            goto 110
          elseif (lqiks .eq. 11) then
            if (kompr(areac2(m), areac2(n), kdum) .lt. 0) goto 120
            if (kompr(areac2(m), areac2(n), kdum) .gt. 0) goto 100
            write (errbuf(1), 10090) areac2(m)
            errbuf(2) = ' '
10090       format ('0', 
     &       ' THE LOAD REPRESENTATION CARD WITH AREA INTERCHANGE (', 
     &       a10, ') IS A DUPLICATE.')
            istop3 = 1
            call prterr('E', 2)
            goto 110
          elseif (lqiks .eq. 12) then
            if (kompr(idcbuc(m), idcbuc(n), kdum) .lt. 0) goto 120
            if (kompr(idcbuc(m), idcbuc(n), kdum) .gt. 0) goto 100
            if (idcbun(m) .lt. idcbun(n)) goto 120
            if (idcbun(m) .gt. idcbun(n)) goto 100
            if (idcinn(1, m) .lt. idcinn(1, n)) goto 120
            if (idcinn(1, m) .gt. idcinn(1, n)) goto 100
            write (errbuf(1), 10100)
10100       format ('0', 
     &       'THE DIRECT CURRENT CARD WITH THE FOLLOWING IDENTIFICATION 
     &IS A DUPLICATE.'
     &       )
            namer = idcbuc(m)
            kb = idcbun(m)
            base = basekv(kb)
            write (errbuf(2), 10030) namer, base
            istope3 = 1
            call prterr('E', 2)
            goto 110
          elseif (lqiks .eq. 13) then
C           -
            call target()
            if (debug) then
              call dbgeko('KOMP31 - comparing load net bus names')
              call dbgwri('  M /subscript of 1st name/ = ', m)
              call dbgwri('  N /subscript of 2nd name/ = ', n)
              if (m .lt. 0) then
                ra = 1.0
                do la = 1, 999999
                  ra = ra*2.0
                enddo
              endif
            endif
C           -
            if (kompr(lnetxc(m), lnetxc(n), kdum) .lt. 0) goto 120
            if (kompr(lnetxc(m), lnetxc(n), kdum) .gt. 0) goto 100
            if (lnetxn(m) .lt. lnetxn(n)) goto 120
            if (lnetxn(m) .gt. lnetxn(n)) goto 100
            goto 110
          elseif (lqiks .eq. 14) then
            if (kompr(msortc(m), msortc(n), kdum) .lt. 0) goto 120
            if (kompr(msortc(m), msortc(n), kdum) .gt. 0) goto 100
            if (msortn(m) .lt. msortn(n)) goto 120
            if (msortn(m) .gt. msortn(n)) goto 100
            if (kompr(msortc(m+MAXLS), msortc(n+MAXLS), kdum) .lt. 0) 
     &       goto 120
            if (kompr(msortc(m+MAXLS), msortc(n+MAXLS), kdum) .gt. 0) 
     &       goto 100
            if (msortn(m+MAXLS) .lt. msortn(n+MAXLS)) goto 120
            if (msortn(m+MAXLS) .gt. msortn(n+MAXLS)) goto 100
            goto 110
          elseif (lqiks .eq. 15) then
            if (basnew(m) .lt. basnew(n)) goto 120
            if (basnew(m) .gt. basnew(n)) goto 100
            goto 110
          elseif (lqiks .eq. 16) then
            if (irbidn(1, m) .lt. irbidn(1, n)) goto 120
            if (irbidn(1, m) .gt. irbidn(1, n)) goto 100
            if (irbidn(2, m) .lt. irbidn(2, n)) goto 120
            if (irbidn(2, m) .gt. irbidn(2, n)) goto 100
            if (kompr(ircidc(m), irbidc(n), kdum) .lt. 0) goto 120
            if (kompr(ircidc(m), irbidc(n), kdum) .gt. 0) goto 100
            goto 110
          elseif (lqiks .eq. 17) then
            if (ircidn(1, m) .lt. ircidn(1, n)) goto 120
            if (ircidn(1, m) .gt. ircidn(1, n)) goto 100
            if (ircidn(2, m) .lt. ircidn(2, n)) goto 120
            if (ircidn(2, m) .gt. ircidn(2, n)) goto 100
            if (kompr(irbidc(m), ircidc(n), kdum) .lt. 0) goto 120
            if (kompr(irbidc(m), ircidc(n), kdum) .gt. 0) goto 100
            goto 110
          elseif (lqiks .eq. 18) then
            if (kompr(ufreqc(m), ufreqc(n), kdum) .lt. 0) goto 120
            if (kompr(ufreqc(m), ufreqc(n), kdum) .gt. 0) goto 100
            if (lufrqn(m) .lt. lufrqn(n)) goto 120
            if (lufrqn(m) .gt. lufrqn(n)) goto 100
            write (errbuf(1), 10110)
10110       format ('0', 
     &       'THE LOAD SHEDDING RELAY CF-1 CARD WITH THE FOLLOWING IDENT
     &IFICATION IS A DUPLICATE.'
     &       )
            namer = ufreqc(m)
            kb = lufrqn(m)
            base = basekv(kb)
            write (errbuf(2), 10030) namer, base
            istope3 = 1
            call prterr('E', 2)
            goto 110
          elseif (lqiks .eq. 19) then
            if (kompr(ugenfc(1, m), ugenfc(1, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(ugenfc(1, m), ugenfc(1, n), kdum) .gt. 0) goto 
     &       100
            if (iugenn(m) .lt. iugenn(n)) goto 120
            if (iugenn(m) .gt. iugenn(n)) goto 100
            if (kompr(ugenfc(2, m), ugenfc(2, n), kdum) .lt. 0) goto 
     &       120
            if (kompr(ugenfc(2, m), ugenfc(2, n), kdum) .gt. 0) goto 
     &       100
            write (errbuf(1), 10120)
            write (errbuf(2), 10130) ugenfc(1, m), basekv(iugenn(m)), 
     &       ugenfc(2, m)
10120       format ('0', 
     &       'THE GENERATOR UNDERFREQUENCY RELAY CARD WITH THE FOLLOWING
     & IDENTIFICATION IS A DUPLICATE.'
     &       )
10130       format (1x, a8, f6.1, 2x, a1)
            call prterr('W', 2)
            goto 110
          elseif (lqiks .eq. 20) then
            base1 = reappx(m, 2)
            base2 = reappx(n, 2)
            if (base1 .lt. base2) goto 120
            if (base1 .gt. base2) goto 100
            base3 = reappx(m, 3)
            base4 = reappx(n, 3)
            if (base3 .lt. base4) goto 120
            if (base3 .gt. base4) goto 100
            write (errbuf(1), 10140)
10140       format ('0', 
     &       'ONE OF THE LINE RESISTANCE APPROXIMATION DATA CARDS IS A D
     &UPLICATE.'
     &       )
            call prterr('W', 2)
            goto 110
          elseif (lqiks .eq. 21) then
            if (kompr(izn1c(m), izn1c(n), kdum) .lt. 0) goto 120
            if (kompr(izn1c(m), izn1c(n), kdum) .gt. 0) goto 100
            if (izn1n(n) .lt. izn1n(n)) goto 120
            if (izn1n(n) .gt. izn1n(n)) goto 100
            if (kompr(izn2c(m), izn2c(n), kdum) .lt. 0) goto 120
            if (kompr(izn2c(m), izn2c(n), kdum) .gt. 0) goto 100
            if (izn2n(m) .lt. izn2n(n)) goto 120
            if (izn2n(m) .gt. izn2n(n)) goto 100
            if (kompr(izn3c(1, m), izn3c(1, n), kdum) .lt. 0) goto 120
            if (kompr(izn3c(1, m), izn3c(1, n), kdum) .gt. 0) goto 100
            if (kompr(izn3c(2, m), izn3c(2, n), kdum) .lt. 0) goto 120
            if (kompr(izn3c(2, m), izn3c(2, n), kdum) .gt. 0) goto 100
            if (izn3n(1, m) .lt. izn3n(1, n)) goto 120
            if (izn3n(1, m) .gt. izn3n(1, n)) goto 100
            write (errbuf(1), 10150)
10150       format ('0', 
     &       'THE ZNO CAPACITOR CARD WITH THE FOLLOWING IDENTIFI', 
     &       'CATION IS A DUPLICATE.')
            nam1er = izn1c(m)
            nam2er = izn2c(m)
            kb1 = izn1n(m)
            kb2 = izn2n(m)
            if (kb1 .ne. 0) base1 = basekv(kb1)
            if (kb2 .ne. 0) base2 = basekv(kb2)
            write (errbuf(2), 10160) nam1er, base1, nam2er, base2, 
     &       iparer, ksecer
            if (lrrswt .ne. 0) then
              write (errbuf(2)(41:), 10170) nam3er, base3, nam4er, 
     &         base4, ipr3er, ksc3er
              lrrswt = 0
            endif
            call prterr('W', 2)
            goto 110
          else
            if (basekv(m) .lt. basekv(n)) goto 120
            if (basekv(m) .gt. basekv(n)) goto 100
            goto 110
          endif

  100   komp31 =  + 1
        goto 130
  110   komp31 = 0
        goto 130
  120   komp31 =  - 1
        goto 130
      endif
  130 return
      end
