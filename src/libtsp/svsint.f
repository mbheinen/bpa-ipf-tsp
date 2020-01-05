C    %W% %G%
      subroutine svsint

C     THIS SUBROUTINE INITIALIZES THE STATIC VAR SOURCE
C     DATA TABLES.  IT IS CALLED BY INITL3.

      include 'tspinc/params.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/svs.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/packtn.inc'
      include 'tspinc/int3.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/comn34.inc'

      character*8 nameln
      dimension bsbr(200)
      dimension jbus(200), gij(199), bij(198)
      equivalence (bsbr, jbus, pnet), (bsbr(2), gij, qnet), (bsbr(3), 
     &            bij, gii), (bsbr(4), bii), (bsbr(9), pload), (bsbr
     &            (10), qload)

      ksv = igndta(2, j)
      vhr = eyr(i)
      vhi = eyi(i)
      vsq = vhr*vhr + vhi*vhi
      emag = 1./sqrt(vsq)
      sig1 = 0.0
      sig2 = 0.0
      x4 = 0.0
      x2 = 0.0
      vscs(ksv) = 0.0
      sigo1(ksv) = 0.0
      sigo2(ksv) = 0.0

C     CONVERT ALL BUS NUMBERS TO NEW ORDER

      if (isvno(ksv) .ne. 0) isvno(ksv) = indx2n(isvno(ksv))
      if (ispcde(ksv) .ne. 0) then

C       SUPPLEMENTAL SIGNAL INITIALIZATION--SVS

        if (isupb(ksv) .ne. 0) then

C         INITIALIZE D,E,OR F OPTION

          if (isupb(ksv) .eq. 2) then

C           SVS VAR INPUT OPTION E

            sig1 = qgen
          elseif (isupb(ksv) .eq. 3) then

C           SVS CURRENT INPUT OPTION F

            cr = vhi*qgen/vsq
            ci =  - vhr*qgen/vsq
            sig1 = sqrt(cr*cr+ci*ci)
          else

C           REMOTE VOLTAGE INPUT OPTION D

            if (iremte(ksv) .ne. 0) ire = indx2n(iremte(ksv))
            eik = eyr(ire)
            fik = eyi(ire)
            sig1 = sqrt(eik*eik+fik*fik)
          endif

C         CALCULATE INITIAL STATE VARIABLES FOR SUPPLEMENTAL LOGIC D E F

          x3 = sig1*cks2(ksv)
          x4 = x3

C         CALCULATE PAST VALUES FOR SUPPLEMENTAL LOGIC D E F

          bs10(ksv) = (as10(ksv)-2.)*x3 + cks2(ksv)*sig1
          bs1112(ksv) = (as12(ksv)-2.)*x4 - (as11(ksv)-2.0)*x3

        endif

C       INITIALIZE SUPPLEMENTAL SIGNAL LOGIC FOR TYPE A B AND C
        if (isupt(ksv) .eq. 2) then

C         LINE POWER OPTION B

          if (irmote(ksv) .ne. 0) then
            irmote(ksv) = indx2n(irmote(ksv))
            if (jrmote(ksv) .ne. 0) then
              jrmote(ksv) = indx2n(jrmote(ksv))
              ibus = irmote(ksv)
              jbs = jrmote(ksv)
              call redecs(bsbr, kkp(1, ibus)+kecst, kkp(2, ibus))
              nbr = kkp(2, ibus) - 2
              do ind1 = 13, nbr, 3
                if (jbus(ind1) .eq. jbs) goto 100
              enddo
              goto 110
  100         svgij(ksv) =  - gij(ind1)
              svbij(ksv) =  - bij(ind1)
              svgio(ksv) = 0.0
              svbio(ksv) = 0.0
              ei = eyr(ibus)
              fi = eyi(ibus)
              ej = eyr(jbs)
              fj = eyi(jbs)
              cr = (ei-ej)*svgij(ksv) - (fi-fj)*svbij(ksv)
              ci = (ei-ej)*svbij(ksv) + (fi-fj)*svgij(ksv)
              sig2 = ei*cr + fi*ci
              call redecs(bsbr, kkp(1, i)+kecst, kkp(2, i))
              goto 130
            endif
          endif
  110     iabort = 1
          write (errbuf(1), 10000) name, base, id
10000     format ('0', 5x, a8, 1x, f5.1, 1x, a1)
          write (errbuf(2), 10010)
10010     format ('o', 5x, 
     &     'UNABLE TO FIND REMOTE LINE SPECIFIED ON SUPPLEMENT', 
     &     'AL SIGNAL CARD')
          call prterr('E', 2)
          call redecs(bsbr, kkp(1, i)+kecst, kkp(2, i))
        elseif (isupt(ksv) .eq. 3) then

C         DELTA FREQUENCY OPTION C

          sig2 = 0.0
          if (irmote(ksv) .ne. 0) irmote(ksv) = indx2n(irmote(ksv))
          ibus = irmote(ksv)
          er = eyr(ibus)
          ei = eyi(ibus)
          eang = atan2(ei, er)
          svango(ksv) = eang
        else

C         GENERATOR ACCELERATING POWER OPTION A

          sig2 = 0.0
          if (irmote(ksv) .gt. 0) irmote(ksv) = indx2n(irmote(ksv))
          ibus = irmote(ksv)
          if (ipactn(1, ibus) .eq. 1) then
            irbus(ksv) = ipactn(2, ibus)
          else
            do itr = 1, ipactn(1, ibus)
              im = ipactn(2, ibus) + itr
              if (igentn(1, im) .eq. ibus) goto 120
            enddo
            iabort = 0
            nameln = exnamc(ibus)
            kvcode = ixnamn(ibus)
            write (errbuf(1), 10020) nameln, basekv(kvcode), iparsv
     &       (ksv), name, base
10020       format ('0', 5x, 'GENERATOR ', a8, 1x, f5.1, a1, 
     &       ' COULD NOT BE ', 'FOUND.')
            write (errbuf(2), 10030) name, base
10030       format ('0', 5x, 'FOR SVS ', a8, f5.1)
            call prterr('E', 2)
            goto 130
  120       irbus(ksv) = im
          endif
          if (irbus(ksv) .eq. 0) iabort = 1
        endif

C       INITIALIZE STATE VARIABLES FOR SUPPLEMENTAL OPTIONS A B C

  130   x1 = sig2*cks1(ksv)
        x2 = x1

        sigo1(ksv)  = sig1
        sigo2(ksv)  = sig2
        sigoo1(ksv) = sig1
        sigoo2(ksv) = sig2

C       INITALIZE PAST VALUES FOR SUPPLEMENTAL LOGIC

        bs7(ksv) = cks1(ksv)*sig2 + x1*(as7(ksv)-2.)
        bs89(ksv) = x2*(as9(ksv)-2.) - x1*(as8(ksv)-2.0)
        x5 = x4 + x2
        x6 = 0.0
        bs1314(ksv) = x6*( as14(ksv)-2. ) - x5*as13(ksv)
        vscs(ksv) = cks3(ksv)*x6
        if (vscs(ksv) .gt. vscsmx(ksv)) then
          write (errbuf(1), 10040) name, base, id, vscs(ksv), vscsmx
     &     (ksv)
10040     format ('o', 5x, a8, 1x, f5.1, 1x, a1, ' INITIAL VSCS ', 
     &     e13.6, 2x, 'VIOLATES VSCSMX ', 2x, e13.6)
          call prterr('E', 1)
          iabort = 1
          jabort = 1
        endif
        if (vscs(ksv) .lt. -vscsmx(ksv)) then
          write (errbuf(1), 10050) name, base, id, vscs(ksv), vscsmx
     &     (ksv)
10050     format ('0', 5x, a8, 1x, f5.1, 1x, a1, 'INITAL VSCS ', e13.6, 
     &     2x, 'VIOLATES VSCSMIN ', 2x, e13.6)
          call prterr('E', 1)
          iabort = 1
          jabort = 1
        endif
      endif

C     INITIALIZATION OF STATIC VAR SOURCE LOGIC

      em = 1./emag
      if (iremte(ksv) .ne. 0) then
        iremte(ksv) = indx2n(iremte(ksv))
        ibus = iremte(ksv)
        eik = eyr(ibus)
        fik = eyi(ibus)
        em = sqrt(eik*eik+fik*fik)
      endif
      svsetn(ksv)  = em
      etermo(ksv)  = em
      etermoo(ksv) = em
      bsvs(ksv) = qgen/vsq

C     NEWTONS ADD.

      if (bsvs(ksv) .gt. bmax(ksv) .or. bsvs(ksv) .lt. bmin(ksv)) then
        write (errbuf(1), 10060) name, base, id, bsvs(ksv), bmax(ksv), 
     &   bmin(ksv)
10060   format ('0', 5x, a8, 2x, f5.1, 2x, a1, 2x, 
     &   'INITIAL VAR OUTPUT ', e13.6, 2x, 'VIOLATES LIMITS ', 2x, 
     &   e13.6, 2x, e13.6)
        call prterr('E', 1)
        iabort = 1
        jabort = 1
      endif
      brp(ksv) = bsvs(ksv)

C     ASSUME VERR .GT. -DV AND VERR .LT. DV

      br = brp(ksv)
      if (cksvs(ksv) .gt. 0.0) then
        x4 = br/cksvs(ksv)
      else
        x4 = 0.0
      endif

      x3 = x4
      x2 = x3

      if (x4 .gt. vemax(ksv) .or. x4 .lt. vemin(ksv)) then
        write (errbuf(1), 10070) name, base, id, x2, vemax(ksv), vemin
     &   (ksv)
10070   format ('0', 5x, a8, 2x, f5.1, 2x, a1, 2x, 
     &   'INITIAL VALUE OF X4 ', 2x, f13.6, 2x, 'VIOLATES VEMAX VEMIN', 
     &   2x, e13.6, 2x, e13.6)
        call prterr('E', 1)
        iabort = 1
        jabort = 1
      endif

      x1 = em
      verr = x2 - vscs(ksv)
      svref(ksv) = verr + x1
      if (cksvs(ksv) .eq. 0.0) vscs(ksv) = br

      if (verr .gt. dvlo(ksv) .or. verr .lt. dvhi(ksv)) then
        write (errbuf(1), 10080) name, base, id, verr, dvlo(ksv), dvhi
     &   (ksv)
10080   format ('o', 5x, a8, 2x, f5.1, 2x, a1, 2x, 'VERR ', 2x, e13.6, 
     &   2x, 'VIOLATES', 'DVLO OR DVHI', 2e13.6, 2x, e13.6)
        call prterr('E', 1)
        iabort = 1
        jabort = 1
      endif

C     CALCULATE PAST VALUES

      bsv1(ksv) = (as1(ksv)-2.)*x1 + etermo(ksv)
      bsv2(ksv) = (as3(ksv)-2.)*x3 - x2*(as2(ksv)-2.)
      bsv3(ksv) = (as5(ksv)-2.)*x4 - (as4(ksv)-2.)*x3
      bsv4(ksv) = (as6(ksv)-2.)*bsvs(ksv) + brp(ksv)

C     CALCULATE CURRENT INJECTION

      eyri(j) = bsvs(ksv)*eyi(i)
      eyii(j) =  - bsvs(ksv)*eyr(i)
      ctrr = eyri(j)
      ctri = eyii(j)
      govpwr(j) = bsvs(ksv)/100.
      vfldtn(1, j) = sig1
      vfldtn(2, j) = sig2
      regout(j) = vscs(ksv)
      supout(j) = sig2
      isvsfz(ksv) = 0

C     IF A MANUAL SVS FREEZE IS REQUESTED SET JFTABN TO INDEX IN
C     SVS TABLES

      do igndrp = 1, ifcd
        if (mflt(igndrp) .eq. 9) then
          if (ipcdtn(igndrp) .eq. 1) then
            if (iftabn(igndrp) .eq. igentn(1, j)) then
              if (idgnc(igndrp) .eq. id) jftabn(igndrp) = ksv
            endif
          endif
        endif
      enddo
      return
      end
