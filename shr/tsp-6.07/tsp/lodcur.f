C    %W% %G%
      subroutine lodcur

c     THIS SUBROUTINE CALCULATES THE CURRENT INJECTION
c     PRODUCED BY THE BUS LOADS.  IT IS CALLED BY DERIV.

      include 'tspinc/params.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/busdta.inc'
      include 'tspinc/ldidxn.inc'
      include 'tspinc/ldndxp.inc'
      include 'tspinc/ldshdn.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/ecstbj.inc'
      include 'tspinc/bcur.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/iter.inc'
      include 'tspinc/deltfq.inc'
      include 'tspinc/param.inc'
      include 'tspinc/delcr.inc'

      common /satsw/satsw
      equivalence (ldp, asat), (ldq, bsat)

      dimension itemp(4)
      equivalence (itemp(1), ityp1), (itemp(2), ityp2), 
     &            (itemp(3), ityp3), (itemp(4), ityp4)
      logical lowvoltage

c     LV COUNTS THE BUSES PROCESSED MAX = NMX
c     LIND COUNTS THE BUSES WITH LOAD DATA MAX = LREP
      lind = 0
      lv   = 0
      dsum = 0.0

      do while (.true.)

        lv = lv + 1

c       exit loop if all buses have been processed
        if (lv .gt. nmx) goto 150

        do while (.true.)
          lind = lind + 1
          if (lind .gt. lrep) goto 140
          ifreq = ldndxp(3, lind)
          if (ifreq .ne. 6) goto 100
        enddo

  100   ibusno = ldndxp(4, lind)

        do while (ibusno .ne. lv)
          bcurr(lv) = 0.0
          bcuri(lv) = 0.0
          lv = lv + 1
          if (lv .gt. nmx) goto 150
        enddo

        if (abs(eyr(lv)) .ge. 0.0001 .or. abs(eyi(lv)) .ge. 0.0001) then
          iecsl = ldidxn(6, lv)
          nitem = ldidxn(5, lv)
          itemp(1) = ldidxn(4, lv)
          itemp(2) = ldidxn(3, lv)
          itemp(3) = ldidxn(2, lv)
          itemp(4) = ldidxn(1, lv)
          ifreq = ldndxp(3, lind)
          emag = emagrn(1, lv)
          pasta = emagrn(2, lv)
          eyi1 = eyi(lv)
          eyr1 = eyr(lv)
          qfqfac = 0.0
          rfqfac = 0.0

          if (ifreq .ne. 1) then

            ldq = ldndxp(2, lind)
            ldp = ldndxp(1, lind)

c           IFREQ .GT. 3 MEANS SATURATION REPRESENTATION
            if (ifreq .le. 3) then

              if (idsw .ne. 7) then

c               not at discontinutity
                presa = atan2(eyi1, eyr1)
                delan = presa - pasta
                if (delan .lt. -3.14159) delan = delan + 6.28318
                if (delan .gt. 3.14159) delan = delan - 6.28318

c               IF TBUSF .NE. 0 USE FILTERED BUS FREQUENCY
                if (tbusf .eq. 0.0) then


c                 CHANGED DATA TO UNSCALED FIXED POINT NO. IN CNTRL
c                 DELTFQ IS THE PAST VALUE OF BUS FREQUENCY DIFFERENCE
                  delfeq = delan / edt
                else
                  dfreq = delan / edt
                  delfeq = (dfreq + deltfq(lv) + delfqo(lv)
     &                    * (abus - 2.0)) * abusr
                endif
              elseif (tbusf .eq. 0.0) then
c               No bus filter time constant
                delfeq = deltfq(lv)
              else
c               USE STORED DF FOR FREQ DEP LOADS AT DISCONTINUITY
                delfeq = delfqo(lv)
              endif

              xldp = float(ldp)*0.01
              xldq = float(ldq)*0.01

              rfqfac = delfeq*xldp
              qfqfac = delfeq*xldq

            endif
          endif

          nitemt = nitem
          iecsin = iecsl - 1
          curr = 0.0
          curi = 0.0
          p1 = 0.
          q1 = 0.
          p2 = 0.
          q2 = 0.
          p3 = 0.
          q3 = 0.
          p4 = 0.
          q4 = 0.

          if (ifreq .ne. 6) then
            emagsq = emag * emag
            evltsq = 1.0 / (eyi1 * eyi1 + eyr1 * eyr1)
            evlt = sqrt(evltsq)

c           lowvoltage = .false. MEANS VOLTAGE IS GREATER THAN 0.5
c           lowvoltage = .true.  MEANS VOLTAGE IS LESS THAN 0.5
            lowvoltage = (evlt .gt. 2.0) 

c           INEWTS = 1 ADMITTANCE SOLUTION 
c                  = 2 NEWTON'S SOLUTION
c           MDEYOJ = 1 ADMITTANCE SOLUTION 
c                  = 2 NEWTON'S SOLUTION
            if (inewts .eq. 2 .and. mdeyoj .eq. 2) then

c             CALCULATE CURRENT FOR NEWTONS METHOD
              gi1 = 0.
              gi2 = 0.
              bi1 = 0.
              bi2 = 0.

              if (ifreq .eq. 3) then
c               TYPE B LOAD REPRESENTATION

                if (ityp1 .eq. 1) then
c                 CONSTANT IMPEDANCE REPRESENTATION
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p1 = busldn(1, iecsin)
                  q1 = busldn(2, iecsin)
                  p1f = (1.0 + rfqfac) * p1
                  q1f = (1.0 + qfqfac) * q1
                  gi1 =  - p1f * emagsq
                  gi2 = gi1
                  bi1 =  - q1f * emagsq
                  bi2 =  - bi1
                  if (nitemt .eq. 0) goto 110
                endif

                if (ityp3 .eq. 3) then
c                 CONSTANT POWER REPRESENTATION
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p3 = busldn(1, iecsin)
                  q3 = busldn(2, iecsin)
                  p3f = (1.0 + rfqfac) * p3
                  q3f = (1.0 + qfqfac) * q3

c                 CONSTANT IMPEDANCE AT 0.5 PU VOLTAGE
                  if (.not. lowvoltage) then
                    curr = curr
     &                   + 2.0 * (p3f * eyr1 + q3f * eyi1) * evltsq
                    curi = curi 
     &                   + 2.0 * (p3f * eyi1 - q3f * eyr1) * evltsq
                    edif = eyr1 * eyr1 - eyi1 * eyi1
                    esq = evltsq * evltsq
                    b = (-q3f * edif + 2.0 * p3f * eyr1 * eyi1) * esq
                    g = (p3f * edif + 2.0 * q3f * eyr1 * eyi1) * esq
                    bi1 = bi1 + b
                    gi1 = gi1 + g
                    bi2 = bi2 + b
                    gi2 = gi2 - g
                  else
                    bi1 = bi1 - 4.0 * q3f
                    bi2 = bi2 + 4.0 * q3f
                    gi2 = gi2 - 4.0 * p3f
                    gi1 = gi1 - 4.0 * p3f
                  endif
                  if (nitemt .eq. 0) goto 110
                endif

c               CONSTANT CURRENT REPRESENTATION
                iecsin = iecsin + 1
                p4 = busldn(1, iecsin)
                q4 = busldn(2, iecsin)
                p4f = (1.0 + rfqfac) * p4
                q4f = (1.0 + qfqfac) * q4

                if (.not. lowvoltage) then
                  watf = p4f * eyr1 + q4f * eyi1
                  varf = p4f * eyi1 - q4f * eyr1
                  curr = curr + watf * evlt * emag
                  curi = curi + varf * evlt * emag
                  eden = evlt * emag * evltsq
                  bi1 = bi1 + eyr1 * varf * eden
                  gi1 = gi1 - eyi1 * varf * eden
                  gi2 = gi2 - eyr1 * watf * eden
                  bi2 = bi2 + eyi1 * watf * eden
                else
                  bi1 = bi1 - 4.0 * q4f
                  bi2 = bi2 + 4.0 * q4f
                  gi2 = gi2 - 4.0 * p4f
                  gi1 = gi1 - 4.0 * p4f
                endif
              else

c               TYPE A REPRESENTATION
                if (ityp1 .eq. 1) then
c                 CONSTANT Z REPRESENTATION

                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p1 = busldn(1, iecsin)
                  q1 = busldn(2, iecsin)
                  bi2 = bi2 + q1 * emagsq
                  gi2 = gi2 - p1 * emagsq
                  bi1 = bi1 - q1 * emagsq
                  gi1 = gi1 - p1 * emagsq
                endif

                if (ifreq .eq. 1 .and. ityp2 .eq. 2) then
c                 CONSTANT POWER WITH FREQ DEPENDENCE REP
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p2 = busldn(1, iecsin)
                  q2 = busldn(2, iecsin)
                  p2f = p2 * (rfqfac + 1.0)
                  q2f = q2 * (qfqfac + 1.0)
                  if (.not. lowvoltage) then
                    curr = curr 
     &                   + 2.0 * (p2f * eyr1 + q2f * eyi1) * evltsq
                    curi = curi 
     &                   + 2.0 * (p2f * eyi1 - q2f * eyr1) * evltsq
                    edif = eyr1 * eyr1 - eyi1 * eyi1
                    esq = evltsq * evltsq
                    b = (-q2f * edif + 2.0 * p2f * eyr1 * eyi1) * esq
                    g = (p2f * edif + 2.0 * q2f * eyr1 * eyi1) * esq
                    bi1 = bi1 + b
                    bi2 = bi2 + b
                    gi1 = gi1 + g
                    gi2 = gi2 - g
                  else

c                   CONSTANT IMPED. AT 0.5 PU VOLTAGE
                    bi1 = bi1 - 4.0 * q2f
                    bi2 = bi2 + 4.0 * q2f
                    gi1 = gi1 - 4.0 * p2f
                    gi2 = gi2 - 4.0 * p2f
                  endif
                  if (nitemt .eq. 0) goto 110
                endif

                if (ityp3 .eq. 3) then
c                 CONSTANT POWER REP.
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p3 = busldn(1, iecsin)
                  q3 = busldn(2, iecsin)
                  if (.not. lowvoltage) then
                    curr = curr + 2.0 * (p3 * eyr1 + q3 * eyi1) * evltsq
                    curi = curi + 2.0 * (p3 * eyi1 - q3 * eyr1) * evltsq
                    edif = eyr1 * eyr1 - eyi1 * eyi1
                    esq = evltsq * evltsq
                    b = (-q3 * edif + 2.0 * p3 * eyr1 * eyi1) * esq
                    g = (p3 * edif + 2.0 * q3 * eyr1 * eyi1) * esq
                    bi1 = bi1 + b
                    gi1 = gi1 + g
                    bi2 = bi2 + b
                    gi2 = gi2 - g
                  else

c                   LOAD IS CONST. IMPED. AT 0.5 VOLTAGE
                    bi1 = bi1 - 4.0 * q3
                    bi2 = bi2 + 4.0 * q3
                    gi1 = gi1 - 4.0 * p3
                    gi2 = gi2 - 4.0 * p3
                  endif
                endif

                if (nitemt .ne. 0) then
c                 CONSTANT CURR. REP.
                  iecsin = iecsin + 1
                  p4 = busldn(1, iecsin)
                  q4 = busldn(2, iecsin)

                  if (.not. lowvoltage) then
c                   LOADS ARE CONSTANT IMPEDANCE AT O.5 PU VOLTAGE
                    watf = p4 * eyr1 + q4 * eyi1
                    varf = p4 * eyi1 - q4 * eyr1
                    curr = curr + watf * evlt * emag
                    curi = curi + varf * evlt * emag
                    eden = evlt * emag * evltsq
                    bi1 = bi1 + eyr1 * varf * eden
                    gi1 = gi1 - eyi1 * varf * eden
                    gi2 = gi2 - eyr1 * watf * eden
                    bi2 = bi2 + eyi1 * watf * eden
                  else
                    bi1 = bi1 - 4.0 * q4
                    gi2 = gi2 - 4.0 * p4
                    gi1 = gi1 - 4.0 * p4
                    bi2 = bi2 + 4.0 * q4
                  endif
                endif
              endif

  110         gnewt(2*lv-1) = gnewt(2*lv-1) + gi1
              gnewt(2*lv) = gnewt(2*lv) + gi2
              bnewt(2*lv-1) = bnewt(2*lv-1) + bi1
              bnewt(2*lv) = bnewt(2*lv) + bi2
c             END OF NEWTONS LOGIC

            else

c             IFREQ .GT. 3 MEANS SATURATION REPRESENTATION
              if (ifreq .gt. 3) then
                cio = busldn(1, 2)
                vcnv = busldn(2, 2)
                csatmx = busldn(1, 3)
                vsfull = busldn(2, 3)
                p4 = busldn(1, 4)
                q4 = busldn(2, 4)

              elseif (ifreq .eq. 3) then
c               TYPE B LOAD REPRESENTATION

                if (ityp1 .eq. 1) then
c                 CONSTANT IMPEDANCE REPRESENTATION
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p1 = busldn(1, iecsin)
                  q1 = busldn(2, iecsin)
                  if (p1 .ne. 0.0 .or. q1 .ne. 0.0) then
                    curr = curr
     &                   + (p1 * eyr1 + q1 * eyi1) * rfqfac * emagsq
                    curi = curi 
     &                   + (p1 * eyi1 - q1 * eyr1) * qfqfac * emagsq
                  endif
                  if (nitemt .eq. 0) goto 120
                endif

                if (ityp3 .eq. 3) then
c                 CONSTANT POWER REPRESENTATION
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p3 = busldn(1, iecsin)
                  q3 = busldn(2, iecsin)
                  if (p3 .ne. 0.0 .or. q3 .ne. 0.0) then
                    if (.not. lowvoltage) then
                      fpterm = (1.0 + rfqfac) * evltsq - emagsq
                      fqterm = (1.0 + qfqfac) * evltsq - emagsq
                    else

c                     LOADS ARE CONSTANT IMPEDANCE IF VOLTAGE IS UNDER
c                     0.5
                      fpterm = (1.0 + rfqfac) * 4.0 - emagsq
                      fqterm = (1.0 + qfqfac) * 4.0 - emagsq
                    endif
                    curr = curr + p3 * eyr1 * fpterm 
     &                          + q3 * eyi1 * fqterm
                    curi = curi + p3 * eyi1 * fpterm 
     &                          - q3 * eyr1 * fqterm
                  endif
                  if (nitemt .eq. 0) goto 120
                endif

c               CONSTANT CURRENT REPRESENTATION
                iecsin = iecsin + 1
                p4 = busldn(1, iecsin)
                q4 = busldn(2, iecsin)
                if (p4 .ne. 0.0 .or. q4 .ne. 0.0) then

                  if (.not. lowvoltage) then
                    fpterm = (1.0 + rfqfac) * evlt * emag - emagsq
                    fqterm = (1.0 + qfqfac) * evlt * emag - emagsq
                  else
c                   LOADS ARE CONSTANT IMPEDANCE IF VOLTAGE IS UNDER 0.5
                    fpterm = (1.0 + rfqfac) * 2.0 * emag - emagsq
                    fqterm = (1.0 + qfqfac) * 2.0 * emag - emagsq
                  endif
                  curr = curr + p4 * eyr1 * fpterm + q4 * eyi1 * fqterm
                  curi = curi + p4 * eyi1 * fpterm - q4 * eyr1 * fqterm
                endif
                goto 120
              else

c               TYPE A LOAD REPRESENTATION
                if (ityp1 .eq. 1) then

c                 CONSTANT Z REPRESENTATION
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                endif
                if (ifreq .eq. 1 .and. ityp2 .eq. 2) then
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p2 = busldn(1, iecsin)
                  q2 = busldn(2, iecsin)

                  if (p2 .ne. 0.0 .or. q2 .ne. 0.0) then
c                   CONSTANT POWER WITH FREQ. DEPENDENCE REPRESENTATION
                    p2f = rfqfac + 1.0
                    q2f = qfqfac + 1.0
                    pe2 = p2 * eyr1
                    qf2 = q2 * eyi1
                    pf2 = p2 * eyi1
                    qe2 = q2 * eyr1

c                   LOADS ARE CONSTANT IMPEDANCE IF VOLTAGE IS UNDER 0.5
                    if (.not. lowvoltage) then
                      fpterm = p2f * evltsq - emagsq
                      fqterm = q2f * evltsq - emagsq
                    else
                      fpterm = p2f * 4.0 - emagsq
                      fqterm = q2f * 4.0 - emagsq
                    endif
                    curr = curr + pe2 * fpterm + qf2 * fqterm
                    curi = curi + pf2 * fpterm - qe2 * fqterm
                  endif
                  if (nitemt .eq. 0) goto 120
                endif

                if (ityp3 .eq. 3) then
c                 CONSTANT POWER REPRESENTATION
                  nitemt = nitemt - 1
                  iecsin = iecsin + 1
                  p3 = busldn(1, iecsin)
                  q3 = busldn(2, iecsin)
                  if (p3 .ne. 0.0 .or. q3 .ne. 0.0) then
                    wattf = p3 * eyr1 + q3 * eyi1
                    varf = p3 * eyi1 - q3 * eyr1

c                   LOADS ARE CONSTANT IMPEDANCE IF VOLTAGE IS UNDER 0.5
                    if (.not. lowvoltage) then
                      difvsq = evltsq - emagsq
                    else
                      difvsq = 4.0 - emagsq
                    endif
                    curr = curr + wattf * difvsq
                    curi = curi + varf * difvsq
                  endif
                endif
                if (nitemt .eq. 0) goto 120

c               CONSTANT CURRENT REPRESENTATION
                iecsin = iecsin + 1
                p4 = busldn(1, iecsin)
                q4 = busldn(2, iecsin)
                if (p4 .eq. 0.0 .and. q4 .eq. 0.0) goto 120
              endif

              wattf = p4 * eyr1 + q4 * eyi1
              varf = p4 * eyi1 - q4 * eyr1

c             LOADS ARE CONSTANT IMPEDANCE IF VOLTAGE IS UNDER 0.5
              if (.not. lowvoltage) then
                difvsq = evlt * emag - emagsq
              else
                difvsq = 2.0 * emag - emagsq
              endif
              curr = curr + wattf * difvsq
              curi = curi + varf * difvsq

c             CHECK FOR AND PROCESS SATURATION EFFECTS
  120         do while (ifreq .ge. 4)

c               CONVERT V TO NEW PU & TEST FOR SATURATION
                vnew = vcnv/evlt

c               IGNORE SATURATION FOR V < 1.1 PER UNIT OR WHEN T = 0 
c               MINUS
                if (vnew .le. 1.1) goto 130

c               CONT
                if (vnew .lt. vsfull) then
                  cursat = cio * vnew * satsw/(asat + bsat * vnew)
                else

c                 ISAT FOR ABNORMALLY HI VOLTS
                  cursat = csatmx
                endif

c               PREPARE CURRENT VECTOR DUE TO SATURATION
                scurr =  - cursat * eyi1 * evlt
                scuri = cursat * eyr1 * evlt
                curr = curr + scurr
                curi = curi + scuri
              enddo
            endif

  130       continue

c           calculate change from previous iteration
            sumcr     = abs(curr) + abs(curi)
            cdel      = abs(delcr(lv) - sumcr)
            delcr(lv) = sumcr

c           ?
            dsum = dsum + cdel

c           update max change if necessary
            if (cmax .le. cdel) then
              imax = lv
              cmax = cdel
            endif

c           save current injection for this bus
            bcurr(lv) = curr
            bcuri(lv) = curi

          endif
        endif
      enddo
 
  140 continue

c     zero all remaining load current injection values
      do i = lv, nmx
        bcurr(i) = 0.0
        bcuri(i) = 0.0
      enddo
 
  150 continue

      return
      end
