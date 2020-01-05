C    @(#)chk_brndta.f	20.5 7/18/96
      integer function chk_brndta(ptr, field, count, out_buffer)
      integer ptr, field, count
      character out_buffer(10)*120

C     This subroutine checks change record CHGRCD(IC) more extensively
C     for data errors.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/prt.inc'

      common /range/ numlrg, numtrg, range(9, 11), txrange(4, 18)
      integer numlrg, numtrg, ptrx, status, 
     &        chk_brnfld, find_bus, sect
      real range, txrange, txran1(4, 10), txran2(4, 8), miles
      equivalence (txran1, txrange(1, 1)), (txran2, txrange(1, 11))
      character kyri*3, kyro*3, type*1
      logical finished

      data numlrg, numtrg / 11, 18/
      data range /
     &      0.0, 0.17, 1.42, 0.12, 1.13, 0.52, 0.94, 2.59, 4.05, 
     &     69.0, 0.16, 1.29, 0.12, 1.13, 0.60, 0.97, 2.49, 3.47, 
     &    100.0, 0.11, 0.71, 0.07, 0.60, 0.63, 0.92, 2.49, 3.35, 
     &    115.0, 0.09, 0.71, 0.06, 0.60, 0.63, 0.92, 2.49, 3.38, 
     &    138.0, 0.08, 0.68, 0.06, 0.60, 0.67, 0.94, 2.45, 3.20, 
     &    161.0, 0.08, 0.41, 0.06, 0.31, 0.67, 0.86, 2.49, 3.20, 
     &    230.0, 0.07, 0.17, 0.05, 0.14, 0.74, 0.88, 2.42, 2.85, 
     &    300.0, 0.07, 0.12, 0.05, 0.09, 0.76, 0.82, 2.60, 2.77, 
     &    345.0, 0.06, 0.15, 0.03, 0.12, 0.56, 0.86, 2.48, 3.73, 
     &    500.0, 0.03, 0.07, 0.01, 0.05, 0.49, 0.77, 2.75, 4.21, 
     &   1100.0, 0.03, 0.07, 0.01, 0.05, 0.49, 0.77, 2.75, 4.21
     &     /
      data txran1 /
     &    46.0, 115.0, 0.055, 0.075, 
     &    59.8, 115.0, 0.070, 0.090, 
     &    59.8, 230.0, 0.140, 0.150, 
     &    66.0, 115.0, 0.070, 0.090, 
     &    69.0, 115.0, 0.130, 0.140, 
     &    69.0, 230.0, 0.120, 0.152, 
     &   115.0, 161.0, 0.055, 0.075, 
     &   115.0, 230.0, 0.065, 0.078, 
     &   115.0, 500.0, 0.110, 0.130, 
     &   138.0, 161.0, 0.055, 0.075 /
      data txran2 /
     &   138.0, 230.0, 0.055, 0.075, 
     &   138.0, 345.0, 0.090, 0.110, 
     &   161.0, 230.0, 0.065, 0.075, 
     &   161.0, 345.0, 0.090, 0.110, 
     &   230.0, 300.0, 0.050, 0.060, 
     &   230.0, 345.0, 0.055, 0.076, 
     &   230.0, 500.0, 0.090, 0.110, 
     &   500.0, 1100.0, 0.090, 0.110 /

      if (ptr .eq. 0) go to 220
      chk_brndta = 0
      nbr = iabs (brnch_ptr(ptr))
      if ((field .eq. 0 .or. field .eq. 19) .and.
     &    (kbrnch(15,nbr) .gt. 2)) then
        if (brtype(ptr) .eq. 4) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          out_buffer(count) = ' LTC side > 2 '
        else
          chk_brndta = 1
          count = min0 (count + 1, 10)
          out_buffer(count) = ' Meter code > 2 '
        endif
      endif

      if (brtype(ptr) .eq. 3) then

C       "L" DATA

        if ((field .eq. 0 .or. field .eq. 34) .and.
     &      (brnch(4,nbr) .lt. 0.0 .or. brnch(4,nbr) .gt. 3000.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10010) brnch(4,nbr)
10010     format (' Rating (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 38) .and.
     &      (brnch(16,nbr) .lt. 0.0 .or. brnch(16,nbr) .gt. 9.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10012) brnch(16,nbr)
10012     format (' Number of lines (', f4.0, ') < 0 or > 9 ')
        endif
        if ((field .eq. 0 .or. field .eq. 39) .and.
     &      (brnch(5,nbr) .lt. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10020) brnch(5,nbr)
10020     format (' Resistance (', f7.5, ') < 0')
        endif
        if ((field .eq. 0 .or. field .eq. 45) .and.
     &      (brnch(6,nbr) .eq. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10030) brnch(6,nbr)
10030     format (' Reactance (', f7.5, ') = 0')
        endif
        if ((field .eq. 0 .or. field .eq. 51) .and.
     &      (brnch(7,nbr) .ne. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10032) brnch(7,nbr)
10032     format (' G/2 (', f7.5, ') >< 0')
        endif
        if ((field .eq. 0 .or. field .eq. 63) .and.
     &      (brnch(16,nbr) .lt. 0.0 .or. brnch(16,nbr) .gt. 1000.0))
     &    then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10034) brnch(16,nbr)
10034     format (' Mileage (', f7.1, ') < 0 or > 999 ')
        endif
      elseif (brtype(ptr) .eq. 7) then

C       "LD" DATA

        if ((field .eq. 0 .or. field .eq. 34) .and.
     &      (brnch(4,nbr) .lt. 0.0 .or. brnch(4,nbr) .gt. 3000.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10040) brnch(4,nbr)
10040     format (' Rating (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 38) .and.
     &      (brnch(5,nbr) .le. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10050) brnch(5,nbr)
10050     format (' D-c resistance (', f7.3, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 44) .and.
     &      (brnch(6,nbr) .le. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10060) brnch(6,nbr)
10060     format (' D-c Inductance (', f7.3, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 50) .and.
     &      (brnch(7,nbr) .le. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10062) brnch(7,nbr)
10062     format (' D-c Capacitance (', f7.3, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 56) .and.
     &      (kbrnch(14,nbr) .eq. 0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10070) kbrnch(14,nbr)
10070     format (' D-c Converter control (', i1, 
     &      ') must be "I" (2) or "R" (1)')
        endif
        if (brnch_ptr(ptr) .gt. 0) then
          pdc = brnch(8,nbr)
          vdc = brnch(9,nbr)
        else
          pdc = -brnch(8,nbr)
          vdc = brnch(9,nbr)
        endif
        if ((field .eq. 0 .or. field .eq. 57) .and.
     &      (pdc .lt. -3000.0 .or. pdc .gt. 3000.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10110) pdc
10110     format (' D-c Power (', f7.1, ') < -3000 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 62) .and.
     &      (vdc .le. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10100) vdc
10100     format (' D-c Volts (', f7.1, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 34. or. field .eq. 57 .or.
     &       field .eq. 62) .and.
     &      (brnch(4,nbr) .le. 0.000999*pdc/vdc)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          rate = 0.001*pdc/vdc
          write (out_buffer(count), 10120, err = 100) rate, 
     &      0.001*pdc/vdc
10120     format (' Rating (', f7.1, ') < Pdc / Vdc (', f7.1, ')')
          goto 110
  100     write (out_buffer(count), 10130) rate, pdc, vdc
10130     format (' Rating (', f7.1, ') < Pdc (', f7.1, ') / Vdc (', 
     &      f7.1, ')')
  110     continue
        endif
        if ((field .eq. 0 .or. field .eq. 67) .and.
     &      (brnch(9,nbr) .le. 5.0 .or. brnch(9,nbr) .ge. 105.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10080) brnch(9,nbr)
10080     format (' D-c Firing angle (', f7.3, ') < 5 or > 105 ')
        endif
        if ((field .eq. 0 .or. field .eq. 71) .and.
     &      (brnch(10,nbr) .le. 5.0 .or. brnch(10,nbr) .ge. 105.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10090) brnch(10,nbr)
10090     format (' D-c Extinction angle (', f7.3, ') < 5 or > 105 ')
        endif
      elseif (brtype(ptr) .eq. 2) then

C       "LM" DATA

        if ((field .eq. 0 .or. field .eq. 34) .and.
     &      (brnch(4,nbr) .lt. 0.0 .or. brnch(4,nbr) .gt. 3000.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10140) brnch(4,nbr)
10140     format (' Rating (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 38) .and.
     &      (brnch(5,nbr) .le. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10150) brnch(5,nbr)
10150     format (' D-c resistance (', f7.3, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 44) .and.
     &      (brnch(6,nbr) .le. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10160) brnch(6,nbr)
10160     format (' D-c Inductance (', f7.3, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 50) .and.
     &      (brnch(7,nbr) .le. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10170) brnch(7,nbr)
10170     format (' D-c Capacitance (', f7.3, ') < 0 ')
        endif

      elseif (brtype(ptr) .eq. 4) then

C       "R" DATA

        call getchr (1, type, kbrnch(3,nbr))
        if ((field .eq. 0 .or. field .eq. 2) .and.
     &      (index(' VQNOPRM', type) .eq. 0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10180) type
10180     format (' Subtype (', a, ') must be { ,V,Q,N,O,P,M}')
        endif
        tapmax = brnch(6,nbr)
        tapmin = brnch(7,nbr)
        if ((field .eq. 0 .or. field .eq. 46 .or. field .eq. 51) .and.
     &      (tapmax .le. tapmin)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10190) tapmax, tapmin
10190     format (' MAX_TAP (', f8.2, ') < MIN_TAP (', f8.2, ')')
        endif
        if (index('RP*RM', type) .eq. 0) then

C         Non-phase shifter:

          if ((field .eq. 0 .or. field .eq. 34 .or. field .eq. 42) .and.
     &        (kbrnch(4,nbr) .eq. 0)) then
            chk_brndta = 1
            count = min0 (count + 1, 10)
            out_buffer(count) = 
     &        ' Remote bus must be specified for this type'
          endif
c
c         Find variable tap side
c
          if (brnch_ptr(ptr) .gt. 0) then
            intovr = kbrnch(15,nbr)
          else
            intovr = mod(3-kbrnch(15,nbr),3)
          endif
          if (intovr .eq. 0) then
            t1 = dim(base(kx(ptr)), tapmax) - dim(tapmin, base(kx(ptr)))
            t2 = dim(base(ky(ptr)), tapmax) - dim(tapmin, base(ky(ptr)))
            if (abs(t1) .lt. abs(t2)) then

C             "K1" is variable tap side

              intovr = 1
            else

C             "K2" is variable tap side

              intovr = 2
            endif
          endif
          if (intovr .eq. 1) then
            if ((field .eq. 0 .or. field .eq. 46 .or. 
     &           field .eq. 51) .and.
     &          (tapmax .lt. base(kx(ptr)) .or. 
     &           tapmin .ge. base(kx(ptr)))) then
              chk_brndta = 1
              count = min0 (count + 1, 10)
              write (out_buffer(count), 10200) tapmax, tapmin,
     &           base(kx(ptr))
10200         format (' MAX_TAP (', f8.2, '), MIN_TAP (', f8.2, 
     &           ') does not enclose BASE1 kv (', f7.2, ')')
            endif
          elseif ((field .eq. 0 .or. field .eq. 46 .or. 
     &             field .eq. 51) .and.
     &            (tapmax .lt. base(ky(ptr)) .or. 
     &             tapmin .gt. base(ky(ptr)))) then
            chk_brndta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10210) tapmax, tapmin,
     &        base(ky(ptr))
10210       format (' MAX_TAP (', f8.2, '), MIN_TAP (', f8.2, 
     &        ') does not enclose BASE2 kv (', f7.2, ')')
          endif

C         Phase shifter:

        elseif ((field .eq. 0 .or. field .eq. 46 .or. 
     &           field .eq. 51) .and.
     &          (tapmax .gt. 60.0 .or. tapmin .lt. -60.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
            write (out_buffer(count), 10220) tapmax, tapmin
10220       format (' Alpha max (', f8.2, ') > 60 or Alpha min (', 
     &        f8.2, ') < -60 ')
        endif
        if (index('QN', type) .ne. 0) then
          if ((field .eq. 0 .or. field .eq. 58 .or. field .eq. 63) .and.
     &        (brnch(9,nbr) .lt. brnch(10,nbr))) then
            chk_brndta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10230) brnch(9,nbr), 
     &        brnch(10,nbr)
10230       format (' Max MW/MVAR (', f7.1, ') < min MW/MVAR (', 
     &        f7.1, ')')
          endif
        endif
      elseif (brtype(ptr) .eq. 5 .or. brtype(ptr) .eq. 6) then

C       "T" DATA

        if ((field .eq. 0 .or. field .eq. 34) .and.
     &      (brnch(4,nbr) .lt. 0.0 .or. brnch(4,nbr) .gt. 3000.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10260) brnch(4,nbr)
10260     format (' Rating (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 39) .and.
     &      (brnch(5,nbr) .lt. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10270) brnch(5,nbr)
10270     format (' Resistance (', f7.5, ') < 0')
        endif
        if ((field .eq. 0 .or. field .eq. 45) .and.
     &      (brnch(6,nbr) .eq. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10280) brnch(6,nbr)
10280     format (' Reactance (', f7.5, ') = 0')
        endif
        if (brnch_ptr(ptr) .gt. 0) then
           tap1 = brnch(9,nbr)
           tap2 = brnch(10,nbr)
        else
           tap1 = brnch(10,nbr)
           tap2 = brnch(9,nbr)
        endif
        if (brtype(ptr) .eq. 5) then
          if ((field .eq. 0 .or. field .eq. 63) .and.
     &        (dim(tap1, 1.33*base(kx(ptr))) 
     &        +dim(0.67*base(kx(ptr)), tap1) .ne. 0)) then
            chk_brndta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10240) tap1, base(kx(ptr))
10240       format (' TAP1 (', f8.2, ') > 1.33*BASE1 (', f8.2, 
     &        ') or < 0.67*BASE1 ')
          endif
          if ((field .eq. 0 .or. field .eq. 68) .and.
     &        (dim(tap2, 1.33*base(ky(ptr)))
     &        +dim(0.67*base(ky(ptr)), tap2) .ne. 0)) then
            chk_brndta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10250) tap2, base(ky(ptr))
10250       format (' TAP2 (', f8.2, ') > 1.33*BASE2 (', f8.2, 
     &        ') or < 0.67*BASE2 ')
          endif
        else
          if ((field .eq. 0 .or. field .eq. 63) .and.
     &        (dim(tap1, 60.0) + dim( -60.0, tap1) .ne. 0.0)) then
            chk_brndta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10252) tap1
10252       format (' TAP1 (', f8.2, ') > 60.0 or < -60.0 ')
          endif
        endif

C       Search for "R" record; check validity of starting taps

        ptrx = numbrn (kx(ptr), ky(ptr), ' ', 0)
        if (ptrx .gt. 0) then
          if (brtype(ptrx) .eq. 4) then
            nbrx = brnch_ptr(ptrx)
            call getchr (1, type, kbrnch(3,nbr))
            if (nbrx .gt. 0) then
               intovr = kbrnch(15,nbrx)
               tapmax = brnch(6,nbrx)
               tapmin = brnch(7,nbrx)
            else
               intovr = mod (3-kbrnch(15,-nbrx), 3)
               if (brtype(ptr) .eq. 5) then
                 tapmax = brnch(6,-nbrx)
                 tapmin = brnch(7,-nbrx)
               else
                 tapmax = -brnch(7,-nbrx)
                 tapmin = -brnch(6,-nbrx)
               endif
            endif
            if (brtype(ptr) .eq. 5 .and. intovr .eq. 0) then
              t1 = dim(base(kx(ptr)), tapmax) 
     &           - dim(tapmin, base(kx(ptr)))
              t2 = dim(base(ky(ptr)), tapmax) 
     &           - dim(tapmin, base(ky(ptr)))
              if (abs(t1) .lt. abs(t2)) then

C               "K1" is variable tap side

                intovr = 1
              elseif (abs(t1) .gt. abs(t2)) then

C               "K2" is variable tap side

                intovr = 2
              endif
            endif
            if (intovr .eq. 1) then
              if ((field .eq. 0 .or. field .eq. 63) .and.
     &            (tapmax .lt. tap1 .or. tapmin .gt. tap1)) then
                chk_brndta = 1
                count = min0 (count + 1, 10)
                write (out_buffer(count), 10290) tap1, tapmin, tapmax
10290           format ('  Tap 1 (', f8.2, ') must between MIN_TAP (', 
     &            f8.2, ') and MAX_TAP (', f8.2, ')')
              endif
            elseif ((field .eq. 0 .or. field .eq. 68) .and.
     &              (tapmax .lt. tap2 .or. tapmin .gt. tap2)) then
              chk_brndta = 1
              count = min0 (count + 1, 10)
              write (out_buffer(count), 10300) tap2, tapmin, tapmax
10300         format ('  Tap 2 (', f8.2, ') must between MIN_TAP (', 
     &          f8.2, ') and MAX_TAP (', f8.2, ')')
            endif
          endif
        endif
  120   if (brtype(ptr) .eq. 5) then

          basmin = amin1(base(kx(ptr)), base(ky(ptr)))
          basmax = amax1(base(kx(ptr)), base(ky(ptr)))
          if (basmin .gt. 25.0 .and. brnch(6,nbr) .gt. 0.0 .and. 
     &        brnch(4,nbr) .gt. 0.0) then
            finished = .false.
            k = 1
            do while (k .le. numtrg .and. .not. finished)
              if (txrange(1, k) .gt. basmin) then
                k = numtrg + 1
              else
                if (txrange(1, k) .le. basmin) then
                  if (txrange(2, k) .gt. basmax) then
                    k = numtrg + 1
                  else if (txrange(2, k) .le. basmax) then
                    finished = .true.
                  else  
                    k = k + 1
                  endif
                else
                  k = k + 1
                endif
              endif
            enddo
            if (.not. finished) then
              chk_brndta = 1
              count = min0 (count + 1, 10)
              out_buffer(count) = ' No %Z rating for this voltage '
            else
              z = sqrt (brnch(5,nbr)**2 + brnch(6,nbr)**2)
     &          * brnch(4,nbr) / 100.0
              pct = dim(z, txrange(4, k)) - dim(txrange(3, k), z)
              if (pct .gt. 0.0) then
                pct = 100.0*pct/abs(txrange(4, k))
              elseif (pct .lt. 0.0) then
                pct = 100.0*pct/abs(txrange(3, k))
              else
                goto 150
              endif
              z = 100.0*z
              if (field .eq. 0 .or. field .eq. 38 .or. 
     &            field .eq. 39 .or. field .eq. 45) then
                chk_brndta = 1
                count = min0 (count + 1, 10)
                write (out_buffer(count), 10310) z, pct
10310           format (' %Z     (', f7.1, ') violates limits by ', 
     &            f7.1, ' %')
             endif
            endif
          endif
        endif
      elseif (brtype(ptr) .eq. 8) then

C       "E" DATA

        if ((field .eq. 0 .or. field .eq. 34) .and.
     &      (brnch(4,nbr) .lt. 0.0 .or. brnch(4,nbr) .gt. 3000.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10320) brnch(4,nbr)
10320     format (' Rating (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 39) .and.
     &      (brnch(5,nbr) .lt. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10330) brnch(5,nbr)
10330     format (' Resistance (', f7.5, ') < 0')
        endif
        if ((field .eq. 0 .or. field .eq. 44) .and.
     &      (brnch(6,nbr) .eq. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10340) brnch(6,nbr)
10340     format (' Reactance (', f7.5, ') = 0')
        endif
        if (brnch_ptr(ptr) .gt. 0) then
           g1 = brnch(7,nbr)
           b1 = brnch(8,nbr)
           g2 = brnch(9,nbr)
           b2 = brnch(10,nbr)
        else
           g2 = brnch(7,nbr)
           b2 = brnch(8,nbr)
           g1 = brnch(9,nbr)
           b1 = brnch(10,nbr)
        endif
        if ((field .eq. 0 .or. field .eq. 51) .and.
     &      (g1 .ne. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10350) g1
10350     format (' G1 (', f7.5, ') >< 0')
        endif
        if ((field .eq. 0 .or. field .eq. 63) .and.
     &      (g2 .ne. 0.0)) then
          chk_brndta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10360) g2
10360     format (' G2 (', f7.5, ') >< 0')
        endif
      else
        chk_brndta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10370) brtype(ptr)
10370   format (' Illegal branch type (', i2, ')')
        goto 220
      endif

  150 if (brtype(ptr) .ne. 1 .and. brtype(ptr) .ne. 2 .and. 
     &    brtype(ptr) .ne. 4 .and. brtype(ptr) .ne. 7) then
        if (brnch(5,nbr) .ge. 0.0) then
          finished = .false.
          k = 1
          do while (k .le. numlrg .and. .not. finished)
            if (base(kx(ptr)) .eq. range(1, k)) then
              finished = .true.
            else
              k = k + 1
            endif
          enddo
          if (.not. finished) k = 10
          ratio = 0.0
          if (brnch(6,nbr) .ne. 0.0) ratio = brnch(5,nbr)/brnch(6,nbr)
          if (ratio .gt. 0.0) then
            pct = dim(ratio, range(3, k)) - dim(range(2, k), ratio)
            if (pct .gt. 0.0) then
              pct = 100.0*pct/abs(range(3, k))
            elseif (pct .lt. 0.0) then
              pct = 100.0*pct/abs(range(2, k))
            else
              goto 180
            endif
            if (field .eq. 0 .or. field .eq. 39 .or. field .eq. 45) then
              chk_brndta = 1
              count = min0 (count + 1, 10)
              write (out_buffer(count), 10380) ratio, pct
10380         format (' R/X    (', f7.3, ') violates limits by ', f7.1, 
     &          '%')
            endif
          endif
  180     if (brtype(ptr) .eq. 3) then
            miles = brnch(9,nbr)
            parls = brnch(16,nbr)
            if (miles .gt. 0.0) then
              parls = amax1(1.0, parls)
              ratio = brnch(5,nbr)/miles*base(kx(ptr))**2/100.0/parls
              if (ratio .gt. 0.0) then
                pct = dim(ratio, range(5, k)) - dim(range(4, k), ratio)
                if (pct .gt. 0.0) then
                  pct = 100.0*(ratio-abs(range(5, k)))
                  pct = pct/abs(range(5, k))
                elseif (pct .lt. 0.0) then
                  pct = 100.0*(ratio-abs(range(4, k)))
                  pct = pct/abs(range(4, k))
                else
                  goto 190
                endif
                if (field .eq. 0 .or. field .eq. 38 .or. 
     &              field .eq. 39 .or. field .eq. 63) then
                  chk_brndta = 1
                  count = count + 1
                  write (out_buffer(count), 10390) ratio, pct
10390             format (' R/MILE (', f7.3, ') violates limits by ', 
     &              f7.1, ' %')
                endif
              endif
  190         ratio = brnch(6,nbr)/miles*base(kx(ptr))**2/100.0/parls
              if (ratio .gt. 0.0) then
                pct = dim(ratio, range(7, k)) - dim(range(6, k), ratio)
                if (pct .gt. 0.0) then
                  pct = 100.0*(ratio-abs(range(7, k)))
                  pct = pct/abs(range(7, k))
                elseif (pct .lt. 0.0) then
                  pct = 100.0*(ratio-abs(range(6, k)))
                  pct = pct/abs(range(6, k))
                else
                  goto 200
                endif
                if (field .eq. 0 .or. field .eq. 38 .or. 
     &              field .eq. 45 .or. field .eq. 63) then
                  chk_brndta = 1
                  count = min0 (count + 1, 10)
                  write (out_buffer(count), 10400) ratio, pct
10400             format (' X/MILE (', f7.3, ') violates limits by ', 
     &              f7.1, ' %')
                endif
              endif
  200         ratio = brnch(8,nbr)/miles/base(kx(ptr))**2*10.0**8*parls
              if (ratio .gt. 0.0) then
                pct = dim(ratio, range(9, k)) - dim(range(8, k), ratio)
                if (pct .gt. 0.0) then
                  pct = 100.0*(ratio-abs(range(9, k)))
                  pct = pct/abs(range(9, k))
                elseif (pct .lt. 0.0) then
                  pct = 100.0*(ratio-abs(range(8, k)))
                  pct = pct/abs(range(8, k))
                else
                  goto 220
                endif
                if (field .eq. 0 .or. field .eq. 38 .or. 
     &              field .eq. 57 .or. field .eq. 63) then
                  chk_brndta = 1
                  count = min0 (count + 1, 10)
                  write (out_buffer(count), 10410) ratio, pct
10410             format (' B/MILE (', f7.3, ') violates limits by ', 
     &              f7.1, ' %')
                endif
              endif
            endif
          endif
        endif
      endif

  220 return
      end
