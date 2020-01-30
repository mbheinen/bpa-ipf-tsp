C    @(#)chk_busdta.f	20.4 11/12/98
      integer function chk_busdta(nb, field, count, out_buffer)
      integer nb, field, count
      character out_buffer(10)*120

C     This subroutine checks BUSDTA(*,NB) extensively for data errors.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/bstype.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/xdata.inc'

      integer error, status, chk_busfld, find_bus
      character type*1
      logical finished

C     Set up pointers to X data (busxdtptr)

      if (.not. xdt_flag) then
        do nb = 1, ntot
          busxdtptr(nb) = 0
        enddo
        do i = 1, kxtot
          kxd = xdata(1, i)
          if (kxd .gt. 0) busxdtptr(kxd) = i
        enddo
        xdt_flag = .true.
      endif

      chk_busdta = 0
      if (kbsdta(1,nb) .ne. 5 .and. kbsdta(1,nb) .ne. 12) then

        if ((field .eq. 0 .or. field .eq. 21) .and.
     &      (busdta(3,nb) .lt. 0.0 .or. busdta(3,nb) .gt. 3000.0)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10010) busdta(3,nb)
10010     format (' Pload (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 26) .and.
     &      (busdta(4,nb) .lt. 0.0 .or. busdta(4,nb) .gt. 3000.0)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10020) busdta(4,nb)
10020     format (' Qload (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 31) .and.
     &      (busdta(5,nb) .ne. 0.0)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10030) busdta(5,nb)
10030     format (' Gshunt (', f7.1, ') >< 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 35) .and.
     &      (busdta(6,nb) .lt. -3000.0 .or. busdta(6,nb) .gt. 3000.0))
     &    then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10040) busdta(6,nb)
10040     format (' Bshunt (', f7.1, ') < -3000 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 31) .and.
     &      (busxdtptr(nb) .ne. 0)) then
          if (kbsdta(1,nb) .ne. BSTYP_BX) then
            kxd = busxdtptr(nb)
            total_cap = dim(busdta(6,nb),0.0)
            total_rek = dim(0.0, busdta(6,nb))
            if (abs(total_rek-xdata(3,kxd)) + 
     &          abs(total_cap-xdata(4,kxd)) .gt. 1.0) then
              chk_busdta = 1
              count = min0 (count + 1, 10)
              write (out_buffer(count), 10042) total_cap, total_rek, 
     &          xdata(4,kxd), xdata(3,kxd)
10042         format (' BShunt (', f7.1, ',', f7.1, 
     &          ') mismatches XShunt (', f7.1, ',', f7.1, ')')
            endif
          endif
        endif
        if ((field .eq. 0 .or. field .eq. 39 .or. field .eq. 43) .and.
     &      (busdta(7,nb) .lt. 0.0 .or. busdta(8,nb) .lt. 0.0 .or.
     &      (busdta(7,nb) .gt. 0.0 .and. 
     &       busdta(7,nb) .lt. busdta(8,nb)))) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10050) busdta(7,nb), busdta(8,nb)
10050     format (' Pmax (', f7.1, ') < Pgen (', f7.1, ')')
        endif
        if ((field .eq. 0 .or. field .eq. 43) .and.
     &      (busdta(8,nb) .lt. 0.0 .or. busdta(8,nb) .gt. 3000.0)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10052) busdta(8,nb)
10052     format (' Pgen (', f7.1, ') < 0 or > 3000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 43 .or. field .eq. 48 .or.
     &       field .eq. 53) .and.
     &      (busdta(8,nb) .gt. 0.0 .and. 
     &      (busdta(9,nb) .gt. busdta(8,nb) .or. 
     &       busdta(10,nb) .lt. -busdta(8,nb)))) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10060) busdta(8,nb), 
     &      busdta(9,nb), busdta(10,nb)
10060     format (' Pgen (', f7.1, ') < Qmax (', f7.1, 
     &      ') or -Pgen > Qmin (', f7.1, ')')
        endif
        if ((field .eq. 0 .or. field .eq. 48 .or. field .eq. 53) .and.
     &      (busdta(9,nb) .lt. busdta(10,nb))) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10070) busdta(9,nb), busdta(10,nb)
10070     format (' Qmax (', f7.1, ') < Qmin (', f7.1, ')')
        endif
        call glbvlt (nb, vmin2, vmax2)
        if ((field .eq. 0 .or. field .eq. 58) .and.
     &      (busdta(11,nb) .gt. 0.0 .and. 
     &      (busdta(11,nb) .gt. vmax2 .or. busdta(11,nb) .lt. vmin2))) 
     &    then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10080) busdta(11,nb), vmin2, 
     &      vmax2
10080     format (' Vmax (', f6.3, ') violates global limits (',
     &      f6.3, ',', f6.3, ')')
        endif
        if ((field .eq. 0 .or. field .eq. 62) .and.
     &      (busdta(12,nb) .gt. 0.0 .and. 
     &      (busdta(12,nb) .gt. vmax2 .or. busdta(12,nb) .lt. vmin2)))
     &      then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10090) busdta(12,nb), 
     &      busdta(11,nb)
10090     format (' Vmin (', f6.3, ') > Vmax (', f6.3, ')')
        endif
        if ((field .eq. 0 .or. field .eq. 78) .and.
     &      (kbsdta(1,nb) .eq. 8 .and. 
     &      (busdta(14,nb) .lt. 0.0 .or. busdta(14,nb) .gt. 100.0))) 
     &    then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10100) busdta(14,nb)
10100     format (' Pct (', f4.0, ') < 0 or > 100 ')
        endif

      else
        if ((field .eq. 0 .or. field .eq. 24) .and.
     &      (busdta(3,nb) .le. 0.0)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10120) busdta(3,nb)
10120     format (' Number of bridges  (', f4.0, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 26) .and.
     &      (busdta(4,nb) .lt. 0.00)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10130) busdta(4,nb)
10130     format (' Smoothing reactor (', f7.1, ') < 0 ')
        endif
        if ((field .eq. 0 .or. field .eq. 31 .or. field .eq. 36) .and.
     &      (busdta(5,nb) .lt. 0.0 .or. 
     &       busdta(5,nb) .gt. busdta(6,nb))) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10140) busdta(5,nb), busdta(6,nb)
10140     format (' Alpha_min (', f7.1, 
     &      ') < 0 or alpha_min > alpha_max (', f7.1, ')')
        endif
        if ((field .eq. 0 .or. field .eq. 41) .and.
     &      (busdta(7,nb) .lt. 0.0 .or. busdta(7,nb) .gt. 1000.0)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10142) busdta(7,nb)
10142     format (' Voltage drop (', f7.1, ') < 0 or > 1000 ')
        endif
        if ((field .eq. 0 .or. field .eq. 46) .and.
     &      (busdta(8,nb) .lt. 0.0 .or. busdta(7,nb) .gt. 3000.0)) then
          chk_busdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10144) busdta(8,nb)
10144     format (' Rating (', f7.1, ') < 0 or > 3000 ')
        endif
        if (kbsdta(1,nb) .eq. 12) then
          call getchr(1, type, kbsdta(12,nb))
          if ((field .eq. 0 .or. field .eq. 63) .and.
     &        (index ('IRM ', type) .eq. 0)) then
            chk_busdta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10150) type
10150       format (' Converter type (', a, ') must be { ,I,R,M} ')
          endif
          if ((field .eq. 0 .or. field .eq. 64 .or. field .eq. 67) .and.
     &        (busdta(10,nb) .lt. 0.0 .or. 
     &         busdta(10,nb) .gt. busdta(11,nb) .or.
     &         busdta(11,nb) .gt. 30.0)) then
            chk_busdta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10160) busdta(10,nb), 
     &        busdta(11,nb)
10160       format (' Alpha_nom (', f7.1, 
     &        ') < 0 or alpha_nom > gamma_min (', f7.1, ')')
          endif
          if ((field .eq. 0 .or. field .eq. 70) .and.
     &        (busdta(13,nb) .lt. -3000.0 .or. 
     &         busdta(13,nb) .gt. 3000.0)) then
            chk_busdta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10170) busdta(13,nb)
10170       format (' Pdc (', f7.1, ') < -3000 or > 3000 ')
          endif
          vdo = 1.35 * base(nb) * busdta(3,nb)
          if ((field .eq. 0 .or. field .eq. 76) .and.
     &       (busdta(14,nb) .ne. 0.0 .and.
     &        (busdta(14,nb) .lt. 0.75 * vdo .or. 
     &         busdta(14,nb) .gt. vdo))) then
            chk_busdta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10180) busdta(14,nb), vdo
10180       format (' Vdc (', f7.1, ') < 0.75 Vdo (', f7.1, 
     &        ') or > 1.35 * Vdo ')
          endif
        endif
      endif
      return
      end
