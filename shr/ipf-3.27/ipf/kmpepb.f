C    @(#)kmpepb.f	20.3 2/13/96
      function kmpepb (i, j)
C
C     This function compares EPBUS(I) and EPBUS(J).
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/epridc.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      character type_code(2)*2, dcbus1(2)*8, dcbus2(2)*8, dcckt(2)*1
C
C     Sort according to the following fields:
C
C     Code      (1:2) < 'MD' and Type (4:4)
C     D-C bus 1 (4:11)
C     D-C bus 2 (12:19)
C     Circuit   (32:32) or (20:20)
C
      if (i .eq. j) then
         kmpepb = 0
      else
         type_code(1) = epbus(i)(1:2)
         type_code(2) = epbus(j)(1:2)
         if (type_code(1) .eq. 'BD' .or. type_code(1) .eq. 'BZ') then
            dcbus1(1) = epbus(i)(4:11)
            dcbus2(1) = epbus(i)(12:19)
            dcckt(1) = epbus(i)(32:32)
         else if (type_code(1) .eq. 'LD') then
            dcbus1(1) = epbus(i)(4:11)
            dcbus2(1) = epbus(i)(12:19)
            dcckt(1) = epbus(i)(20:20)
         else if (type_code(1) .eq. 'MD') then
            dcbus1(1) = epbus(i)(4:4)
            dcbus2(1) = '~~~~~~~~'
            dcckt(1) = '~'
         else
            write (errbuf(1), 100)
  100       format ('Illegal d-c record type:')
            write (errbuf(2), 110) epbus(i)(1:80)
  110       format (' (', a, ')')
            call prterx ('W', 2)
         endif
 
         if (type_code(2) .eq. 'BD' .or. type_code(2) .eq. 'BZ') then
            dcbus1(2) = epbus(j)(4:11)
            dcbus2(2) = epbus(j)(12:19)
            dcckt(2) = epbus(j)(32:32)
         else if (type_code(2) .eq. 'LD') then
            dcbus1(2) = epbus(j)(4:11)
            dcbus2(2) = epbus(j)(12:19)
            dcckt(2) = epbus(j)(20:20)
         else if (type_code(2) .eq. 'MD') then
            dcbus1(2) = epbus(j)(4:4)
            dcbus2(2) = '~~~~~~~~'
            dcckt(2) = '~'
         else
            write (errbuf(1), 100)
            write (errbuf(2), 110) epbus(j)(1:80)
            call prterx ('W', 2)
         endif
 
         kmpepb = kompr (type_code(1), type_code(2), kmpepb)
         if (kmpepb .eq. 0) then
            kmpepb = kompr (dcbus1(1), dcbus1(2), kmpepb)
            if (kmpepb .eq. 0) then
               kmpepb = kompr (dcbus2(1), dcbus2(2), kmpepb)
               if (kmpepb .eq. 0) then
                  kmpepb = kompr (dcckt(1), dcckt(2), kmpepb)
               endif
            endif
         else
            if (type_code(1) .eq. 'MD' .or. type_code(2) .eq. 'MD') then
            else
               kmpepb = kompr (dcbus1(1), dcbus1(2), kmpepb)
               if (kmpepb .eq. 0) then
                  kmpepb = kompr (dcbus2(1), dcbus2(2), kmpepb)
                  if (kmpepb .eq. 0) then
                     kmpepb = kompr (dcckt(1), dcckt(2), kmpepb)
                     if (kmpepb .eq. 0) then
                        kmpepb = kompr(type_code(1),type_code(2),kmpepb)
                     endif
                  endif
               endif
            endif
            if (kmpepb .eq. 0) then
               write (errbuf(1), 120)
  120          format ('Duplicate d-c records:')
               write (errbuf(2), 110) epbus(i)(1:80)
               write (errbuf(3), 110) epbus(j)(1:80)
               call prterx ('W', 3)
            endif
         endif
      endif
      return
      end
