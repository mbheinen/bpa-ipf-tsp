C    @(#)kmpepc.f	20.3 2/13/96
      function kmpepc (i, j)
C
C     This function compares EPCTL(I) and EPCTL(J).
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/epridc.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      character tp_code(2)*2, dcbus1(2)*8, dcbus2(2)*8, dcckt(2)*1,
     1          acbus(2)*12, ident(2)*2
C
C        Sort according to the following fields:
C
C                      CC        CD, CR      CN
C
C           D-C bus 1 (8:15)     (6:13)      (81:88)
C           D-C bus 2 (16:23)    (14:21)     (89:96)
C           A-C bus   (24:35)    (22:33)     (97:108)
C           Circuit   (36:36)    (34:34)     (109:109)
C           Ident     (4:5)      (4:5)       (4:5)
C           Code      (1:2)      (1:2)       (1:2)
C
      if (i .eq. j) then
         kmpepc = 0
      else
         tp_code(1) = epctl(i)(1:2)
         tp_code(2) = epctl(j)(1:2)
         if (tp_code(1) .eq. 'CC') then
            dcbus1(1) = epctl(i)(8:15)
            dcbus2(1) = epctl(i)(16:23)
            acbus(1) = epctl(i)(24:35)
            dcckt(1) = epctl(i)(36:36)
            ident(1) = epctl(i)(4:5)
         else if (tp_code(1) .eq. 'CD' .or. tp_code(1) .eq. 'CR') then
            dcbus1(1) = epctl(i)(6:13)
            dcbus2(1) = epctl(i)(14:21)
            acbus(1) = epctl(i)(22:33)
            dcckt(1) = epctl(i)(34:34)
            ident(1) = epctl(i)(4:5)
         else if (tp_code(1) .eq. 'CN') then
            dcbus1(1) = epctl(i)(81:88)
            dcbus2(1) = epctl(i)(89:96)
            acbus(1) = epctl(i)(97:108)
            dcckt(1) = epctl(i)(109:109)
            ident(1) = epctl(i)(4:5)
         else
            write (errbuf(1), 100)
  100       format ('Illegal d-c record type:')
            write (errbuf(2), 110) epctl(i)(1:80)
  110       format (' (', a, ')')
            call prterx ('W', 2)
         endif
 
         if (tp_code(2) .eq. 'CC') then
            dcbus1(2) = epctl(j)(8:15)
            dcbus2(2) = epctl(j)(16:23)
            acbus(2) = epctl(j)(24:35)
            dcckt(2) = epctl(j)(36:36)
            ident(2) = epctl(j)(4:5)
         else if (tp_code(2) .eq. 'CD' .or. tp_code(2) .eq. 'CR') then
            dcbus1(2) = epctl(j)(6:13)
            dcbus2(2) = epctl(j)(14:21)
            acbus(2) = epctl(j)(22:33)
            dcckt(2) = epctl(j)(34:34)
            ident(2) = epctl(j)(4:5)
         else if (tp_code(2) .eq. 'CN') then
            dcbus1(2) = epctl(j)(81:88)
            dcbus2(2) = epctl(j)(89:96)
            acbus(2) = epctl(j)(97:108)
            dcckt(2) = epctl(j)(109:109)
            ident(2) = epctl(j)(4:5)
         else
            write (errbuf(1), 100)
            write (errbuf(2), 110) epctl(j)(1:80)
            call prterx ('W', 2)
         endif
 
         kmpepc = kompr (tp_code(1), tp_code(2), kmpepc)
         if (kmpepc .eq. 0) then
            kmpepc = kompr (dcbus1(1), dcbus1(2), kmpepc)
            if (kmpepc .eq. 0) then
               kmpepc = kompr (dcbus2(1), dcbus2(2), kmpepc)
               if (kmpepc .eq. 0) then
                  kmpepc = kompr (dcckt(1), dcckt(2), kmpepc)
                  if (kmpepc .eq. 0) then
                     kmpepc = kompr (ident(1), ident(2), kmpepc)
                  endif
               endif
            endif
         else
            if (tp_code(1) .eq. 'MD' .or. tp_code(2) .eq. 'MD') then
            else
               kmpepc = kompr (dcbus1(1), dcbus1(2), kmpepc)
               if (kmpepc .eq. 0) then
                  kmpepc = kompr (dcbus2(1), dcbus2(2), kmpepc)
                  if (kmpepc .eq. 0) then
                     kmpepc = kompr (dcckt(1), dcckt(2), kmpepc)
                     if (kmpepc .eq. 0) then
                        kmpepc = kompr (ident(1), ident(2), kmpepc)
                        if (kmpepc .eq. 0) then
                           kmpepc = kompr (tp_code(1), tp_code(2), 
     &                                     kmpepc)
                        endif
                     endif
                  endif
               endif
            endif
            if (kmpepc .eq. 0) then
               write (errbuf(1), 120)
  120          format ('Duplicate d-c records:')
               write (errbuf(2), 110) epctl(i)(1:80)
               write (errbuf(3), 110) epctl(j)(1:80)
               call prterx ('W', 3)
            endif
         endif
      endif
      return
      end
