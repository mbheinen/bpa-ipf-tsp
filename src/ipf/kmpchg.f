C    @(#)kmpchg.f	20.3 2/13/96
        function kmpchg (i1,i2) 
c
c       This function sorts chgcrd(i1) by blocks and by network
c       data: 
c
c       "D", "A", "I", "P", "Z", and "B+XQR$LET"
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/changr.inc'

        integer type1,type2,sec1,sec2

        kmpchg = 0
        if (i1 .eq. i2) go to 1040

        type1 = index ('DAIPZB+XQR$LET',chgcrd(i1)(1:1))
        if (type1 .eq. 10 .and. chgcrd(i1)(2:2) .eq. 'Z') then
           type1 = type1 + 1
        endif
        type2 = index ('DAIPZB+XQR$LET',chgcrd(i2)(1:1))
        if (type2 .eq. 10 .and. chgcrd(i2)(2:2) .eq. 'Z') then
           type2 = type2 + 1
        endif
        kmpchg = min0 (type1, 6) - min0 (type2, 6)
 
        if (kmpchg .ne. 0) go to 1040
 
        if (chgcrd(i1)(1:1) .eq. 'D') then
 
           kmpchg = kompr (chgcrd(i1)(2:2),chgcrd(i2)(2:2),junk)
 
           if (kmpchg .eq. 0) then
              kmpchg = kompr (chgcrd(i1)(4:13),chgcrd(i2)(4:13),junk)
           endif

        else if (chgcrd(i1)(1:1) .eq. 'A') then
 
           kmpchg = kompr (chgcrd(i1)(4:13),chgcrd(i2)(4:13),junk)
    
           if (kmpchg .eq. 0) then
              kmpchg = kompr (chgcrd(i1)(2:2),chgcrd(i2)(2:2),junk)
           endif

           if (kmpchg .eq. 0) then
              kmpchg = index ('DR M', chgcrd(i1)(3:3))
     &               - index ('DR M', chgcrd(i2)(3:3))
           endif
 
        else if (chgcrd(i1)(1:1) .eq. 'I') then

           kmpchg = kompr(chgcrd(i1)(4:13),chgcrd(i2)(4:13),junk)
 
           if (kmpchg.eq.0) then
              kmpchg = kompr(chgcrd(i1)(15:24),chgcrd(i2)(15:24),junk)
           endif
           if (kmpchg .eq. 0) then
              kmpchg = index ('DR M', chgcrd(i1)(3:3))
     &               - index ('DR M', chgcrd(i2)(3:3))
           endif
 
        else if (chgcrd(i1)(1:1) .eq. 'P') then
 
           kmpchg = kompr (chgcrd(i1)(2:2),chgcrd(i2)(2:2),junk)
   
           if (kmpchg .eq. 0) then
               kmpchg = kompr (chgcrd(i1)(4:6),chgcrd(i2)(4:6),junk)
               if (kmpchg .eq. 0) then
                  kmpchg = kompr(chgcrd(i1)(35:80),chgcrd(i2)(35:80),
     &                           junk)
               endif
           endif

        else if (chgcrd(i1)(1:1) .eq. 'Z') then
 
           kmpchg = 0

        else if (index ('B+XQLRET', chgcrd(i1)(1:1)) .ne. 0) then
 
           kmpchg = kompr (chgcrd(i1)(7:14), chgcrd(i2)(7:14), junk)
 
           if (kmpchg .eq. 0) then
              read (chgcrd(i1)(15:19),610) base1
  610         format (bz, f4.0)
              read (chgcrd(i2)(15:19),610) base2
              kmpchg = 100.0*(base1 - base2)
           endif
             
           if (kmpchg .eq. 0) then
              type1 = index ('B+XQR$LET', chgcrd(i1)(1:1))
              if (type1 .eq. 5 .and. chgcrd(i1)(2:2) .eq. 'Z') then
                 type1 = type1 + 1
              endif
              type2 = index ('B+XQR$LET', chgcrd(i2)(1:1))
              if (type2 .eq. 5 .and. chgcrd(i2)(2:2) .eq. 'Z') then
                 type2 = type2 + 1
              endif

              kmpchg = min0 (type1, 5) - min0 (type2, 5)
 
              if (kmpchg .ne. 0) go to 1040

              if (chgcrd(i1)(1:1) .eq. 'B') then 

                 kmpchg = index ('DR M', chgcrd(i1)(3:3))
     &                  - index ('DR M', chgcrd(i2)(3:3))

              else if (chgcrd(i1)(1:1) .eq. '+') then 
 
                 
                 kmpchg = kompr (chgcrd(i1)(2:2), chgcrd(i2)(2:2),
     &                           junk)
                 if (kmpchg .eq. 0) then
                    kmpchg = kompr (chgcrd(i1)(4:6),
     &                              chgcrd(i2)(4:6), junk)
                    if (kmpchg .eq. 0) then
                       kmpchg = kompr (chgcrd(i1)(19:20),
     &                                 chgcrd(i2)(19:20), junk)
                       if (kmpchg .eq. 0) then
                          kmpchg = index ('DR M', chgcrd(i1)(3:3))
     &                           - index ('DR M', chgcrd(i2)(3:3))
                       endif
                    endif
                 endif
 
              else if (chgcrd(i1)(1:1) .eq. 'X') then 
 
                 kmpchg = index ('DR M', chgcrd(i1)(3:3))
     &                  - index ('DR M', chgcrd(i2)(3:3))

              else if (chgcrd(i1)(1:1) .eq. 'Q') then 
 
                 kmpchg = index ('PXN', chgcrd(i1)(2:2)) - 
     &                    index ('PXN', chgcrd(i2)(2:2))
                 if (kmpchg .eq. 0) then
                    kmpchg = index ('DR M', chgcrd(i1)(3:3))
     &                     - index ('DR M', chgcrd(i2)(3:3))
                 endif
 
              else if (index ('LRET', chgcrd(i1)(1:1)) .ne. 0) then 

                 kmpchg = kompr (chgcrd(i1)(20:27), 
     &                             chgcrd(i2)(20:27), junk)
                 if (kmpchg .eq. 0) then
                    read (chgcrd(i1)(28:31),610) base1
                    read (chgcrd(i2)(28:31),610) base2
                    kmpchg = 100.0 * (base1 - base2)
                    if (kmpchg .eq. 0) then
c
c                      Compare "R" with non-"R"
c
                       kmpchg = min0 (type1,2) - min0 (type2,2)
                       if (kmpchg .eq. 0) then 
                           kmpchg = kompr (chgcrd(i1)(32:32),
     &                                     chgcrd(i2)(32:32), junk)
                           if (kmpchg .eq. 0) then
                              read (chgcrd(i1)(33:33),1010) sec1
 1010                         format (bz, i1)
                              if (chgcrd(i1)(121:121) .eq. 'T')
     &                           sec1 = mod (10-sec1, 10)
                              read (chgcrd(i2)(33:33),1010) sec2
                              if (chgcrd(i2)(121:121) .eq. 'T')
     &                           sec2 = mod (10-sec2, 10)
                              kmpchg = sec1 - sec2
                              if (kmpchg .eq. 0) then
                                  kmpchg = type1 - type2
                                 if (kmpchg .eq. 0) then
                                    kmpchg = index ('DR M', 
     &                                              chgcrd(i1)(3:3))
     &                                     - index ('DR M', 
     &                                              chgcrd(i2)(3:3))
           endif
                              endif
                          endif
                       endif
                    endif
                 endif
              endif
           endif
        endif

        if (kmpchg .eq. 0) then
            read (chgcrd(i1)(122:125), '(bz, i4)') sec1
            read (chgcrd(i2)(122:125), '(bz, i4)') sec2
            kmpchg = sec1 - sec2
        endif

 1040   continue
        return
        end
