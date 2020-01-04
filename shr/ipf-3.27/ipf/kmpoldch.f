C    @(#)kmpoldch.f	20.3 2/13/96
C****************************************************************
C
C   	File: kmpoldch.f
C
C   	Purpose: Sort oldchg records to faciliate change reduction.
C                The set of change records will be a contiguous set.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: qiksrt.f through savechgs.f
C
C****************************************************************
C
        function kmpoldch (c1, c2) 
        integer c1, c2

        include 'ipfinc/parametr.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/oldchg.inc'
        include 'ipfinc/qksrt.inc'

        common /scratch/ array(MAXCHG)
        integer array

        integer type1, type2, sec1, sec2, gtchgtyp

        i1 = array(c1)
        i2 = array(c2)
        
        kmpoldch = 0
        if (i1 .eq. i2) go to 1040
c
c       gtchgtyp returns an index corresponding to the following
c       string:  '/.DPZAIB+XQEL$$R$T'
c
        type1 = gtchgtyp(oldchg(i1)(1:2))
        type2 = gtchgtyp(oldchg(i2)(1:2))
c
c       Sorts oldchg(i1) by blocks and by network data: 
c
c       "D", "P", "Z", "A", "I", and "B+XQEL$$R$T"
c
        kmpoldch = min0 (type1, 8) - min0 (type2, 8)
 
        if (kmpoldch .ne. 0) go to 1040
 
        if (oldchg(i1)(1:1) .eq. 'D') then
 
           kmpoldch = kompr (oldchg(i1)(2:2),oldchg(i2)(2:2),junk)
 
           if (kmpoldch .eq. 0) then
              kmpoldch = kompr (oldchg(i1)(4:13),oldchg(i2)(4:13),junk)
           endif
           if (kmpoldch .eq. 0 .and. key .eq. 0) then
              read (oldchg(i1)(122:125), '(bz, i4)') num1
              read (oldchg(i2)(122:125), '(bz, i4)') num2
              kmpoldch = num1 - num2
           endif

        else if (oldchg(i1)(1:1) .eq. 'P') then
 
           kmpoldch = kompr (oldchg(i1)(2:2),oldchg(i2)(2:2),junk)
   
           if (kmpoldch .eq. 0) then
               kmpoldch = kompr (oldchg(i1)(4:6),oldchg(i2)(4:6),junk)
           endif

           if (kmpoldch .eq. 0 .and. key .eq. 0) then
              read (oldchg(i1)(122:125), '(bz, i4)') num1
              read (oldchg(i2)(122:125), '(bz, i4)') num2
              kmpoldch = num1 - num2
           endif

        else if (oldchg(i1)(1:1) .eq. 'Z') then
 
           if (key .eq. 0) then
              read (oldchg(i1)(122:125), '(bz, i4)') num1
              read (oldchg(i2)(122:125), '(bz, i4)') num2
              kmpoldch = num1 - num2
           endif

        else if (oldchg(i1)(1:1) .eq. 'A') then
 
           kmpoldch = kompr (oldchg(i1)(2:2),oldchg(i2)(2:2),junk)
    
           if (kmpoldch .eq. 0) then
              kmpoldch = kompr (oldchg(i1)(4:13),oldchg(i2)(4:13),junk)
           endif

           if (kmpoldch .eq. 0 .and. key .eq. 0) then
              read (oldchg(i1)(122:125), '(bz, i4)') num1
              read (oldchg(i2)(122:125), '(bz, i4)') num2
              kmpoldch = num1 - num2
           endif

        else if (oldchg(i1)(1:1) .eq. 'I') then

           kmpoldch = kompr(oldchg(i1)(4:13),oldchg(i2)(4:13),junk)
 
           if (kmpoldch.eq.0) then
              kmpoldch = kompr(oldchg(i1)(15:24),oldchg(i2)(15:24),junk)
           endif

           if (kmpoldch .eq. 0 .and. key .eq. 0) then
              read (oldchg(i1)(122:125), '(bz, i4)') num1
              read (oldchg(i2)(122:125), '(bz, i4)') num2
              kmpoldch = num1 - num2
           endif
c
c          Network data is sorted by the following fields:
c
c             1. bus bus1, base1, 
c             2. B, +, X, Q, {LERT} 
c             3. bus2, base2
c             4. R, id
c             5. sect
c             6. num
c 
        else if (type1 .ge. 8) then
 
           kmpoldch = kompr (oldchg(i1)(7:14), oldchg(i2)(7:14), junk)
 
           if (kmpoldch .eq. 0) then
              read (oldchg(i1)(15:19),610) base1
  610         format (bz, f4.0)
              read (oldchg(i2)(15:19),610) base2
              kmpoldch = 100.0*(base1 - base2)
           endif
             
           if (kmpoldch .eq. 0) then
c
c             type1 corresponds to the string '/.DPZAIB+XQEL$$R$T'
c
              kmpoldch = min0 (type1, 12) - min0 (type2, 12)
 
              if (kmpoldch .ne. 0) go to 1040

              if (oldchg(i1)(1:1) .eq. 'B') then 

                 if (key .eq. 0) then
                    read (oldchg(i1)(122:125), '(bz, i4)') num1
                    read (oldchg(i2)(122:125), '(bz, i4)') num2
                    kmpoldch = num1 - num2
                 endif

              else if (oldchg(i1)(1:1) .eq. '+') then 
 
                kmpoldch = kompr (oldchg(i1)(2:2), oldchg(i2)(2:2),
     &                             junk)
                if (kmpoldch .eq. 0) then
                    kmpoldch = kompr (oldchg(i1)(4:6),
     &                                oldchg(i2)(4:6), junk)
                    if (kmpoldch .eq. 0) then
                       kmpoldch = kompr (oldchg(i1)(19:20),
     &                                   oldchg(i2)(19:20), junk)
                       if (kmpoldch .eq. 0 .and. key .eq. 0) then
                          read (oldchg(i1)(122:125), '(bz, i4)') num1
                          read (oldchg(i2)(122:125), '(bz, i4)') num2
                          kmpoldch = num1 - num2
                       endif
                    endif
                 endif
 
              else if (oldchg(i1)(1:1) .eq. 'X') then 
 
                 if (key .eq. 0) then
                    read (oldchg(i1)(122:125), '(bz, i4)') num1
                    read (oldchg(i2)(122:125), '(bz, i4)') num2
                    kmpoldch = num1 - num2
                 endif

              else if (oldchg(i1)(1:1) .eq. 'Q') then 
 
                 kmpoldch = index ('PXN', oldchg(i1)(2:2)) - 
     &                      index ('PXN', oldchg(i2)(2:2))
                 if (kmpoldch .eq. 0 .and. key .eq. 0) then
                    read (oldchg(i1)(122:125), '(bz, i4)') num1
                    read (oldchg(i2)(122:125), '(bz, i4)') num2
                    kmpoldch = num1 - num2
                 endif
 
              else if (index ('LRET', oldchg(i1)(1:1)) .ne. 0) then 

                 kmpoldch = kompr (oldchg(i1)(20:27), 
     &                             oldchg(i2)(20:27), junk)
                 if (kmpoldch .eq. 0) then
                    read (oldchg(i1)(28:31),610) base1
                    read (oldchg(i2)(28:31),610) base2
                    kmpoldch = 100.0 * (base1 - base2)
                    if (kmpoldch .eq. 0) then
c
c                      Compare "R" with non-"R"
c
                       if (type1 .eq. 15 .or. type2 .eq. 15) then
                           kmpoldch = type1 -  type2
                       endif
                       if (kmpoldch .eq. 0) then 
                           kmpoldch = kompr (oldchg(i1)(32:32),
     &                                     oldchg(i2)(32:32), junk)
                           if (kmpoldch .eq. 0) then
                              read (oldchg(i1)(33:33),1010) sec1
 1010                         format (bz, i1)
                              read (oldchg(i2)(33:33),1010) sec2
                              kmpoldch = sec1 - sec2
                           endif
                           if (kmpoldch .eq. 0) then
                              kmpoldch = type1 - type2
                           endif
                           if (kmpoldch .eq. 0 .and. key .eq. 0) then
                              read (oldchg(i1)(122:125), '(bz, i4)') 
     &                           num1
                              read (oldchg(i2)(122:125), '(bz, i4)') 
     &                           num2
                              kmpoldch = num1 - num2
                           endif
                       endif
                    endif
                 endif
              endif
           endif
        endif

        if (kmpoldch .eq. 0 .and. key .eq. 0) then
            read (oldchg(i1)(122:125), '(bz, i4)') sec1
            read (oldchg(i2)(122:125), '(bz, i4)') sec2
            kmpoldch = sec1 - sec2
        endif

 1040   continue
        return
        end
