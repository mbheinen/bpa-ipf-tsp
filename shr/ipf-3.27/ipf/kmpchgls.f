C    @(#)kmpchgls.f	20.3 2/13/96
C****************************************************************
C
C   	File: kmpchgls.f
C
C   	Purpose: Sort chgcrd records in accordance with original
C                input order.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: qiksrt.f via chglis.f
C
C****************************************************************
C
        function kmpchgls (c1, c2) 
        integer c1, c2

        include 'ipfinc/parametr.inc'
        include 'ipfinc/changr.inc'

        common /scratch/ array(MAXCHG)
        integer array
 
        integer type1, type2, sec1, sec2, gtchgtyp

        i1 = array(c1)
        i2 = array(c2)
        
        read (chgcrd(i1)(122:125), '(bz, i4)') sec1
        read (chgcrd(i2)(122:125), '(bz, i4)') sec2
        kmpchgls = sec1 - sec2

        if (kmpchgls .ne. 0) go to 1040
c
c       gtchgtyp returns an index corresponding to the following
c       string:  '/.DPZAIB+XQEL$$R$T'
c
        type1 = gtchgtyp(chgcrd(i1)(1:2))
        type2 = gtchgtyp(chgcrd(i2)(1:2))
c
c       Sorts chgcrd(i1) by blocks and by network data: 
c
c       "D", "P", "Z", "A", "I", and "B+XQEL$$R$T"
c
        kmpchgls = min0 (type1, 8) - min0 (type2, 8)
 
        if (kmpchgls .ne. 0) go to 1040
 
        if (chgcrd(i1)(1:1) .eq. 'D') then
 
           kmpchgls = kompr (chgcrd(i1)(2:2),chgcrd(i2)(2:2),junk)
 
           if (kmpchgls .eq. 0) then
              kmpchgls = kompr (chgcrd(i1)(4:13),chgcrd(i2)(4:13),junk)
           endif

        else if (chgcrd(i1)(1:1) .eq. 'P') then
 
           kmpchgls = kompr (chgcrd(i1)(2:2),chgcrd(i2)(2:2),junk)
   
           if (kmpchgls .eq. 0) then
               kmpchgls = kompr (chgcrd(i1)(4:6),chgcrd(i2)(4:6),junk)
           endif

        else if (chgcrd(i1)(1:1) .eq. 'Z') then
 
        else if (chgcrd(i1)(1:1) .eq. 'A') then
 
           kmpchgls = kompr (chgcrd(i1)(2:2),chgcrd(i2)(2:2),junk)
    
           if (kmpchgls .eq. 0) then
              kmpchgls = kompr (chgcrd(i1)(4:13),chgcrd(i2)(4:13),junk)
           endif

        else if (chgcrd(i1)(1:1) .eq. 'I') then

           kmpchgls = kompr(chgcrd(i1)(4:13),chgcrd(i2)(4:13),junk)
 
           if (kmpchgls.eq.0) then
              kmpchgls = kompr(chgcrd(i1)(15:24),chgcrd(i2)(15:24),junk)
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
 
           kmpchgls = kompr (chgcrd(i1)(7:14), chgcrd(i2)(7:14), junk)
 
           if (kmpchgls .eq. 0) then
              read (chgcrd(i1)(15:19),610) base1
  610         format (bz, f4.0)
              read (chgcrd(i2)(15:19),610) base2
              kmpchgls = 100.0*(base1 - base2)
           endif
             
           if (kmpchgls .eq. 0) then
c
c             type1 corresponds to the string '/.DPZAIB+XQEL$$R$T'
c
              kmpchgls = min0 (type1, 12) - min0 (type2, 12)
 
              if (kmpchgls .ne. 0) go to 1040

              if (chgcrd(i1)(1:1) .eq. 'B') then 

              else if (chgcrd(i1)(1:1) .eq. '+') then 
 
                kmpchgls = kompr (chgcrd(i1)(2:2), chgcrd(i2)(2:2),
     &                             junk)
                if (kmpchgls .eq. 0) then
                    kmpchgls = kompr (chgcrd(i1)(4:6),
     &                                chgcrd(i2)(4:6), junk)
                    if (kmpchgls .eq. 0) then
                       kmpchgls = kompr (chgcrd(i1)(19:20),
     &                                   chgcrd(i2)(19:20), junk)
                    endif
                 endif
 
              else if (chgcrd(i1)(1:1) .eq. 'X') then 
 
              else if (chgcrd(i1)(1:1) .eq. 'Q') then 
 
                 kmpchgls = index ('PXN', chgcrd(i1)(2:2)) - 
     &                      index ('PXN', chgcrd(i2)(2:2))
 
              else if (index ('LRET', chgcrd(i1)(1:1)) .ne. 0) then 

                 kmpchgls = kompr (chgcrd(i1)(20:27), 
     &                             chgcrd(i2)(20:27), junk)
                 if (kmpchgls .eq. 0) then
                    read (chgcrd(i1)(28:31),610) base1
                    read (chgcrd(i2)(28:31),610) base2
                    kmpchgls = 100.0 * (base1 - base2)
                    if (kmpchgls .eq. 0) then
c
c                      Compare "R" with non-"R"
c
                       if (type1 .eq. 15 .or. type2 .eq. 15) then
                           kmpchgls = type1 -  type2
                       endif
                       if (kmpchgls .eq. 0) then 
                           kmpchgls = kompr (chgcrd(i1)(32:32),
     &                                     chgcrd(i2)(32:32), junk)
                           if (kmpchgls .eq. 0) then
                              read (chgcrd(i1)(33:33),1010) sec1
 1010                         format (bz, i1)
                              read (chgcrd(i2)(33:33),1010) sec2
                              kmpchgls = sec1 - sec2
                           endif
                           if (kmpchgls .eq. 0) then
                              kmpchgls = type1 - type2
                           endif
                       endif
                    endif
                 endif
              endif
           endif
        endif


 1040   continue
        return
        end
