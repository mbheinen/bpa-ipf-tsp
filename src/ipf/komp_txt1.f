C    @(#)komp_txt1.f	20.3 2/13/96
        integer function komp_txt1 (ix, jx) 
        implicit none
        integer ix, jx
c
c       This function sorts text1 by blocks and by network
c       data: 
c
c       "A", "I", and "B+XQR$LET"
c
        common /difference/ numtxt1, array1, text1, numtxt2, array2, 
     &                      text2, numchg, change

        integer MAXTEXT, MAXCHG
        parameter (MAXTEXT = 10000)
        parameter (MAXCHG = 4000)

        character text1(MAXTEXT)*120, text2(MAXTEXT)*120,
     &            change(MAXCHG)*120
        integer array1(MAXTEXT), numtxt1, array2(MAXTEXT), numtxt2, 
     &          numchg

        integer type1, type2, sec1, sec2, junk, ic, jc, kompr
        real base1, base2

        komp_txt1 = 0
        if (ix .eq. jx) go to 1040

        ic = array1(ix)
        jc = array1(jx)

        type1 = index ('AIB+XQR$LET',text1(ic)(1:1))
        if (type1 .eq. 7 .and. text1(ic)(2:2) .eq. 'Z') then
           type1 = type1 + 1
        endif
        type2 = index ('AIB+XQR$LET',text1(jc)(1:1))
        if (type2 .eq. 7 .and. text1(jc)(2:2) .eq. 'Z') then
           type2 = type2 + 1
        endif
        komp_txt1 = min0 (type1, 3) - min0 (type2, 3)
 
        if (komp_txt1 .ne. 0) go to 1040
 
        if (text1(ic)(1:1) .eq. 'A') then
 
           komp_txt1 = kompr (text1(ic)(4:13),text1(jc)(4:13),junk)
    
           if (komp_txt1 .eq. 0) then
              komp_txt1 = kompr (text1(ic)(2:2),text1(jc)(2:2),junk)
           endif

        else if (text1(ic)(1:1) .eq. 'I') then

           komp_txt1 = kompr(text1(ic)(4:13),text1(jc)(4:13),junk)
 
           if (komp_txt1.eq.0) then
              komp_txt1 = kompr(text1(ic)(15:24),text1(jc)(15:24),junk)
           endif
 
        else if (index ('B+XQLRET', text1(ic)(1:1)) .ne. 0) then
 
           komp_txt1 = kompr (text1(ic)(7:14), text1(jc)(7:14), junk)
 
           if (komp_txt1 .eq. 0) then
              read (text1(ic)(15:19),610) base1
  610         format (bz, f4.0)
              read (text1(jc)(15:19),610) base2
              komp_txt1 = 100.0*(base1 - base2)
           endif
             
           if (komp_txt1 .eq. 0) then
              type1 = index ('B+XQR$LET', text1(ic)(1:1))
              if (type1 .eq. 5 .and. text1(ic)(2:2) .eq. 'Z') then
                 type1 = type1 + 1
              endif
              type2 = index ('B+XQR$LET', text1(jc)(1:1))
              if (type2 .eq. 5 .and. text1(jc)(2:2) .eq. 'Z') then
                 type2 = type2 + 1
              endif

              komp_txt1 = min0 (type1, 5) - min0 (type2, 5)
 
              if (komp_txt1 .ne. 0) go to 1040

              if (text1(ic)(1:1) .eq. 'B') then 

              else if (text1(ic)(1:1) .eq. '+') then 
 
                 komp_txt1 = kompr (text1(ic)(2:2), text1(jc)(2:2),
     &                           junk)
                 if (komp_txt1 .eq. 0) then
                    komp_txt1 = kompr (text1(ic)(4:6),
     &                              text1(jc)(4:6), junk)
                    if (komp_txt1 .eq. 0) then
                       komp_txt1 = kompr (text1(ic)(19:20),
     &                                 text1(jc)(19:20), junk)
                    endif
                 endif
 
              else if (text1(ic)(1:1) .eq. 'X') then 
 
              else if (text1(ic)(1:1) .eq. 'Q') then 
 
                 komp_txt1 = index ('PXN', text1(ic)(2:2)) - 
     &                    index ('PXN', text1(jc)(2:2))
 
              else if (index ('LRET', text1(ic)(1:1)) .ne. 0) then 

                 komp_txt1 = kompr (text1(ic)(20:27), 
     &                             text1(jc)(20:27), junk)
                 if (komp_txt1 .eq. 0) then
                    read (text1(ic)(28:31),610) base1
                    read (text1(jc)(28:31),610) base2
                    komp_txt1 = 100.0 * (base1 - base2)
                    if (komp_txt1 .eq. 0) then
c
c                      Compare "R" with non-"R"
c
                       komp_txt1 = min0 (type1,2) - min0 (type2,2)
                       if (komp_txt1 .eq. 0) then 
                           komp_txt1 = kompr (text1(ic)(32:32),
     &                                     text1(jc)(32:32), junk)
                           if (komp_txt1 .eq. 0) then
                              read (text1(ic)(33:33),1010) sec1
 1010                         format (bz, i2)
                              read (text1(jc)(33:33),1010) sec2
                              komp_txt1 = sec1 - sec2
                              if (komp_txt1 .eq. 0) then
                                  komp_txt1 = type1 - type2
                              endif
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
