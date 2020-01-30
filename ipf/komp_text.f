C    @(#)komp_text.f	20.3 2/13/96
        integer function komp_text (text1, text2) 
        implicit none
        character *(*) text1, text2
c
c       This function sorts text1 by blocks and by network
c       data: 
c
c       "A", "I", and "B+XQR$LET"
c
        integer type1, type2, sec1, sec2, junk, ic, jc, kompr
        real base1, base2

        komp_text = 0

        type1 = index ('AIB+XQR$LET',text1(1:1))
        if (type1 .eq. 7 .and. text1(2:2) .eq. 'Z') then
           type1 = type1 + 1
        endif
        type2 = index ('AIB+XQR$LET',text2(1:1))
        if (type2 .eq. 7 .and. text2(2:2) .eq. 'Z') then
           type2 = type2 + 1
        endif
        komp_text = min0 (type1, 3) - min0 (type2, 3)
 
        if (komp_text .ne. 0) go to 1040
 
        if (text1(1:1) .eq. 'A') then
 
           komp_text = kompr (text1(4:13),text2(4:13),junk)
    
           if (komp_text .eq. 0) then
              komp_text = kompr (text1(2:2),text2(2:2),junk)
           endif

        else if (text1(1:1) .eq. 'I') then

           komp_text = kompr(text1(4:13),text2(4:13),junk)
 
           if (komp_text.eq.0) then
              komp_text = kompr(text1(15:24),text2(15:24),junk)
           endif
 
        else if (index ('B+XQLRET', text1(1:1)) .ne. 0) then
 
           komp_text = kompr (text1(7:14), text2(7:14), junk)
 
           if (komp_text .eq. 0) then
              read (text1(15:19),610) base1
  610         format (bz, f4.0)
              read (text2(15:19),610) base2
              komp_text = 100.0*(base1 - base2)
           endif
             
           if (komp_text .eq. 0) then
              type1 = index ('B+XQR$LET', text1(1:1))
              if (type1 .eq. 5 .and. text1(2:2) .eq. 'Z') then
                 type1 = type1 + 1
              endif
              type2 = index ('B+XQR$LET', text2(1:1))
              if (type2 .eq. 5 .and. text2(2:2) .eq. 'Z') then
                 type2 = type2 + 1
              endif

              komp_text = min0 (type1, 5) - min0 (type2, 5)
 
              if (komp_text .ne. 0) go to 1040

              if (text1(1:1) .eq. 'B') then 

              else if (text1(1:1) .eq. '+') then 
 
                 komp_text = kompr (text1(2:2), text2(2:2),
     &                           junk)
                 if (komp_text .eq. 0) then
                    komp_text = kompr (text1(4:6),
     &                              text2(4:6), junk)
                    if (komp_text .eq. 0) then
                       komp_text = kompr (text1(19:20),
     &                                 text2(19:20), junk)
                    endif
                 endif
 
              else if (text1(1:1) .eq. 'X') then 
 
              else if (text1(1:1) .eq. 'Q') then 
 
                 komp_text = index ('PXN', text1(2:2)) - 
     &                    index ('PXN', text2(2:2))
 
              else if (index ('LRET', text1(1:1)) .ne. 0) then 

                 komp_text = kompr (text1(20:27), 
     &                             text2(20:27), junk)
                 if (komp_text .eq. 0) then
                    read (text1(28:31),610) base1
                    read (text2(28:31),610) base2
                    komp_text = 100.0 * (base1 - base2)
                    if (komp_text .eq. 0) then
c
c                      Compare "R" with non-"R"
c
                       komp_text = min0 (type1,2) - min0 (type2,2)
                       if (komp_text .eq. 0) then 
                           komp_text = kompr (text1(32:32),
     &                                     text2(32:32), junk)
                           if (komp_text .eq. 0) then
                              read (text1(33:33),1010) sec1
 1010                         format (bz, i2)
                              read (text2(33:33),1010) sec2
                              komp_text = sec1 - sec2
                              if (komp_text .eq. 0) then
                                  komp_text = type1 - type2
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
