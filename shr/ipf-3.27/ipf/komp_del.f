C    @(#)komp_del.f	20.3 2/13/96
        integer function komp_del (i1, i2)

c       This function compares DELETE(i1) with DELETE(i2).

        include 'ipfinc/parametr.inc'
        include 'ipfinc/delete.inc'

        integer type1, type2, sec1, sec2
 
        if (i1 .eq. i2) then
           komp_del = 0
        else

           komp_del = kompr (delete(i1)(7:14), delete(i2)(7:14), junk)
 
           if (komp_del .eq. 0) then
              read (delete(i1)(15:19),610) base1
  610         format (bz, f4.0)
              read (delete(i2)(15:19),610) base2
              komp_del = 100.0*(base1 - base2)
           endif
             
           if (komp_del .eq. 0) then
              type1 = index ('B+XR$LET', delete(i1)(1:1))
              if (type1 .eq. 4 .and. delete(i1)(2:2) .eq. 'Z') then
                 type1 = type1 + 1
              endif
              type2 = index ('B+XR$LET', delete(i2)(1:1))
              if (type2 .eq. 4 .and. delete(i2)(2:2) .eq. 'Z') then
                 type2 = type2 + 1
              endif

              komp_del = min0 (type1,4) - min0 (type2,4)
 
              if (komp_del .ne. 0) go to 1040

              if (delete(i1)(1:1) .eq. '+') then 
 
                 if (komp_del .eq. 0) then
                    komp_del = kompr (delete(i1)(2:2), delete(i2)(2:2),
     &                                junk)
                    if (komp_del .eq. 0) then
                       komp_del = kompr (delete(i1)(4:6),
     &                                   delete(i2)(4:6), junk)
                       if (komp_del .eq. 0) then
                          komp_del = kompr (delete(i1)(19:20),
     &                                      delete(i2)(19:20), junk)
                       endif
                    endif
                 endif
 
              else if (delete(i1)(1:1) .eq. 'X') then 
 
                 komp_del = 0

              else if (index ('LRET', delete(i1)(1:1)) .ne. 0) then 

                 komp_del = kompr (delete(i1)(20:27), 
     &                             delete(i2)(20:27), junk)
                 if (komp_del .eq. 0) then
                    read (delete(i1)(28:31),610) base1
                    read (delete(i2)(28:31),610) base2
                    komp_del = 100.0 * (base1 - base2)
                    if (komp_del .eq. 0) then
c
c                      Compare "R" with non-"R"
c
                       komp_del = min0 (type1,2) - min0 (type2,2)
                       if (komp_del .eq. 0) then 
                           if (delete(i1)(32:32) .eq. '*' .or.
     &                         delete(i2)(32:32) .eq. '*') then
                           else
                              komp_del = kompr (delete(i1)(32:32),
     &                                          delete(i2)(32:32), junk)
                              if (komp_del .eq. 0) then
                                 read (delete(i1)(33:33),1010) sec1
 1010                            format (bz, i1)
                                 if (delete(i1)(3:3) .eq. 'T')
     &                              sec1 = mod (10 - sec1, 10)
                                 read (delete(i2)(33:33),1010) sec2
                                 if (delete(i2)(3:3) .eq. 'T')
     &                              sec2 = mod (10 - sec2, 10)
                                 if (sec1 .eq. 0 .or. sec2 .eq. 0) then
                                     komp_del = 0
                                 else
                                     komp_del = sec1 - sec2
                                     if (komp_del .eq. 0) then
                                        komp_del = type1 - type2
                                     endif
                                 endif
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
