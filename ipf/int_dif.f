C    @(#)int_dif.f	20.3 2/13/96
        character *(*) function int_dif (text1, text2)
        character text1 *(*), text2 *(*)
c
c       Determine intertie changes between text1 and text2
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
c
        int_dif = text1
        int_dif(3:3) = 'M'
        int_dif(25:) = ' '

        if (text1(4:13) .ne. text2(4:13) .or.
     &      text1(15:24) .ne. text2(15:24)) then
           write (errbuf(1), 110) text1(1:34)
  110      format (' Modified records do not match ', a)
           write (errbuf(2), 120) text2(1:34)
  120      format ('                               ', a)
           call prterx ('W', 2)
           go to 920
        endif

        if (text1(27:34) .ne. text2(27:34)) then
           if (text2(27:34) .ne. ' ') then
              int_dif(27:34) = text2(27:34)
           else
              int_dif(27:34) = '000000000'
           endif
        endif

  920   continue
        return
        end
