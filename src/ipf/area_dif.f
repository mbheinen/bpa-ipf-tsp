C    @(#)area_dif.f	20.3 2/13/96
c***
c***  currently not used, but may be used in the future
c***  modify records for area records must contain all fields so
c***   "A ?"-old  "A ?"-new   style changes for result in merely
c***  the new record with the "?" changed to "M"
c*** 
        character *(*) function area_dif (text1, text2)
        character text1 *(*), text2 *(*)
c
c       Determine area changes between text1 and text2
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
c
        character bus1*8, bus2*8
        integer find_bus, field(2,13)

        data field /
     &      27, 34, 36, 37, 39, 40, 42, 43, 45, 46, 48, 49, 51, 52,
     &      54, 55, 57, 58, 60, 61, 63, 64, 73, 76, 77, 80 /

        area_dif = text1
        area_dif(2:3) = ' M'
        area_dif(14:) = ' '

        if (text1(4:13) .ne. text2(4:13)) then
           write (errbuf(1), 110) text1(1:18)
  110      format (' Areas modified do not match ', a)
           write (errbuf(2), 120) text2(1:18)
  120      format ('                             ', a)
           call prterx ('W', 2)
           go to 920
        endif

c        if (text1(2:2) .ne. text2(2:2)) then
c           write (errbuf(1), 130) text1(1:18)
c  130      format (' Illegal change of bus type ', a)
c           call prterx ('W', 1)
c        else
c           area_dif(2:2) = text2(2:2)
c        endif

        bus1 = text1(14:21)
        read (text1(22:25), '(bz, f4.0)', err=900) base1
        i1 = find_bus (bus1, base1)
        if (i1 .eq. 0) then
           write (errbuf(1), 140) text1(1:18)
  140      format (' Area slack bus is not in system ', a)
           call prterx ('W', 1)
           go to 900
        endif

        bus2 = text2(14:21)
        read (text2(22:25), '(bz, f4.0)', err=920) base2
        i2 = find_bus (bus2, base2)
        if (i2 .eq. 0) then
           write (errbuf(1), 140) text2(1:18)
           call prterx ('W', 1)
           go to 920
        endif
c
c       Perform table driven change modifications
c
        do i = 1, 13

           i1 = field(1,i)
           i2 = field(2,i)

           if (text1(i1:i2) .ne. text2(i1:i2)) then
              if (text2(i1:i2) .ne. ' ') then
                 area_dif(i1:i2) = text2(i1:i2)
              else
                 area_dif(i1:i2) = '000000000'
              endif
           endif
        enddo

        go to 940

  900   write (errbuf(1), 910) text1(1:80)                                  
  910   format (' Illegal data in text1 field : (',a80,')')                      
        call prterx ('W',1)                                               
        go to 940

  920   write (errbuf(1), 910) text2(1:80)                                  
  930   format (' Illegal data in text2 field : (',a80,')')                      
        call prterx ('W',1)                                               

  940   continue

        return
        end
