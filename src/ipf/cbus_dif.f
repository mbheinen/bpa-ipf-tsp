C    @(#)cbus_dif.f	20.3 2/13/96
        character *(*) function cbus_dif (text1, text2)
        character text1 *(*), text2 *(*)
c
c       Determine continuation bus changes between text1 and text2
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
c
        character bus1*8, bus2*8
        integer find_bus, field(4,7)

        data field /
     &      1, 21, 25,  2, 1, 26, 30,  3, 1, 31, 34,  4, 1, 35, 38, 5,
     &      1, 43, 47,  6, 1, 48, 52, 11, 1, 53, 57, 12 /

        cbus_dif = text1
        cbus_dif(3:3) = 'M'
        cbus_dif(4:6) = ' '
        cbus_dif(21:) = ' '

        bus1 = text1(7:14)
        read (text1(15:18), '(bz, f4.0)', err=900) base1
        i1 = find_bus (bus1, base1)
        if (i1 .eq. 0) then
           write (errbuf(1), 100) text1(1:18)
  100      format (' Modified bus is not in system ', a)
           call prterx ('W', 1)
           go to 900
        endif

        bus2 = text2(7:14)
        read (text2(15:18), '(bz, f4.0)', err=920) base2
        i2 = find_bus (bus2, base2)
        if (i2 .eq. 0) then
           write (errbuf(1), 100) text2(1:18)
           call prterx ('W', 1)
           go to 920
        endif

        if (i1 .ne. i2) then
           write (errbuf(1), 110) text1(1:20)
  110      format (' Modified records do not match ', a)
           write (errbuf(2), 120) text2(1:20)
  120      format ('                               ', a)
           call prterx ('W', 2)
           go to 920
        endif
        
        if (text1(2:2) .ne. text2(2:2)) then
           write (errbuf(1), 120) text1(1:20)
           write (errbuf(2), 120) text2(1:20)
           call prterx ('W', 1)
        else
           cbus_dif(2:2) = text2(2:2)
        endif

        if (text1(4:6) .ne. text2(4:6)) then
           write (errbuf(1), 120) text1(1:20)
           write (errbuf(2), 120) text2(1:20)
           call prterx ('W', 1)
        else
           cbus_dif(4:6) = text2(4:6)
        endif

        if (text1(19:20) .ne. text2(19:20)) then
           write (errbuf(1), 120) text1(1:20)
           write (errbuf(2), 120) text2(1:20)
           call prterx ('W', 1)
        else
           cbus_dif(19:20) = text2(19:20)
        endif
c
c       Perform table driven change modifications
c
        do i = 1, 7

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (text1(i2:i3) .ne. text2(i2:i3)) then
              if (text2(i2:i3) .ne. ' ') then
                 cbus_dif(i2:i3) = text2(i2:i3)
              else
                 cbus_dif(i2:i3) = '000000000'
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
