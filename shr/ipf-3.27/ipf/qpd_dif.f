C    @(#)qpd_dif.f	20.3 2/13/96
        character *(*) function qpd_dif (text1, text2)
        character text1 *(*), text2 *(*)
c
c       Determine pq-curve data changes between text1 and text2
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
c
        character bus1*8, bus2*8
        integer find_bus, field(4,10)

        data field /
     &      1, 19, 24,  1,  1, 25, 30,  2, 1, 31, 36,  3,  1, 37, 42, 4,
     &      1, 43, 48,  5,  1, 49, 54,  6, 1, 55, 60,  7,  1, 61, 66, 8,
     &      1, 67, 72,  9,  1, 73, 78, 10 /

        qpd_dif = text1
        qpd_dif(3:3) = 'M'
        qpd_dif(4:6) = ' '
        qpd_dif(19:) = ' '

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
           write (errbuf(1), 110) text1(1:18)
  110      format (' Modified records do not match ', a)
           write (errbuf(2), 120) text2(1:18)
  120      format ('                               ', a)
           call prterx ('W', 2)
           go to 920
        endif
        
        if (text1(4:5) .ne. text2(4:5)) qpd_dif(4:5) = text2(4:5)
        if (text1(6:6) .ne. text2(6:6)) qpd_dif(6:6) = text2(6:6)
c
c       Perform table driven change modifications
c
        do i = 1, 10

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (text1(i2:i3) .ne. text2(i2:i3)) then
              if (text2(i2:i3) .ne. ' ') then
                 qpd_dif(i2:i3) = text2(i2:i3)
              else
                 qpd_dif(i2:i3) = '000000000'
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
