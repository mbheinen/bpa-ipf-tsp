C    @(#)xbus_dif.f	20.3 2/13/96
        character *(*) function xbus_dif (text1, text2)
        character text1 *(*), text2 *(*)
c
c       Determine switched reactance changes between text1 and text2
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'
c
        character bus1*8, bus2*8
        integer find_bus, field(4,16)

        data field /
     &      1, 33, 33,  7, 1, 34, 38,  8, 1, 39, 39,  9, 1, 40, 44, 10,
     &      1, 45, 45, 11, 1, 46, 50, 12, 1, 51, 51, 13, 1, 52, 56, 14,
     &      1, 57, 57, 15, 1, 58, 62, 16, 1, 63, 63, 17, 1, 64, 68, 18,
     &      1, 69, 69, 19, 1, 70, 74, 20, 1, 75, 75, 21, 1, 76, 80, 22 /


        xbus_dif = text1
        xbus_dif(3:3) = 'M'
        xbus_dif(4:6) = ' '
        xbus_dif(19:) = ' '

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
        
        if (text1(4:6) .ne. text2(4:6)) xbus_dif(4:6) = text2(4:6)
c                                                                       
c       Implement any remote or commutator bus changes
c                                                                       
        if (text1(21:32) .ne. text2(21:32)) then
           read (text2, 140, err=920) bus2, base2                
  140      format (bz, t21, a8, f4.0)                   
           if (bus2 .ne. ' ') then    
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 xbus_dif(21:32) = text2(21:32)
              else
                 write (errbuf(1), 150) text2(1:18), bus2, base2
  150            format(' Modification ', a, ' has a non-existant remote
     &ly controlled bus (', a8, f6.1)
                 call prterx ('W', 1)                       
              endif
           endif
        endif
c
c       Perform table driven change modifications
c
        do i = 1, 16

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (text1(i2:i3) .ne. text2(i2:i3)) then
              if (text2(i2:i3) .ne. ' ') then
                 xbus_dif(i2:i3) = text2(i2:i3)
              else
                 xbus_dif(i2:i3) = '000000000'
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
