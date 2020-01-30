C    @(#)bus_dif.f	20.3 2/13/96
        character *(*) function bus_dif (text1, text2)
        character text1 *(*), text2 *(*)
c
c       Determine bus changes between text1 and text2
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
c	Global variables used:
c		None
        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
c
        character bus1*8, bus2*8
        integer find_bus, fieldptr(4), field(4,28), type

        data fieldptr / 1, 12, 18, 29 /
        data field /
c
c          Types B , BE, BS, BC, BV, BQ, BG, BO, BT, BX, BJ, BK, BL
c                 1   2   3   4   6   7   8   9  10  11  12  13  14
c
     &      1, 21, 25,  3, 1, 26, 30,  4, 1, 31, 34,  5, 1, 35, 38, 6,
     &      1, 39, 42,  7, 1, 43, 47,  8, 1, 48, 52,  9, 1, 53, 57, 10,
     &      1, 58, 61, 11, 1, 62, 65, 12, 2, 78, 80, 14,
c
c          Type BD
c                5
c
     &      1, 24, 25,  3, 1, 26, 30,  4, 1, 31, 35,  5, 1, 36, 40, 6,
     &      1, 41, 45,  7, 1, 46, 50,  8,
c
c          Type BM
c               12
c
     &      1, 24, 25,  3, 1, 26, 30,  4, 1, 31, 35,  5, 1, 36, 40,  6,
     &      1, 41, 45,  7, 1, 46, 50,  8, 2, 63, 63, 12, 1, 64, 66, 10,
     &      1, 67, 69, 11, 1, 70, 75, 13, 1, 76, 80, 14  /

        bus_dif = text1
        bus_dif(3:3) = 'M'
        bus_dif(4:6) = ' '
        bus_dif(19:) = ' '

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
           write (errbuf(1), 120) text2(1:18)
  120      format ('                               ', a)
           call prterx ('W', 2)
           go to 920
        endif
        
        if (text1(2:2) .ne. text2(2:2)) then
           if (index (' ESCDVQGOTXMF', text2(2:2)) .eq. 0) then
              write (errbuf(1), 120) text1(1:18)
  130         format (' Illegal change of bus type ', a)
              call prterx ('W', 1)
           else if (text2(2:2) .ne. ' ') then
              bus_dif(2:2) = text2(2:2)
           else 
              bus_dif(2:2) = '0'
           endif
        endif

        if (text1(4:6) .ne. text2(4:6)) bus_dif(4:6) = text2(4:6)

        if (text1(19:20) .ne. text2(19:20)) 
     &     bus_dif(19:20) = text2(19:20)
c
c       Perform table driven change modifications
c
        if (text2(2:2) .eq. 'D') then
           type = 2
        else if (text2(2:2) .eq. 'M') then
           type = 3
        else
           type = 1
        endif

        do i = fieldptr(type), fieldptr(type+1)-1

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (text1(i2:i3) .ne. text2(i2:i3)) then
              if (text2(i2:i3) .ne. ' ') then
                 bus_dif(i2:i3) = text2(i2:i3)
              else
                 bus_dif(i2:i3) = '000000000'
              endif
           endif
        enddo
c                                                                       
c       Implement any remote or commutator bus changes
c                                                                       
        if ((text2(2:2) .eq. 'G' .or. text2(2:2) .eq. 'X') .and.
     &       text1(66:77) .ne. text2(66:77)) then
           read (text2, 140, err=920) bus2, base2                
  140      format (bz, t66, a8, f4.0)                   
           if (bus2 .ne. ' ') then    
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 bus_dif(66:77) = text2(66:77)
              else
                 write (errbuf(1), 150) text2(1:18), bus2, base2
  150            format(' Modification ', a, ' has a non-existant remote
     &ly controlled bus (', a8, f6.1)
                 call prterx ('W', 1)                       
              endif
           endif

        else if ((text2(2:2) .eq. 'D' .or. text2(2:2) .eq. 'M') .and.
     &            text1(66:77) .ne. text2(66:77)) then
           read (text2, 160, err=920) bus2, base2                
  160      format (bz, t66, a8, f4.0)                   
           if (bus2 .ne. ' ') then 
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 bus_dif(66:77) = text2(66:77)
              else
                 write (errbuf(1), 170) bus2, base2
  170            format(' Modification ', a, ' has non-existant commutat
     &or bus (', a8, f6.1)
                 call prterx ('W', 1)                       
              endif
           endif
        endif

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
