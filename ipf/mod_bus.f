C    @(#)mod_bus.f	20.4 2/13/96
        subroutine mod_bus (ic, ibus)
        integer ic, ibus
c
c       Perform bus modification changes from chgcrd(ic) into
c       busdta(*,ibus).
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
 
        common /is_batch / is_batch
c
        character bus2*8, subtype*1, oldsubtyp*1, fieldfmt(28)*10
        integer find_bus, fieldptr(4), field(4,28), type, oldtype

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

        data fieldfmt /
c
c          Types B , BE, BS, BC, BV, BQ, BG, BO, BT, BX, BJ, BK, BL
c                 1   2   3   4   6   7   8   9  10  11  12  13  14
c
     &      '(bz, f5.0)', '(bz, f5.0)', '(bz, f4.0)', '(bz, f4.0)', 
     &      '(bz, f4.0)', '(bz, f5.0)', '(bz, f5.0)', '(bz, f5.0)', 
     &      '(bz, f4.3)', '(bz, f4.3)', '(bz, f3.0)',
c
c          Types BD
c                 5
c
     &      '(bz, f2.0)', '(bz, f5.1)', '(bz, f5.1)', '(bz, f5.1)', 
     &      '(bz, f5.1)', '(bz, f5.1)',
c
c          Types BM
c                12
c
     &      '(bz, f2.0)', '(bz, f5.1)', '(bz, f5.1)', '(bz, f5.1)', 
     &      '(bz, f5.1)', '(bz, f5.1)', '(a1)  ',    '(bz, f3.1)', 
     &      '(bz, f3.1)', '(bz, f6.1)', '(bz, f5.1)' /
c
c       store old values for special bus types and vlimits
c
        qlold = busdta(4,ibus)
        bold = busdta(6,ibus)
        qgold = busdta(9,ibus)
        vmxold = busdta(11,ibus)
        vmnold = busdta(12,ibus)

        read (chgcrd(ic)(122:125), '(bz, i4)') nc
        oldtype = kbsdta(1,ibus)
        subtype = chgcrd(ic)(2:2)
        if (subtype .eq. '0') then
           subtype = ' '
           call typnam (subtype, newtype)
        else if (subtype .ne. ' ') then
           call typnam (subtype, newtype)
        else
           newtype = oldtype
           call typno (subtype, oldtype)
        endif
        if (index(' ESCVQGOTXFJKL', subtype) .ne. 0) then
           type = 1
        else if (subtype .eq. 'D') then
           type = 2
        else if (subtype .eq. 'M') then
           type = 3
        else
           write (errbuf(1),100) subtype
  100      format(' Illegal bus subtypee (', a, ')')
           write (errbuf(2),110) chgcrd(ic)(1:31), nc
  110      format(' Change record (', a, ') No. ', i5)
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
           go to 920
        endif

        if (oldtype .ne. newtype) then
           if (oldtype .eq. 5 .or. oldtype .eq. 12) then
              call typno (oldsubtyp, oldtype)
              write (errbuf(1),120) oldsubtyp, subtype
  120         format(' Illegal bus subtype change (', a, ') -> (',
     &            a, ')')
              write (errbuf(2),110) chgcrd(ic)(1:31), nc
              if (is_batch .eq. 0) then
                 call prterx ('E',2)
              else
                 call prterx ('F',2)
              endif
              go to 920
           else
              kbsdta(1,ibus) = newtype
           endif
        endif

        if (chgcrd(ic)(4:6) .ne. ' ') owner(ibus) = chgcrd(ic)(4:6)
        if (chgcrd(ic)(19:20) .ne. ' ') zone(ibus) = chgcrd(ic)(19:20)
c
c       Perform table driven change modifications
c
        do i = fieldptr(type), fieldptr(type+1)-1

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (chgcrd(ic)(i2:i3) .ne. ' ') then
              if (index(fieldfmt(i), 'f') .ne. 0) then
c
c                Floating point decoding
c
                 read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &              busdta(i4,ibus)
              else
c
c                Integer or character decoding
c  
                 read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &              kbsdta(i4,ibus)
              endif
           endif
        enddo
c                                                                       
c       Implement any remote or commutator bus changes
c                                                                       
        if (subtype .eq. 'G' .or. subtype .eq. 'X') then
           read (chgcrd(ic), 130, err=900) bus2, base2                
  130      format (bz, t66, a8, f4.0)                   
           if (bus2 .ne. ' ') then    
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 kbsdta(13,ibus) = ix
              else
                 write (errbuf(1), 140) bus2, base2
  140            format(' Remotely controlled bus (', a8, f6.1, 
     &              ') is not in system.')
                 write (errbuf(2),110) chgcrd(ic)(1:31), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',2)
                 else
                    call prterx ('F',2)
                 endif
              endif
           endif

        else if (subtype .eq. 'D' .or. subtype .eq. 'M') then
           read (chgcrd(ic), 150, err=900) bus2, base2                
  150      format (bz, t51, a8, f4.0)                   
           if (bus2 .ne. ' ') then 
              ix = find_bus (bus2, base2)
              if (ix .gt. 0) then
                 kbsdta(9,ibus) = ix
              else
                 write (errbuf(1), 160) bus2, base2
  160            format(' Commutator bus (', a8, f6.1, 
     &              ') is not in system.')
                 write (errbuf(2),110) chgcrd(ic)(1:31), nc
                 if (is_batch .eq. 0) then
                    call prterx ('E',2)
                 else
                    call prterx ('F',2)
                 endif
              endif
           endif
        endif
c
c       Cleanup voltage limits and other esoteric features of 
c       special bus types
c
c       1. Use global limits if type "BQ" bus is changed to type "BG"
c          and Voltage limits are blank
c
        if (oldtype .eq. 7 .and. newtype .eq. 8) then
           if (chgcrd(ic)(58:61) .eq. ' ') busdta(11,ibus) = 0.0
           if (chgcrd(ic)(62:65) .eq. ' ') busdta(12,ibus) = 0.0
        endif
C
C       For a-c busses, change VSTART if bus type or voltage fields
C       changed.
C
        if (oldtype .eq. 5 .or. oldtype .eq. 12) then
        else if ((oldtype .ne. newtype) .or.
     &      (busdta(11,ibus) .ne. vmxold .or.
     &       busdta(12,ibus) .ne. vmnold)) then

           vold = dsqrt (e(ibus) ** 2 + f(ibus) ** 2)
           call vltlim (ibus, vlimn(ibus), vlimx(ibus), vstrt)
           if (vstrt .ne. vstart(ibus) .or. oldtype .ne. newtype) then
              e(ibus) = e(ibus) * vstrt / vold
              f(ibus) = f(ibus) * vstrt / vold
              vstart(ibus) = vstrt
           endif
        endif
        go to 920

  900   write (errbuf(1), 910) chgcrd(ic)(1:80)                                  
  910   format (' Illegal data in field : (',a80,')')                      
        call prterx ('W',1)                                               
        chgcrd(ic)(126:126) = 'E'

  920   continue

        return
        end
