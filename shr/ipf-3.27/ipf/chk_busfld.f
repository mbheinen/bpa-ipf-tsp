C    @(#)chk_busfld.f	20.1 2/13/96
        integer function chk_busfld (text, ptr, count, out_buffer)
        integer ptr, count
        character text *(*), out_buffer(10)*120
c
c       Check data fields for suspicious BZ entitites
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/prt.inc'
 
        character fieldfmt(28)*10
        integer find_bus, fieldptr(4), field(4,28), type, new_count,
     &          status, chk_busdta

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

        chk_busfld = 0
        new_count = count + 1
        out_buffer(count+1) = ' '
        if (text(1:2) .eq. 'BD') then
           type = 2
        else if (text(1:2) .eq. 'BM') then
           type = 3
        else
           type = 1
        endif
c
c       Perform table driven change modifications
c
        do i = fieldptr(type), fieldptr(type+1)-1

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (text(i2:i3) .ne. ' ') then
              if (index(fieldfmt(i), 'f') .ne. 0) then
c
c                Floating point field
c
                 if (text(i3:i3) .eq. ' ' .and.
     &               index(text(i2:i3), '.') .eq. 0) then
                    out_buffer(count+1)(i2+1:i3+1) = '*********'
                    chk_busfld = 1
                 endif
              else
c
c                Integer or character decoding
c  
                 if (text(i3:i3) .eq. ' ') then
                    out_buffer(count+1)(i2+1:i3+1) = '*********'
                    chk_busfld = 1
                 endif
              endif
              status = chk_busdta (ptr, i2, new_count, out_buffer)
           endif
        enddo

        if (chk_busfld .eq. 1) then
          count = new_count
        else if (new_count .gt. count+1) then
          do i = 1, new_count-1
            out_buffer(i) = out_buffer(i+1)
          enddo
          count = new_count - 1
        endif
        return
        end
