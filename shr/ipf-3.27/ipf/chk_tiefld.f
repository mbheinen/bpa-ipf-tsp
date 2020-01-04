C    @(#)chk_tiefld.f	20.1 2/13/96
        integer function chk_tiefld (text, ptr, count, out_buffer)
        integer prt, count
        character text *(*), out_buffer(10)*120
c
c       Check area fields of TEXT for suspicious entitites
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/prt.inc'
 
        character fieldfmt(4)*10
        integer fieldptr(3), field(4,4), type, status, chk_tiedta

        data fieldptr / 1, 4, 5 /
        data field /
c
c          Types A, I
c                1  2
c
     &      1, 27, 34,  0, 1, 73, 76,  0, 1, 77, 80,  0, 1, 27, 34, 0 /

        data fieldfmt /
c
c          Types A , I
c                1   2
c
     &      '(bz, f8.0)', '(bz, f4.3)', '(bz, f4.3)', '(bz, f8.0)' /

        chk_tiefld = 0
        new_count = count + 1
        out_buffer(count+1) = ' '
        type = 2
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
                    chk_tiefld = 1
                 endif
              else
c
c                Integer or character decoding
c  
                 if (text(i3:i3) .eq. ' ') then
                    out_buffer(count+1)(i2+1:i3+1) = '*********'
                    chk_tiefld = 1
                 endif
              endif
              status = chk_tiedta (ptr, i2, new_count, out_buffer)
           endif
        enddo
        if (chk_tiefld .eq. 1) then
          count = new_count
        else if (new_count .gt. count+1) then
          do i = 1, new_count-1
            out_buffer(i) = out_buffer(i+1)
          enddo
          count = new_count - 1
        endif

  220   return
        end
