C    @(#)chk_cbsfld.f	20.1 2/13/96
        integer function chk_cbsfld (text, ptr, count, out_buffer)
        integer ptr, count
        character text*(*), out_buffer(10)*120
c
c       Check data fields for suspicious BZ entitites
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/cbus.inc'
        include 'ipfinc/prt.inc'
c
        character fieldfmt(7)*10, month*1
        integer field(4,7), new_count, status, chk_cbsdta

        data field /
     &      1, 21, 25,  2, 1, 26, 30,  3, 1, 31, 34,  4, 1, 35, 38, 5,
     &      1, 43, 47,  6, 1, 48, 52, 11, 1, 53, 57, 12 /

        data fieldfmt /
     &      '(bz, f5.0)', '(bz, f5.0)', '(bz, f4.0)', '(bz, f4.0)', 
     &      '(bz, f5.0)', '(bz, f5.0)', '(bz, f5.0)' /
c
c       Perform table driven change modifications
c
        chk_cbsfld = 0
        new_count = count + 1
        errbuf(count+1) = ' '
        do i = 1, 7

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
                    errbuf(count+1)(i2+1:i3+1) = '*********'
                    chk_cbsfld = 1
                 endif
              else
c
c                Integer or character decoding
c  
                 if (text(i3:i3) .eq. ' ') then
                    errbuf(count+1)(i2+1:i3+1) = '*********'
                    chk_cbsfld = 1
                 endif
              endif
              status = chk_cbsdta (ptr, i2, new_count, out_buffer)
           endif
        enddo
        if (chk_cbsfld .eq. 1) then
          count = new_count
        else if (new_count .gt. count+1) then
          do i = 1, new_count-1
            out_buffer(i) = out_buffer(i+1)
          enddo
          count = new_count - 1
        endif
        return
        end
