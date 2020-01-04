C    @(#)chk_xdtfld.f	20.1 2/13/96
        integer function chk_xdtfld (text, ptr, count, out_buffer)
        integer ptr, count
        character text*(*), out_buffer(10)*120
c
c       Check xdata fields for suspicious entitites
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/xdata.inc'
        include 'ipfinc/prt.inc'
c
        character fieldfmt(16)*10
        integer field(4,16), itemp, status, chk_xdtdta

        data field /
     &      1, 33, 33,  7, 1, 34, 38,  8, 1, 39, 39,  9, 1, 40, 44, 10,
     &      1, 45, 45, 11, 1, 46, 50, 12, 1, 51, 51, 13, 1, 52, 56, 14,
     &      1, 57, 57, 15, 1, 58, 62, 16, 1, 63, 63, 17, 1, 64, 68, 18,
     &      1, 69, 69, 19, 1, 70, 74, 20, 1, 75, 75, 21, 1, 76, 80, 22 /

        data fieldfmt /
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)', 
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)', 
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)',
     &      '(bz, f1.0)', '(bz, f5.0)', '(bz, f1.0)', '(bz, f5.0)' /
c
c       Perform table driven change modifications
c
        chk_xdtfld = 0
        new_count = count + 1
        out_buffer(count+1) = ' '
        do i = 1, 12

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
                    chk_xdtfld = 1
                 endif
              else
c
c                Integer or character decoding
c  
                 if (text(i3:i3) .eq. ' ') then
                    out_buffer(count+1)(i2+1:i3+1) = '*********'
                    chk_xdtfld = 1
                 endif
              endif
              status = chk_xdtdta (ptr, i2, new_count, out_buffer)
           endif
        enddo
        if (chk_xdtfld .eq. 1) then
          count = new_count
        else if (new_count .gt. count+1) then
          do i = 1, new_count-1
            out_buffer(i) = out_buffer(i+1)
          enddo
          count = new_count - 1
        endif
        return
        end
