	character *(*) function icode (value, length)
        integer value, length

        character buf * 20, fmt * 6

        write (fmt, 100) length
  100   format ('(i', i1, ')')

        write (buf(1:length), fmt=fmt) value

        i = 1
        do while (i .le. length .and. buf(i:i) .eq. ' ')
           buf(i:i) = '0'
          i = i + 1
        enddo

        icode = buf
        return
        end
        
