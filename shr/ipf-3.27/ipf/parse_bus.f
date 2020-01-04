C    @(#)parse_bus.f	20.3 2/13/96
	subroutine parse_bus (text, word, nwrd)
c
c       This subroutine parses a string in the following formats into
c
c       word(1)   word(2)  word(3):
c
c       bus_name, base_kv, buses_back
c       bus_name  base_kv  buses_back
c
        character text *(*), word(*) * (*), xtext * 40, string * 8
        integer nwrd, xwrd, type, get_type
        external get_type
        logical break

	xtext = text
        do i = 1, 3
           word(i) = ' '
        enddo
        nwrd = 3
c
c       parse XTEXT in reverse order
c
        xwrd = 3
        do while (xtext .ne. ' ')
           i = lastch(xtext)
           break = .false.
           do while (i .gt. 0 .and. .not. break) 
              if (index(' ,', xtext(i:i)) .ne. 0) then
                 break = .true.
              else if (i .eq. 1) then
                 break = .true.
              else
                 i = i - 1
              endif
           enddo
           if (break) then
              j = lastch(xtext)
              if (i .lt. j) then
                 string = xtext(i+1:j)
              else
                 xtext(i:i) = ' '
                 string = ' '
              endif
c
c             "get_type" returns the interpreted token type of "string"
c
c                        0 = null
c                        1 = integer
c                        2 = real
c                        3 = character string
c
              j = lastch(string)
              type = get_type(string(1:j))
              do while (string .ne. ' ')
                 if (xwrd .eq. 3) then
                    if (type .eq. 1) then
                       word(xwrd) = string
                       xwrd = xwrd - 1
                       string = ' '
                       xtext(i:) = ' '
                    else
                       xwrd = xwrd - 1
                    endif
                 else if (xwrd .eq. 2) then
                    if (type .eq. 1 .or. type .eq. 2) then
                       word(xwrd) = string
                       xwrd = xwrd - 1
                       string = ' '
                       xtext(i:) = ' '
                    else
                       if (word(xwrd) .eq. ' ') then
                          word(xwrd) = word(xwrd+1)
                          word(xwrd+1) = ' '
                       endif
                       xwrd = xwrd - 1
                    endif
                 else if (xwrd .eq. 1) then
                    word(xwrd) = xtext
                    if (word(xwrd+1) .eq. ' ') then
                       word(xwrd+1) = word(xwrd+2)
                       word(xwrd+2) = ' '
                    endif
                    xwrd = xwrd - 1
                    string = ' '
                    xtext(1:) = ' '
                 endif
              enddo
           else
              do j = 2, xwrd
                 word(j) = word(j+1)
              enddo
              word(xwrd) = ' '
              word(1) = xtext
              xtext = ' '
           endif
        enddo
        return
        end
