C    @(#)get_type.f	20.3 2/13/96
	integer function get_type (string)
c
c       function "get_type" returns the interpreted token type 
c       of "string"
c
c              0 = null
c              1 = integer
c              2 = real
c              3 = character string
c
	character string *(*)

        get_type = 0
        do i = 1, len(string)
           if (ichar(string(i:i)) .ge. ichar('0') .and.
     1         ichar(string(i:i)) .le. ichar('9')) then
              if (get_type .eq. 0) get_type = 1
           else if (string(i:i) .eq. '.') then
              if (get_type .eq. 1) get_type = 2
           else if (ichar(string(i:i)) .ge. ichar('A') .and.
     1              ichar(string(i:i)) .le. ichar('Z')) then
              get_type = 3
           else if (ichar(string(i:i)) .ge. ichar('a') .and.
     1              ichar(string(i:i)) .le. ichar('z')) then
              get_type = 3
        
           endif
        enddo
        return
        end
