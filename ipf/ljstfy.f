C    @(#)ljstfy.f	20.3 2/13/96
      character * (*) function ljstfy (string)
      character string * (*)
C
      ljstfy = string
 
      if (ljstfy .ne. ' ') then
         i = len (ljstfy)
         do while (i .gt. 1 .and. (ljstfy(1:1) .eq. ' '))
            ljstfy  = ljstfy (2:)
            i = i - 1
         enddo
      endif
      return
      end
