C    @(#)postucmt.f	20.6 2/13/96
      subroutine postucmt(record)
      character record*(*)

      include 'ipfinc/pasprm.inc'

      character newrec*150, backslash*2

      backslash = '\\'

      lengmx = len(record)
      do length = lengmx, 2, -1
          if (record(length:length) .ne. ' ') go to 110
      enddo
  110 continue


**** check for "(" or ")" in a comment string
**** if found, precede by "\" escape character
      newlen = 0
      do 130 indlen = 1, length
        if (newlen .ge. 149) go to 130
        newlen = newlen + 1
c        if (record(indlen:indlen) .eq. '(' .or.
c     1      record(indlen:indlen) .eq. ')' .or. 
c     2      record(indlen:indlen) .eq. backslash(1:1)) then
        if (index('()\\', record(indlen:indlen)) .ne. 0) then
            newrec(newlen:newlen) = backslash(1:1)
            newlen = newlen + 1
        endif
        newrec(newlen:newlen) = record(indlen:indlen)
  130 continue



      write (lpost, 141) newrec(1:newlen) 
  141 format('(', a, ')',  ' CordUcmt')
      return
      end 
