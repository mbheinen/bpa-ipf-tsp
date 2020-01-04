C    @(#)postcmnt.f	20.6 2/13/96
      subroutine postcmnt(record)
      character record*(*)

      include 'ipfinc/postdta.inc'
      include 'ipfinc/pasprm.inc'

      character newrec*150, backslash*2

      backslash = '\\'

      lengmx = len(record)
      do length = lengmx, 15, -1
          if (record(length:length) .ne. ' ') go to 110
      enddo
      length = 15
  110 read (record, 111) xcord, ycord
  111 format(bz, 2x, 2f6.2)

**** check for "(" or ")" in a comment string
**** if found, precede by "\" escape character
      newlen = 0
      do 130 indlen = 15, length
      if (newlen .ge. 149) go to 130
      newlen = newlen + 1
c      if (record(indlen:indlen) .eq. '(' .or.
c     1    record(indlen:indlen) .eq. ')' .or.
c     2    record(indlen:indlen) .eq. backslash(1:1)) then
      if (index('()\\', record(indlen:indlen)) .ne. 0) then
          newrec(newlen:newlen) = backslash(1:1)
          newlen = newlen + 1
      endif
      newrec(newlen:newlen) = record(indlen:indlen)
  130 continue


************ debug stuff ********************* 
*     print 9911, lengmx, length
*9911 format('in postcmnt - lengmx, length :', 2i5) 
*     print 9921, record(15:length), xcord, ycord
*9921 format('in postcmnt :', '(', a, ')', 2(f7.1, ' cmtr '))
************ debug stuff ********************* 
      write (lpost, 141) newrec(1:newlen), xcord, ycord
  141 format('(', a, ')', 2(f7.1, ' cmtr '), ' CordCmnt')
      return
      end

