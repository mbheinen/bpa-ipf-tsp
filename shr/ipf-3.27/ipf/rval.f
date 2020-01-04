C    @(#)rval.f	20.3 2/13/96
        function rval (word)
 
C       THIS SUBROUTINE DECODES WORD INTO A FLOATING POINT NUMBER.
 
C       IF WORD CONTAINS ILLEGAL CHARACTER INFORMATION, ITS VALUE
C       IS SET TO ZERO.
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/prt.inc'
 
        character word *(*),wordsv*20
 
        wordsv=word
 
        if (ichar(word(1:1)) .eq. 0) goto 85
 
C       L = LEN (WORDSV)
        l = 20
 
C       RIGHT-JUSTIFY WORD
 
        do 80 i = l,1,-1
           if (wordsv(i:i).ne.' ') go to 90
   80   continue
 
   85   rval = 0.0
        go to 900
 
   90   if (i.lt.l) then
           i = l - i
           wordsv(1+i:) = wordsv
           wordsv(1:i) = ' '
        endif
 
        j = i + 1
 
        do 100 i = j,l
           if (index ('-+E .1234567890',wordsv(i:i)).eq.0) go to 120
  100   continue
 
C       TEST FOR EXPONENTIAL FORMAT
 
        if (index (wordsv,'E').ne.0) then
C                                       RIGHT-JUSTIFY "E"
           i = l - index(wordsv,'E') - 3
           if (i.ne.0) then
              wordsv(1+i:) = wordsv
              wordsv(1:i) = ' '
           endif
        endif
 
        read (wordsv,110) x
C 110   FORMAT (F<L>.0)
  110   format (f20.0)
        rval = x
        go to 900
 
C                               ERROR   ERROR   ERROR
 
  120   write (errbuf(1),130) wordsv
  130   format('0 ILLEGAL ALPHA INFORMATION IN NUMERIC FIELD:(',a20,')')
        call prterx ('W',1)
        rval = 0.0
 
  900   return
        end
