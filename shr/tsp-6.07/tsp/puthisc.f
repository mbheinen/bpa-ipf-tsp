C    %W% %G%
c     -                           puthisc.f

      subroutine puthisc (charr, kount)
c     -    Writes out kount char-quartets from the charr array to the 
c          buffer for da file unit L8 and writes a record if the array
c          fills up more than one record.
c     -    By ANSI standard, a character argument can't be passed down
c          to a float parameter, so each quad of _charr_ must be 
c          inividually equivalenced to a float.
c     -    Note 1: kount assumes a mapping of 4-chars per real number.
c          A good formula for kount would be:
c              (( num_strings * str_size ) + 3) / 4
c 
      character charr(*)*4
      integer kount
      character ch4*4
      real flt4
      equivalence (ch4,flt4)
c
      do la = 1,kount
        ch4 = charr(la)
        call puthisf (flt4, 1)
      enddo
      return
      end
c     -  end of puthisc

