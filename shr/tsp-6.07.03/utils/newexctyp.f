      character*2 function newexctyp (exciter_type)
      character exciter_type*(*)

C     This character function returns the new WSCC exciter type (FA-FL)
c     corresponding to the old type (EL-EX).

      logical found
      character oldtype(12)*2, newtype(12)*2
      data oldtype / 'EM','EN','EO','EP','EQ','ER','ES','ET','EU',
     &   'EV','EW','EX' /
      data newtype / 'FA','FB','FC','FD','FE','FF','FG','FH','FJ',
     &   'FK','FL','FZ' /

      newexctyp = exciter_type
      found = .false.
      i = 1
      do while (i .le. 12  .and. .not. found)
         if (exciter_type .eq. oldtype(i)) then
            newexctyp = newtype(i)
            found = .true.
         else
            i = i + 1
         endif
      enddo
      return
      end
