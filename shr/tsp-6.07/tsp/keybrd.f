C    %W% %G%
      function keybrd(key)
C     
C     This function returns a value of zero or one. If the value is 
c     one, the debug switch for KEY is on. Entry point KRDDBG(KEY) 
c     sets the value of each debug switch as defined on the debug 
c     card. KRDDBG is called by SWINGM.
C     
      include 'tspinc/reread.inc'
      include 'tspinc/param.inc'

      dimension tsnap(6), keywrd(36), keyx(36)

      save

      data tsnap / 6 * 0.0 /
      data keywrd  / 36 * 0 /

C     -     begin     begin     begin     begin     begin     begin

      keybrd = keywrd(key+1)
      if (keybrd .ne. 0) then
        if (to .gt. 0.01) then
          do i = 1, 6, 2
            if ((to .ge. tsnap(i)) .and. (to .le. tsnap(i+1))) goto 100
          enddo
          keybrd = 0
        endif
      endif
      goto 100
      entry krddbg(keyx)
      do i = 1, 36
        keywrd(i) = keyx(i)
      enddo
      read (buffer, 10000) (tsnap(i), i = 1, 6)
10000 format (bz, 50x, 6f5.1)
      nbl = 0
      to = 0.0
      do i = 1, 6
        if (tsnap(i) .gt. 0.0) nbl = 1
      enddo
      if (nbl .eq. 0) tsnap(2) = 9999.0
      keybrd = 0
  100 return
      end
