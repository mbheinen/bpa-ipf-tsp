C    %W% %G%
      subroutine rdpltop 
c
c     -  Reads the cards in the /PLTOPS and /PLODAT sections of the 
c        control file in new format.
c
c     -  by D McNulty - Apr/24/92
c
      include 'tspinc/params.inc'
      include 'tspinc/titles.inc'
c     -  case name is SWCASE (not SWCAS or SCASE)
c     -  add AUXFMT (aux file format) to /LINK56/
c     -  add ITEMNM (bus or line name) to /PLOTLC/  
      include 'tspinc/link56.inc'
c     -  add DOPLOT to /PLOTLC/
      include 'tspinc/comn56.inc'
c     -  add DOSIM (do simulatn char flag) to /FLTOPT/
      include 'tspinc/fltopt.inc'
c     -  add CTLFMT (control input format) to /RREADC/
      include 'tspinc/reread.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/files.inc'
c     -  Local variables
      integer kwbeg, kwend
      logical go_on
      character ch1*1, chsect*16, sectn*2
      character ch8*8, ch10*10, ch16*16, ch20*20, ch60*60, ch80*80
      logical sect_ok, fmt_ok
c     -     Begin     Begin     Begin     Begin     Begin     Begin 
      go_on = .true.
      sectn = '  '
c     -  Set defaults for missing cards
      auxfmt = 'STD'
c     -  Now at /PLOTOPS or /PLOTDAT card
      call nxtwrd (buffer,0,kb,ke)
      chsect = buffer(kb:ke)
      if (chsect .eq. '/PLOTDAT') return 
      sect_ok = .false.
c 
      do while (go_on)
        call nxtwrd (buffer,0,kb,ke)
        if (kb .eq. 0) goto 421
        if (buffer(kb:kb) .eq. '!')  goto 421
        chsect = buffer(kb:ke)
c
        if (chsect .eq. '/PLOTOPS') then 
          sect_ok = .true.
          go_on = .true. 
          do while (go_on)
            call readin
            call nxtwrd (buffer,0,k1b,k1e)
            if (k1b .eq. 0) then 
              ch80 = 'RDPLTOP [w] Next line has unknown plot parameter.'
              call puterr (1,ch80)
              call puterr (2,buffer)
              call prterr ('W',2)
              goto 235
            endif
            ch10 = buffer(k1b:k1e)
            if (ch10 .eq. '/END') goto 421
            if (ch10 .eq. 'AUXFMT') then
              call nxtwrd (buffer,k1e,k2b,k2e)
              if (k2b .eq. 0) then
                ch80 = 'RDPLTOP [w] Next line has no option for plot'//
     +            ' parameter.  Default will be used.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('W',2)
                goto 235
              endif
              ch16 = buffer(k2b:k2e)
              if (ch16 .eq. 'POSTPLOT') auxfmt = 'PST'
            endif
 235        continue
          enddo
        endif
c
        if (chsect .eq. '/PLOTDAT') then 
          sect_ok = .true.
          return
        endif
c 
        if (.not. sect_ok) then
          ch80 = 'RDPLTOP [f] next line has unknown plot section.'
          call puterr (1,ch80)
          call puterr (2,buffer)
          call prterr ('E',2)
        endif
c       -  Read next card
 421    call target
        sect_ok = .false.
        call readin 
 422    continue
      enddo
c     -  Routine ends with buffer containing /PLOTDAT or at end of
c        control file.
      return
      end
