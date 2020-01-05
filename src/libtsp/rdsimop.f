C    %W% %G%
      subroutine rdsimop 
c
c     -  Reads the cards in the /CASES, /FILES, /OPERATION, /SIMOPTS
c        and /GLOBDAT sections of the control file in new format.
c
c     -  by D McNulty - Apr/24/92
c
      include 'tspinc/params.inc'
      include 'tspinc/titles.inc'

c     -  case name is SWCASE (not SWCAS or SCASE)
c     -  add AUXFRM (aux file format) to /PLOTLC/
c     -  add ITEMNM (bus or line name) to /PLOTLC/  
c     -  add DOPLOT to /PLOTLC/

      include 'tspinc/comn56.inc'

c     -  add DOSIM (do simulatn char flag) to /FLTOPT/

      include 'tspinc/fltopt.inc'

c     -  add INPFRM (gen data format) to /FFCARD/

      include 'tspinc/ffcard.inc'

c     -  add CTLFMT (control input format) to /RREADC/

      include 'tspinc/reread.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/amorts.inc'
      include 'tspinc/files.inc'

c     -  Local variables

      logical go_on,debug
      character ch1*1, chsect*16, sectn*2
      character ch8*8, ch10*10, ch16*16, ch20*20, ch60*60, ch80*80

      logical sect_ok, fmt_ok, any_sim

c     -     Begin     Begin     Begin     Begin     Begin     Begin 

      debug = .false.
      go_on = .true.
      any_sim = .false.
      sectn = '  '

c     -  Set defaults for missing cards

      swcase = ' '
      pfcase = ' '
      dosim = 'Y'
      doplot = 'Y'
      itskp = 0
      noprnt = 0
      iwscc = 0
      savin = ' '
      savout = ' '

c     -  Now at beginning of the control file

      do while (go_on)
        sect_ok = .false.
        call readin 
        call nxtwrd (buffer,0,kb,ke)
        if (kb .eq. 0) goto 421
        if (buffer(kb:kb) .eq. '!')  goto 421
        chsect = buffer(kb:ke)
c
        if (chsect .eq. '/CASES') then 
          sect_ok = .true.
          go_on = .true.
          do while (go_on)
            call readin
            call nxtwrd (buffer,0,k1b,k1e)
            if (k1b .eq. 0) goto 171
            if (debug) then 
              call dbgeko ('dbg - RDSIMOP - /CASES section')
              call dbgwrc ('  input line: ',buffer)
              call dbgwri ('  K1B = ',k1b)
              call dbgwri ('  K1E = ',k1e)
              call dbgwrc ('  parameter: ',buffer(k1b:k1e))
            endif
            if (buffer(k1b:k1b) .eq. '!') goto 171
            ch10 = buffer(k1b:k1e)
            if (ch10 .eq. '/END') goto 172
            if (ch10 .eq. 'STAB') then
              call nxtwrd (buffer,k1e,k2b,k2e)
              if (k2b .eq. 0) then 
                ch80 = 'RDSIMOP [f] next line has no stab case.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
              endif
              swcase = buffer(k2b:k2e)
              if (solfl .eq. ' ') then
                solfl   = swcase // '.sol'
                pltfl   = swcase // '.pdf'
                prtfl   = swcase // '.out'
                auxfl   = swcase // '.swx'
              endif
              goto 171
            endif
            if (ch10 .eq. 'PF') then
              call nxtwrd (buffer,k1e,k2b,k2e)
              if (k2b .eq. 0) then 
                ch80 = 'RDSIMOP [f] next line has no powerflow case.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
              endif
              pfcase = buffer(k2b:k2e)
c             IF (BSEFL .EQ. ' ') BSEFL = PFCASE // '.bse'
              goto 171
            endif
            if (ch10 .eq. 'SDIIN') then
              call nxtwrd (buffer,k1e,k2b,k2e)
              if (k2b .eq. 0) then 
                ch80 = 'RDSIMOP [f] next line has no saved data name.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
              endif
              if (buffer(k2b:k2e) .eq. 'NONE') goto 171
              savin = buffer(k2b:k2e)
              if (savifl .eq. ' ') savifl  = savin // '.sdi'
              goto 171
            endif
            if (ch10 .eq. 'SDIOUT') then
              call nxtwrd (buffer,k1e,k2b,k2e)
              if (k2b .eq. 0) then 
                ch80 = 'RDSIMOP [f] next line has no saved data name.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
              endif
              if (buffer(k2b:k2e) .eq. 'NONE') goto 171
              savout = buffer(k2b:k2e)
              if (savofl .eq. ' ') savofl  = savout // '.sdo'
              goto 171
            endif

c           -  Here if unknown /CASES parm

            ch80 = 'RDSIMOP [w] next line has no case parameter.'
            call puterr (1,ch80)
            call puterr (2,buffer)
            call prterr ('E',2)
 171        continue
          enddo
 172      call target

c         -  Check that required inputs were given

          if (swcase .eq. ' ') then
            ch80 = 'RDSIMOP [f] No stab case given.'
            call puterr (1,ch80)
            call puterr (2,buffer)
            call prterr ('E',2)
            call erexit 
          endif
          if (pfcase .eq. ' ') then
            ch80 = 'RDSIMOP [f] No powerflow case given.'
            call puterr (1,ch80)
            call puterr (2,buffer)
            call prterr ('E',2)
            call erexit 
          endif
          goto 421
        endif  

        if (chsect .eq. '/OPERATION') then
          sect_ok = .true.
          do la = 1,2
            call nxtwrd (buffer,ke,kb,ke)
            if (k1b .ne. 0) then
              if (buffer(kb:ke) .eq. 'PLOT') doplot = 'Y'
              if (buffer(kb:ke) .eq. 'NOPLOT') doplot = 'N'
              if (buffer(kb:ke) .eq. 'SIM') dosim = 'Y'
              if (buffer(kb:ke) .eq. 'NOSIM') dosim = 'N'
            endif 
          enddo
          goto 421
        endif

        if (chsect .eq. '/SIMOPTS') then
          sect_ok = .true.
          go_on = .true. 
          any_sim = .true. 
          do while (go_on)
            call readin
            call nxtwrd (buffer,0,k1b,k1e)
            if (k1b .eq. 0) goto 271
            if (buffer(k1b:k1b) .eq. '!') goto 271
            ch10 = buffer(k1b:k1e)
            if (ch10 .eq. '/END') goto 272
            if (dosim .eq. 'N') goto 271
            call nxtwrd (buffer,k1e,k2b,k2e)
            if (k2b .eq. 0) then
              ch80 = 'RDSIMOP [w] next line has no value for '//
     +          'simulation option.  Will use default.'
              call puterr (1,ch80)
              call puterr (2,buffer)
              call prterr ('W',2)
              goto 271
            endif
            ch20 = buffer(k2b:k2e)
            if     (ch10 .eq. 'COMPLINES') then
              if (ch20 .eq. 'NORELOC') lcomp = 1
            elseif (ch10 .eq. 'FAULTBUS') then 
              if (ch20 .eq. 'RELOC') ifltsw = 1
            elseif (ch10 .eq. 'NETSOL') then 
              if (ch20 .eq. 'NEWTON') inewts = 2
            elseif (ch10 .eq. 'SKIPSTEP') then
              fmt_ok = .false.
              read (ch20,*,err=254) itskp
              fmt_ok = .true.
 254          if (.not. fmt_ok) then 
                ch80 = 'RDSIMOP [w] next line has invalid value of '//
     +            ' skipping count.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('W',2)
              endif
            elseif (ch10 .eq. 'PRINT') then
              if (ch20 .eq. 'FULL') noprnt = 1
            elseif (ch10 .eq. 'UFREQFMT') then
              if (ch20 .eq. 'OLD') iwscc = 1
            else 
              ch80 = 'RDSIMOP [w] next line has unknown simulation'//
     +          ' option.'
              call puterr (1,ch80)
              call puterr (2,buffer)
              call prterr ('W',2)
            endif
 271        continue
          enddo
 272      call target
          goto 421
        endif
c        
        if (chsect .eq. '/GLOBDAT') then
          sect_ok = .true.
          go_on = .true. 
          do while (go_on)
            call readin
            call nxtwrd (buffer,0,k1b,k1e)
            call nxtwrd (buffer,k1e,k2b,k2e)
            call nxtwrd (buffer,k2e,k3b,k3e)
            if (buffer(k1b:k1e) .eq. '/END') goto 302
            if (k1b .eq. 0) goto 301
            if (buffer(k1b:k1b) .eq. '!') goto 301
            ch10 = buffer(k1b:k1e)
            if (k2b .eq. 0) then 
              ch80 = 'RDSIMOP [w] next line has too few parameters '//
     +          'for global data.'
              call puterr (1,ch80)
              call puterr (2,buffer)
              call prterr ('W',2)
              goto 301
            endif
            ch16 = buffer(k2b:k2e)
            if (ch10 .eq. 'DFLTDAMP') then
              if (k3b .eq. 0) then
                ch80 = 'RDSIMOP [f] next line has no value for '//
     +            'default damping parameter.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
                goto 301
              endif
              ch20 = buffer(k3b:k3e)
              fmt_ok = .false.
              read (ch20,*,err=283) value 
              fmt_ok = .true.
 283          if (.not. fmt_ok) then 
                ch80 = 'RDSIMOP [f] next line has invalid value '//
     +            'for default damping parameter.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
                goto 301
              endif
              if     (ch16 .eq. 'X"FACT') then
                xfact = value
              elseif (ch16 .eq. 'T"DORND') then
                tdodps = value
              elseif (ch16 .eq. 'T"QORND') then
                tqodps = value
              elseif (ch16 .eq. 'T"DOSAL') then
                tdodph = value
              elseif (ch16 .eq. 'T"QOSAL') then
                tqodph = value
              else 
                ch80 = 'RDSIMOP [w] next line has unknown parameter '//
     +            'for default damping.' 
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('W',2)
              endif
              goto 301
            elseif (ch10 .eq. 'BASEFREQ') then
              if (k2b .eq. 0) then
                ch80 = 'RDSIMOP [f] next line has no value for base '//
     +            'frequency.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
                goto 301
              endif
              ch20 = buffer(k2b:k2e)
              fmt_ok = .false.
              read (ch20,*,err=295) value 
              fmt_ok = .true.
 295          if (.not. fmt_ok) then 
                ch80 = 'RDSIMOP [f] next line has invalid value '//
     +            'for base frequency.'
                call puterr (1,ch80)
                call puterr (2,buffer)
                call prterr ('E',2)
                goto 301
              endif
              frqbse = value 
            else 
              ch80 = 'RDSIMOP [w] Next line has unknown parameter '//
     +          'for global data.'
              call puterr (1,ch80)
              call puterr (2,buffer)
              call prterr ('W',2)
              goto 301
            endif
 301        continue
          enddo
 302      call target
          goto 421
        endif 

        if (chsect .eq. '/SIMDAT')  then
          sect_ok = .true.
          if (dosim .eq. 'Y') return
c         -  skip all data cards if no simulation to be done
          go_on = .true. 
          do while (go_on)
            call readin
            call nxtwrd (buffer,0,k1b,k1e)
            if (k1b .eq. 0) goto 331
            if (buffer(k1b:k1b) .eq. '!') goto 331
            ch10 = buffer(k1b:k1e)
            if (ch10 .eq. '/END') goto 332
 331        continue
          enddo
 332      call target
          goto 421
        endif

        if (chsect .eq. '/PLOTOPS') then
          sect_ok = .true.
          return
        endif

        if (chsect .eq. '/PLOTDAT') then
          sect_ok = .true.
          return
        endif

        if (.not. sect_ok) then
          ch80 = 'RDSIMOP [f] next line has unknown control section.'
          call puterr (1,ch80)
          call puterr (2,buffer)
          call prterr ('E',2)
        endif

 421    continue
      enddo
 422  call target

c     -  Routine ends with buffer containing one of:
c          /SIMDAT, /PLOTOPS, /PLOTDAT

      return
      end
