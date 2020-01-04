C    %W% %G%
        subroutine prodat
C       PROCESS THE SYSTEM DATA....

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/pfstates.inc'

        common /is_batch / is_batch

        character zonex(100)*2
        logical bldtbl, finished
        integer crtswx, fltstr, error, reordr, status, savewscc, 
     &          open_file, findstr, buildzbo, chkerr

        crtswx = crtsw
        if (jobreq(1).eq.'OUTAGE_SIM') go to 800
 
C       SYSTEM-REDUCTION
C           (0,1)
 
        fltstr = -1
        if( jobreq(1).eq.'REDUCTION' ) then
           crtsw = 0
           call reduct
           call prtime( 'REDUCTION' )
           inrcd = buf
c
c	   Note: "findstr" is a user-written, case-insensitive version 
C                of "index".
c
           if (findstr ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
           buf = inrcd
           kspare(1) = 1
           call bsread
           call cmpltbus(error)
           call prtime('BUS_READ')
           call sortbus
           call sortcbus
           call prtime('BUS_SORT')
           call brread
           call prtime('BRANCH_READ_SORT')
C
C          Select voltage flat start by REDUCT option
C
C          > STARTING_VOLTAGES = HOT !  (KASE1(35) = 1)
C                                FLAT ! (KASE1(35) = 0)
C
           fltstr = 1
           if (kase1(35) .eq. 1) fltstr = 0
           call btable (fltstr, .false.)
           call prtime('BUILD_TABLE')
           crtsw = crtswx
           kspare(1) = 2
C
C          Process any EPRI_1964 data.
C
           if (nepbus + nepctl .gt. 0) then
              call lodepr(zonex, numznx)
              call prtime('BUILD_EPRI_1964_TABLES')
           else
              numznx = 0
           endif
C
C          Zones declared in d-c are trasnferred to BLDTIE.
C
           call bldtie(zonex, numznx)
           call prtime('BUILD_AREA_ZONE_TABLES')
 
        endif
 
C       MODIFY_BASE_SYSTEM
C             (1)
C
        bldtbl = .false.
        finished = .false.
        do while (.not. finished)
 
        if (findstr(buf,'CHANGE_BUS') .ne. 0 .or.
     1      findstr(buf,'CHANGEBUS') .ne. 0) then
           crtswx = crtsw
           crtsw = 0
           if (bldtbl) then
C
C             BTABLE must be called prior to HOTCHG since
C             system changes have been made and all array must
C             be updated.
C
              fltstr = 0
              call btable (fltstr, .false.)
              call prtime('BUILD_TABLE')
C
C             Process any EPRI_1964 data.
C
              if (nepbus + nepctl .gt. 0) then
                 call lodepr(zonex, numznx)
                 call prtime('BUILD_EPRI_1964_TABLES')
              else
                 numznx = 0
              endif
C
C             Zones declared in d-c are trasnferred to BLDTIE.
C
              call bldtie(zonex, numznx)
              call prtime('BUILD_AREA_ZONE_TABLES')
              bldtbl = .false.
 
           endif
           call hotchg
           call prtime('CHANGE_BUS_TYPE')
C
C          Set "REBUILD" switch on to force BTABLE.
C          Set "FLTSTR" switch off if unassigned. FLTSTR = 0
C          preserves voltages for hot start.
C
           bldtbl = .true.
           if (fltstr .eq. -1) fltstr = 0
           kspare(1) = 1
           crtsw = crtswx
           inrcd = buf
           if (findstr ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
           buf = inrcd
 
        else if( findstr(buf,'CHANGES ') .ne. 0  .or. 
     &           findstr(buf,'CHANGES,') .ne. 0 )      then
 
           call change
           call prtime('CHANGE')
           bldtbl = .true.
           inrcd = buf
           kerr = chkerr('F')
           if (kerr .gt. 0) then
             write (errbuf(1), 80)
   80        format(' Case aborted by fatal errors.')
             if (is_batch .eq. 0) then
               call prterx ('E',1)
               return
             else
               call prterx ('F',1)
               call erexit()
             endif
          endif
 
        else if (findstr(buf, 'AGC') .ne. 0) then
 
           if (bldtbl) then
C
C             BTABLE must be called prior to GETAGC since
C             system changes have been made and all arrays must
C             be updated.
C
              fltstr = 0
              call btable (fltstr, .false.)
              call prtime('BUILD_TABLE')
 
C
C             Process any EPRI_1964 data.
C
              if (nepbus + nepctl .gt. 0) then
                 call lodepr(zonex, numznx)
                 call prtime('BUILD_EPRI_1964_TABLES')
              else
                 numznx = 0
              endif
C
C             Zones declared in d-c are trasnferred to BLDTIE.
C
              call bldtie(zonex, numznx)
              call prtime('BUILD_AREA_ZONE_TABLES')
 
              bldtbl = .false.
           endif
 
           call getagc(error)
           call prtime('AGC')
           inrcd = buf
           if (findstr ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
           buf = inrcd
 
        else if (findstr(buf,'GEN_DROP') .ne. 0 .or.
     1           findstr(buf,'GENDROP') .ne. 0) then
 
           if (bldtbl) then
C
C             BTABLE must be called prior to GENDROP since
C             system changes have been made and all array must
C             be updated.
C
              fltstr = 0
              call btable (fltstr, .false.)
              call prtime('BUILD_TABLE')
C
C             Process any EPRI_1964 data.
C
              if (nepbus + nepctl .gt. 0) then
                 call lodepr(zonex, numznx)
                 call prtime('BUILD_EPRI_1964_TABLES')
              else
                 numznx = 0
              endif
C
C             Zones declared in d-c are trasnferred to BLDTIE.
C
              call bldtie(zonex, numznx)
              call prtime('BUILD_AREA_ZONE_TABLES')
 
              bldtbl = .false.
 
           endif
           crtswx = crtsw
           crtsw = 0
           call getdrp (dropmw, error)
           call prtime('GENDROP')
           if (error .ne. 0) call erexit
C
C          Set "REBUILD" switch on to force BTABLE.
C
           bldtbl = .true.
           kspare(1) = 1
           crtsw = crtswx
           inrcd = buf
           if (findstr ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
           buf = inrcd
 
        else if (findstr(buf,'%LOAD') .ne. 0) then
 
           if (bldtbl) then
C
C             BTABLE must be called prior to %LOAD_DISTRIBUTION since
C             system changes have been made and all array must
C             be updated.
C
              fltstr = 0
              call btable (fltstr, .false.)
              call prtime('BUILD_TABLE')
C
C             Process any EPRI_1964 data.
C
              if (nepbus + nepctl .gt. 0) then
                 call lodepr(zonex, numznx)
                 call prtime('BUILD_EPRI_1964_TABLES')
              else
                 numznx = 0
              endif
C
C             Zones declared in d-c are trasnferred to BLDTIE.
C
              call bldtie(zonex, numznx)
              call prtime('BUILD_AREA_ZONE_TABLES')
 
              bldtbl = .false.
           endif
           crtswx = crtsw
           crtsw = 0
           call cnvtld (error)
           call prtime('%LOAD_DISTRIBUTION')
           if (error .ne. 0) call erexit
C
C          Set "REBUILD" switch on to force BTABLE.
C
           kspare(1) = 1
           crtsw = crtswx
           inrcd = buf
           if (findstr ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
           buf = inrcd
 
        else
 
           go to 1100
 
        endif
 
        enddo
 
 1100   if (numchg .gt. 0 .or. kspare(1) .eq. 1) then
           crtsw = 0
           if (fltstr .eq. -1) then
              if (nadd .gt. 0 .or. ndel .gt. 0) then
                 fltstr = 1
              else
                 fltstr = 0
              endif
           endif
           call btable (fltstr, .false.)
           call prtime('BUILD_TABLE')
           crtsw = crtswx
        endif
 
C       BUILD-OUTPUT-SORT-ORDER-TABLES
 
        crtsw = 0
        call osorto
C
C          Process any EPRI_1964 data.
C
           if (nepbus + nepctl .gt. 0) then
              call lodepr(zonex, numznx)
              call prtime('BUILD_EPRI_1964_TABLES')
           else
              numznx = 0
           endif
C
C          Zones declared in d-c are trasnferred to BLDTIE.
C
           call bldtie(zonex, numznx)
           call prtime('BUILD_AREA_ZONE_TABLES')
 
C       INPUT_DATA_LISTING
C             (0,1)
 
C       LIST CHANGES
 
        crtsw = crtswx
        if (numchg .gt. 0) then
           crtsw = 0
           call chglis
           crtsw = crtswx
        endif
 
        status = buildzbo(status)
 
        if( kspare(4)+kspare(5).gt.0 ) then
           crtsw = 0
           call datlis
           call prtime('DATA_LIST')
           crtsw = crtswx
           kspare(4) = 0
           kspare(5) = 0
        endif
 
C       SOLVE_CASE
C         (0,1)
 
        islnsw = 1
        if (islnsw .eq. 1 ) then
           crtsw = 0
           status = reordr()
           if (status .ne. 0) go to 900
           call prtime('RE_ORDER')
           call chgorder('inp2opt')             !  EXT2IN
           call prtime('chg_ORDER : inp_TO_opt')
           crtsw = crtswx
           call solton
           ostates = 5
           if (lskp .ne. 1) ostates = 7
           crtsw = 0
           call prtime('SOLUTION')
           call chgorder('opt2inp')              !  INT2EX
           call prtime('chg_ORDER : opt_TO_inp')
           kspare(34) = 1
           crtsw = crtswx
 
        endif
 
        inrcd = buf
        if (findstr ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
        buf = inrcd
 
C       OUTPUT_CASE_RESULTS
C             (0,1)
 
        ioutsw = 1
        if( ioutsw .eq. 1 ) then
 
           crtsw = 0
           call output
           call prtime('OUTPUT')
           call analys
           call prtime('ANALYSIS')
           if( kspare(12).ne. 0 ) then
              call interc
              call prtime('INTERCHG OUTPUT')
           endif
           if( kspare(13) .ne. 0 ) then
              call owner
              call prtime('OWNER_INTERCHG')
           endif
           kspare(35) = 1
           crtsw = crtswx
 
C          INPUT_DATA_LISTING
C                (0,1)
 
           if( kspare(4)+kspare(5).gt.0 ) then
              crtsw = 0
              call datlis
              call prtime('DATA_LIST')
              crtsw = crtswx
              kspare(4) = 0
              kspare(5) = 0
           endif
 
        endif
 
C       SAVE_RESULTS_ON_DATAO_FILE
C                 (0,1)
 
        if ( kspare(14) .eq. 1 ) then
 
              crtsw = 0
              call wrdtao
              call prtime('WRITE_DATAO')
              call close_file(datao)
              crtsw = crtswx
 
        endif

C       SAVE_RESULTS_ON_WSCC_FILE

        kstata = 0
        kstatb = 0
        if ( wscc_aname .ne. ' ' ) then
           crtsw = 0
           last = lastch( wscc_aname )
           status = open_file ( wscfil, wscc_aname(1:last),
     &                               'F', 'W', iostat )
           if ( status .eq. 0 ) then
              kstata = savewscc (wscfil, 'ASCII')
              call close_file (wscfil)
              call prtime('WRITE_WSCC_ASCII')
           else
              write ( errbuf(1), 711 ) wscc_aname(1:last), iostat
  711         format(' failure on opening file ', a,
     &           ' error code =', i2)
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              call close_file (wscfil)
           endif
           crtsw = crtswx
        endif
        if ( wscc_bname .ne. ' ' ) then
           crtsw = 0
           last = lastch( wscc_bname )
           status = open_file ( wscfil, wscc_bname(1:last),
     &                               'U', 'W', iostat )
           if ( status .eq. 0 ) then
              kstatb = savewscc (wscfil, 'BINARY')
              call close_file (wscfil)
              call prtime('WRITE_WSCC_BINARY')
           else
              write ( errbuf(1), 721 ) wscc_bname(1:last), iostat
  721         format(' failure on opening file ', a,
     &           ' error code =', i2)
              if (is_batch .eq. 0) then
                 call prterx ('E',1)
              else
                 call prterx ('F',1)
              endif
              call close_file (wscfil)
           endif
           crtsw = crtswx
        endif
        if ( kstata .ne. 0  .or.  kstatb .ne. 0 ) then
           errbuf(1) = ' Error writing WSCC Stability file.'
           errbuf(2) = ' Possibly due to multi_terminal DC data.'
           errbuf(3) = ' WSCC Stability file NOT written.'
           errbuf(4) = ' '
           call prterx ('W', 4)
        endif 
 
C       FAST_OUTAGE
C          (0,1)
 
  800   if( jobreq(1) .eq. 'OUTAGE_SIM' ) then
 
              crtsw = 0
              call fstout
              call prtime('OUTAGE_SIMULATION')
              crtsw = crtswx
 
        endif
 
        if (findstr ('(/HCS',inrcd(1:1)) .ne. 0) call ctlpow
 
        go to 910

  900   call erexit ()
  910   inrcd = buf
        return
 
        end
