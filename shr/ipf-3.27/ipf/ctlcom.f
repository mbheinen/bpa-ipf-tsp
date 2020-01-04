C    @(#)ctlcom.f	20.4 7/18/96
        subroutine ctlcom
C                               *** CTLCOM ***
C                               CONTROL COMMAND
C
C       When called, the buffer INRCD contains a '(' or a '['
C       control record.
C
C       We are looking for
C
C   [ DUMP_FILE ]    the old [ DEBUG_FILE ] command changed so that
C                 users will stop using it and creating *.PFD files.
C   [ FICHE,COPIES = n ]
C   ( POWER_Flow, CASEid=cccccccccc,PROJect_id=pppppppppppppppppppp)
C   ( NEXT_Case,  CASEid=cccccccccc,PROJect_id=pppppppppppppppppppp)
C   ( END )
C   ( STOP )
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/com001.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      common /is_batch / is_batch

        dimension word(40)
        character word*30, jobnme*10
        equivalence (jobnme,jobreq(1))
        integer ctyp
        logical rdnext
 
C       ************     CONTROL : LEVEL 1 DICTIONARY    ******
C       ************     CASEID  : LEVEL 2 SUB_DICTIONARY  *****
 
        is_batch = 1           ! 0 = interactive, 1 = batch
c                              ! This routine is called only for batch
        rdnext = .true.
        ctyp = index( '([',inrcd(1:1))
 10     continue
C                                '(' SCANABLE CONTROL ******
        if ( ctyp .le. 2 ) then
           call scan(inrcd,word,nwrd)
           iwrd = 0
  110      iwrd = iwrd + 1
           jobsw = ickdic(word(iwrd),comdic,lctl)
 
           if( jobsw .eq. 0 ) then
              if( iwrd.lt.nwrd )goto 110
  120         write (errbuf(1),130) inrcd
  130         format('0 UNRECOGNIZABLE TEXT:[',a80,']')
              call prterx ('W',1)
C                               READ NEXT RECORD....
              go to 2000
           endif
 
C               SKIP TO APPROPRIATE SUBROUTINE BASED ON JOB TYPE
 
           if ( jobsw.eq.1 .or. jobsw.eq.2 ) then
C                               (POWER_FLOW) or (NEXT_CASE)
              rdnext = .false.
  210         iwrd = iwrd + 1
              if( iwrd .lt. nwrd ) then
                 iopt=ickdic(word(iwrd),iddic,lid)
                 if( iopt .eq. 1 ) then
C                  ** CASE **
                    iwrd = iwrd + 1
                    chase1(1) = word(iwrd)
                 else if( iopt .eq. 2 .or. iopt .eq. 3) then
C                  ** PROJ* ** or DESC*
                    iwrd = iwrd + 1
                    chase1(34) = word(iwrd)(1:10)
                    chase1(35) = word(iwrd)(11:20)
                 else
C                  ** BAD PARAMETER
                    write (errbuf ,140) word(iwrd)
 140                format('0 UNRECOGNIZABLE PARAMETER: (',a,')')
                    call prterx ('W',1)
                 endif
                 go to 210
              endif
C                           READ AHEAD, SKIPING '.' COMMENTS
 150          read(inp,135,end=5000) inrcd
              outbuf = ' '//inrcd
              call prtout(1)
              if ( inrcd(1:1) .eq. '.' ) goto 150
              jobnme = 'POWER_FLOW'
              call ctlpow
C                   (END)      or     (STOP)
           else if (jobsw .eq. 3 .or. jobsw .eq. 4) then
              jobnme = word(iwrd)
              rdnext = .false.
 
           else if ( jobsw .eq. 5 ) then
C               ( FICHE,COPIES = n )   or    [ FICHE,COPIES = n ]
 
              iwrd = iwrd + 1
              if( iwrd .lt. nwrd ) then
C                               Skip COPIES and pick up n
                 kspare(16) = rval( word(iwrd+1) )
              else
C                                       SET COPIES = 0
                 kspare(16) = 0
              endif
 
C                KSPARE = -1 WHEN NO FICHE AT ALL WANTED
              if (kspare(16).ge.0) then
C                SET UP "FICHE ON" DEFAULTS
                 call fshlbl( mfich, kspare(16) )
C                                FICHE INPUT DATA LIST
                 kspare(5) = 2
C                                FICHE OUTPUT LISTING
                 kspare(7) = 2
C                                ANALYSIS LISTING LEVEL ON FICHE
                 kspare(9) = 4
                 fichsw = 1
              endif
           else if ( jobsw .eq. 6 ) then
C                                               [ DUMP_FILE ]
               kspare (17) = 1
           endif
        else if( ctyp .eq. 0 ) then
C                               DATA RCD, RETURN *****
           rdnext = .false.
           jobnme = 'DATA'
        endif
C                                      CTLCOM.END
 2000   continue
        if( rdnext ) then
C                               READ-NEXT-RECORD
 2010      read(inp,135,end=5000) inrcd
  135      format(a)
           if (inrcd(1:1) .eq. '.') go to 2010
 3000      ctyp = index('([',inrcd(1:1))
           if( ctyp.gt.0 ) then
              outbuf = ' '//inrcd
              call prtout(1)
              goto 10
           endif
 
        endif
 
C       STOP-PROCESSING-CONTROL-RECORDS
 
 4000   continue
        buf = inrcd
        return
 
 5000   continue
        write (errbuf(1),5010)
 5010   format(' UNEXPECTED END OF FILE (EOF)  ',
     1          'IN THE CONTROL STREAM ')
        call prterx ('W',1)
        jobnme = 'STOP'
        inrcd = '( STOP - EOF )'
        goto 4000
        end
