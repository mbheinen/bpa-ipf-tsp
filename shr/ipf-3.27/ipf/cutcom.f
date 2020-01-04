C    @(#)cutcom.f	20.7 7/18/96
        subroutine cutcom
C
C       Control command
c
c       When called, the buffer INRCD contains a '(' or a '['
C       control record.
C
C       We are looking for
C
C       [ DUMP_FILE ] the old [ DEBUG_FILE ] command changed so that
C       users will stop using it, creating *.PFD files.
C       [ FICHE,COPIES = n ]
C       ( CUT, CASEid=cccccccccc,PROJect_id=pppppppppppppppppppp)
C       ( END )
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/coment.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/com003.inc'

        character word(40)*30, sevrty*1, acctno*8, usernm*15
        integer ctyp
        logical rdnext
 
C       ************     CONTROL : LEVEL 1 DICTIONARY    ******
 
C       ************     CASEID  : LEVEL 2 SUB_DICTIONARY  *****
 
C
        rdnext = .true.
        ctyp = index( '([',inrcd(1:1))
 
 10     continue
 
C       '([' SCANABLE CONTROL ******

        if ( ctyp .eq. 1 .or. ctyp .eq. 2 ) then
           call scan(inrcd,word,nwrd)
           iwrd = 0
 
  110      iwrd = iwrd + 1
           jobsw = ickdic(word(iwrd),comdic,lctl)
 
           if ( jobsw .eq. 0 ) then
              if( iwrd.lt.nwrd ) goto 110
  120         write (errbuf(1),130) inrcd
  130         format('0 Unrecognizable text:[',a80,']')
              call prterx ('W',1)
 
C             READ NEXT RECORD....
 
              go to 2000
           endif
 
C          Skip to appropriate subroutine based on job type
 
           if ( jobsw.eq.1 ) then

C             (CUTTING)

              rdnext = .false.
  210         iwrd = iwrd + 1
              if ( iwrd .lt. nwrd ) then
                 iopt=ickdic(word(iwrd),iddic,lid)
                 if ( iopt .eq. 1 ) then
 
C                   ** CASE **
 
                    iwrd = iwrd + 1
                    chase1(1) = word(iwrd)
                 else if( iopt .eq. 2 .or. iopt .eq. 3) then
 
C                   ** PROJ* ** or DESC*
 
                    iwrd = iwrd + 1
                    chase1(34) = word(iwrd)(1:10)
                    chase1(35) = word(iwrd)(11:20)
 
C                   *** BAD PARAMETER
 
                 else
                    write (errbuf ,140) word(iwrd)
 140                format('0 Unrecognizable parameter: [', a, ']')
                    call prterx ('W',1)
                 endif
                 go to 210
              endif
 
C             READ AHEAD, SKIPING '.' COMMENTS
 
 150          read(inp,135,end=900) inrcd
              outbuf = ' '//inrcd
              call prtout(1)
              if ( inrcd(1:1) .eq. '.' ) goto 150
 
              jobreq(1) = 'CUTTING'
              call ctlpow
 
C             (END)

           else if (jobsw .eq. 2 ) then
              jobreq(1) = word(iwrd)
              rdnext = .false.
 
           else if ( jobsw .eq. 3 ) then

C             [ FICHE,COPIES = n ]
 
              iwrd = iwrd + 1
              if ( iwrd .lt. nwrd ) then

C                Skip COPIES and pick up n

                 kspare(16) = rval( word(iwrd+1) )
              else

C                SET COPIES = 0

                 kspare(16) = 0
              endif
 
              if (kspare(16).ge. 0) then
                 call fshlbl( mfich, kspare(16) )
 
C                SET UP "FICHE ON" DEFAULTS (INSTEAD OF ZERO FOR EACH)
 
C                FICHE INPUT DATA LIST

                 kspare(5) = 2
C                FICHE OUTPUT LISTING

                 kspare(7) = 2
C                ANALYSIS LISTING LEVEL ON FICHE

                 kspare(9) = 4
              endif
 
 
           else if ( jobsw .eq. 4 ) then

C             [ DUMP_FILE ]

              kspare (17) = 1
           endif
 
        else if( ctyp .eq. 0 ) then
C                               DATA RCD, RETURN *****
           rdnext = .false.
           jobreq(1) = 'DATA'
 
        endif

C       CTLCOM.END
 
 2000   continue
        if ( rdnext ) then

C          READ-NEXT-RECORD

 2010      read(inp,135,end=900) inrcd
  135      format(a)
 
           if (inrcd(1:1) .eq. '.') go to 2010
 
 3000      ctyp = index('([',inrcd(1:1))
           if ( ctyp.gt.0 ) then
              outbuf = ' '//inrcd
              call prtout(1)
              goto 10
           endif
 
        endif
        go to 4000

  900   write (errbuf(1), 910)
  910   format(' Unexpected end of file (EOF)  ',
     1          'in the control stream ')
        call prterx ('W',1)
        jobreq(1) = 'STOP'
        inrcd = '( STOP - EOF )'
 
C       STOP-PROCESSING-CONTROL-RECORDS
 
 4000   continue
        buf = inrcd
        return
        end
