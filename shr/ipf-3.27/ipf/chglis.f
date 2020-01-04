C    @(#)chglis.f	20.3 2/13/96
        subroutine chglis
 
C       This subroutine lists change records.
 
        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/blank.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
 
        common /scratch/ array(MAXCHG)
        integer array
 
      	external kmpchgls, swpchgls

        character record*240, linefeed*1
        integer fchtmp, crtemp
 
        linefeed = char(10)

        lprtmp = lprtsw
        fchtmp = fichsw
        crtemp = crtsw
 
        lprtsw=1
        if ( kspare(16) .ge. 0 ) fichsw=1
        crtsw=0
c
c       Initialize OLDCHG cross-reference array
c
        do i = 1, numchg
           array(i) = i
        enddo
c
c       sort chgcrd array by the following fields:
c
C           (122:125) - change record numnber
C             (1:120) - change record
c
        call qiksrt (1, numchg, kmpchgls, swpchgls)

        write (outbuf,10)
   10   format (1x, ' * * * Change records * * *')
        call forbtm
        call rpnlod
        call fortop
 
        do ix = 1, numchg
           ic = array(ix)
           if (chgcrd(ic)(1:1) .eq. '.' .or. 
     &         chgcrd(ic)(1:1) .eq. '/') then
           else if (chgcrd(ic)(121:121) .eq. ' ') then
              if (index ('AI', chgcrd(ic)(1:1)) .ne. 0) then

                 call getaichg (ic, record)

              else if (index ('BX+Q', chgcrd(ic)(1:1)) .ne. 0) then

                 call getbschg (ic, record)

              else if (index ('LERT', chgcrd(ic)(1:1)) .ne. 0) then
 
                 call getbrchg (ic, record)

              else if (chgcrd(ic)(1:1) .eq. 'P') then

                 read (chgcrd(ic)(122:125), '(bz, i4)', err=900) nc
                 write (record, 100) nc, chgcrd(ic)(1:120)
  100            format (1x, i4, 1x, a)

              else

                 read (chgcrd(ic)(122:125), '(bz, i4)', err=900) nc
                 write (record, 100) nc, chgcrd(ic)(1:120)
                 
              endif

              i = index (record, linefeed)
              if (i .eq. 0) then
                 outbuf = record
                 if (chgcrd(ic)(126:126) .eq. 'E') then
                    outbuf(127:132) = '*Err*'
                 endif
                 call prtout(1)
              else
                 outbuf = record(1:i-1) 
                 outbuf(i:) = ' '
                 if (chgcrd(ic)(126:126) .eq. 'E') then
                    outbuf(127:132) = '*Err*'
                 endif
                 call prtout(1)
                 outbuf = record(i+1:) 
                 if (chgcrd(ic)(126:126) .eq. 'E') then
                    outbuf(127:132) = '*Err*'
                 endif
                 call prtout(1)
              endif
           endif
           go to 920

  900      write (errbuf(1), 910) chgcrd(ic)(1:80)                                  
  910      format (' Illegal data in field : (',a80,')')                      
           call prterx ('W',1)                                               
           chgcrd(ic)(126:126) = 'E'

  920      continue

        enddo

        lprtsw = lprtmp
        fichsw = fchtmp
        crtsw = crtemp

        return
        end
