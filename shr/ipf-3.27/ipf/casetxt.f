C    @(#)casetxt.f	20.4 2/13/96
C****************************************************************
C
C       File: casetxt.f
C
C       Purpose: Subroutine to load headers and comments from netdat
C                file. This routine read the particular ".#" formatted
C                records written in subroutine savenetd.f.
C
C       Author: Walt Powell  Date: 16 May 1994
C       Called by: bsread, bread
C
C****************************************************************
C
        subroutine casetxt (text)

        character text*(*)
 
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/basval.inc'
        include 'ipfinc/header.inc'
        include 'ipfinc/coment.inc'
        include 'ipfinc/prt.inc'

        common / is_bpf / is_bpf

        character  null * 1, linefeed * 1, prgvsnxx * 10

        save

        null = char(0)
        linefeed = char(10)
c
c       Process case records
c
        if (text(1:11) .eq. '.#CASE_ID =') then
           basval(4) = text(13:)
        else if (text(1:11) .eq. '.#CASE_DS =') then
           basval(7) = text(13:)
        else if (text(1:4) .eq. '.#H1') then
           i = index (text, 'H Version: ')
           if (i .gt. 0) then
              i = i + len ('H Version: ')
              prgvsnxx = text(i:)
           endif
           i = index (text, 'CaseID: ')
           if (i .gt. 0) then
              i = i + len ('CaseID: ')
              basval(4) = text(i:i+9)
           endif
           i = index (text, 'Description: ')
           if (i .gt. 0) then
              i = i + len ('Description: ')
              basval(7) = text(i:)
           endif
           i = index (text, 'Date: ')
           if (i .gt. 0) then
              i = i + len ('Date: ')
              basval(5) = text(i:)
           endif
        else if (is_bpf .ne. 1 .and. 
     &           (text(1:4) .eq. '.#H2'  .or.  text(1:4) .eq. '.#H3')
     &            ) then
           if (coment(1) .eq. ' ') then
              coment(1) = text(6:)
           else if (coment(2) .eq. ' ') then
              coment(2) = text(6:)
           endif
        else if (text(1:3) .eq. '.#C' .and. 
     &           index (' 0123456789', text(4:4)) .ne. 0 .and.
     &           index (' 0123456789', text(5:5)) .ne. 0 .and.
     &           index (' 0123456789', text(6:6)) .ne. 0 .and.
     &           is_bpf .ne. 1 ) then
           ncom = ncom + 1
           if ( ncom .lt. MAXCMT+1 ) com(ncom) = text(8:)
        endif

        if ( is_bpf .eq. 1 ) then
c          ! Force to (powerflow) record values, if specified
           if ( chase1(1) .ne. '  ' ) then
              basval(4) = chase1(1)
           endif
           if ( chase1(34) .ne. '  ' ) then
              clabl1 = chase1(34)
              clabl2 = chase1(35)
              basval(7) = clabl1 // clabl2
           endif
        else
c          ! Redefine these variables only for non-BPF  runs.
c          ! In BPF, they are already defined in CTLCOM.
           chase1(1) = basval(4)
           clabl1 = basval(7)(1:10)
           clabl2 = basval(7)(11:20)
           chase1(34) = clabl1
           chase1(35) = clabl2
        endif

        return
        end
