C    @(#)gtbase.f	20.7 3/29/99
      subroutine gtbase (obname, caseid, baseok)
      character obname *(*), caseid *(*)
      logical baseok
C ***                                                                  *
C *** OPEN OLD-BASE FILE OBNAME: (CALL OPNFIL)                         *
C ***    IF NOT "PF DATA" THEN                                         *
C ***       PRINT ERROR                                                *
C ***    ELSE                                                          *
C ***       SEARCH FOR CASEID AND LOAD IT                              *
C ***       IF CASE LOADED THEN                                        *
C ***          BASEOK = .TRUE.                                         *
C ***       ELSE                                                       *
C ***          BASEOK = .FALSE.                                        *
C ***       ENDIF                                                      *
C ***    ENDIF                                                         *
C *** CLOSE OLD-BASE FILE OBNAME:                                      *
C ***                                                                  *
 
      include 'ipfinc/blank.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      common /basnam/ basnam
      character basnam * 60
 
      common /is_batch / is_batch
  
      integer status
 
      call close_file (datai)
      ierro = 99
      call opnfil(datai,obname,ierro)
      if (ierro .eq. 0) then
         inquire (unit=datai,name=basnam)
         rewind datai
         call rddtai(caseid,status)
         call close_file (datai)
      else
         status = -1
      endif
      if ( status .eq. 1 ) then
         baseok = .true.
         if (count(1) .eq. 0) then
            write ( errbuf(1),100) caseid
  100       format ('0 Case ', a, ' had a failed solution')
            call prterx ('W',1)
            kspare(1) = 1
         endif
C ***                                                                  *
         if (kspare(22) .lt. kspare(33)) then
C ***                                                                  *
C ***       DATAI FILE HAS NOT BEEN CONVERTED TO CURRENT DATAO FORMAT. *
C ***       FORCE UPDATE.                                              *
C ***                                                                  *
            write (errbuf(1),110) caseid, kspare(22), kspare(33)
  110       format ('0 CASE ', a, ' IS OLD VERSION "', i3, '" AND IS ',
     1          'CONVERTED TO NEW VERSION "', i3, '".')
            call prterx ('W',1)
            if (kspare(22) .eq. 8 .and. kspare(33) .eq. 9) then
            else
              kspare(1) = 1
            endif
C ***                                                                  *
         else if (kspare(22) .gt. kspare(33)) then
C ***                                                                  *
C ***       DATAI FILE HAS NOT BEEN CONVERTED TO CURRENT DATAO FORMAT. *
C ***       FORCE UPDATE.                                              *
C ***                                                                  *
            write (errbuf(1),112) caseid, kspare(22), kspare(33)
  112       format ('0 CASE ', a, ' IS NEW VERSION "', i3, 
     1              '" WHICH MAY NOT BE DOWNWARDS COMPATIBLE ',
     2              'WITH OLD PF VERSION "', i3, '".')
            call prterx ('W',1)
            kspare(1) = 1
C ***                                                                  *
         endif
         oldcse = caseid
C ***                                                                  *
      else if( status .eq. 0 ) then
         baseok = .false.
         write (errbuf(1),120) caseid
  120    format ('CASE "', a, '" IS NOT ON OLD-BASE FILE!')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
C ***                                                                  *
      else if( status .eq. 2 ) then
         baseok = .false.
         i = lastch( obname )
         write (errbuf(1),130) obname(1:i)
  130    format ('FILE "', a, '" IS NOT "PF DATA" ON OLD-BASE FILE.')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
C ***                                                                  *
      else
         i = lastch( obname )
         write (errbuf(1),150) obname(1:i)
  150    format ('FILE "', a, '" COULD NOT BE OPENED.......')
         baseok = .false.
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
C ***                                                                  *
      endif
      call close_file (datai)
      jobreq(2) = ' '
C ***                                                                  *
      return
      end
