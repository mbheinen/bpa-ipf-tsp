C    %W% %G%
      subroutine getfnam 

C     Picks up file names from the file_list file or VMS assignment
C     statements
C     New on Jul/31/92
C
      include 'tspinc/files.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/reread.inc' 
C
      character keyword*8, filnam*60, word(10)*60, linefeed*1, tab*1
      integer status, open_file
C
      tab = char(9)
      linefeed = char(10)
      bsefl  = ' '
      ctrlfl = ' '
      savifl = ' '
      savofl = ' '
      solfl  = ' '
      pltfl  = ' '
      prtfl  = ' '
      auxfl  = ' '
      dbgifl = '0'
      dbgofl = '0'
      postmstr = ' '
      iabort = 0

C     VAX can have missing file list file

      if (filefl(1:1) .eq. ' ') goto 403

C     Read file list file
C     Unit L1 used only for this subroutine.

      status = open_file (l1, filefl, 'F', 'R', iostat)
      if (status .ne. 0) go to 403

      do while (.true.)
c
c       Only exit to this read loop is e-of-f, .i.e., end=911
c
        read (l1,'(A)',end=911) buffer
        call uscan(buffer, word, nwrd, '!=', ' ,' // tab // linefeed)
        iwrd = 1
        if (iwrd .lt. nwrd .and. word(iwrd) .ne. '!') then
          keyword = word(iwrd)
          iwrd = iwrd + 1
          if (word(iwrd) .eq. '=') iwrd = iwrd + 1
          if (iwrd .gt. nwrd .or. word(iwrd) .eq. '!') then
            call puterr (1,'GETFNAM - next line has file name missing.')
            call puterr (2,buffer)
            call prterr ('W',2)
            iabort = 1
          else

C           check for acceptable file type

            filnam = word(iwrd)

C           determine type & assign name

            if     (keyword .eq. 'PFBASE') then
              bsefl = filnam
            elseif (keyword .eq. 'FOR003') then 
              bsefl = filnam
            elseif (keyword .eq. 'CTRL') then
              ctrlfl = filnam
            elseif (keyword .eq. 'FOR005') then
              ctrlfl = filnam
            elseif (keyword .eq. 'FOR025') then
              ctrlfl = filnam
            elseif (keyword .eq. 'SAVIN') then
              savifl = filnam
            elseif (keyword .eq. 'FOR009') then
              savifl = filnam
            elseif (keyword .eq. 'SAVOUT') then
              savofl = filnam
            elseif (keyword .eq. 'FOR015') then
              savofl = filnam
            elseif (keyword .eq. 'SOL') then
              solfl = filnam
            elseif (keyword .eq. 'FOR008') then
              solfl = filnam
            elseif (keyword .eq. 'PLTOUT') then
              pltfl = filnam
            elseif (keyword .eq. 'FOR022') then
              pltfl = filnam
            elseif (keyword .eq. 'PRTOUT') then
              prtfl = filnam
            elseif (keyword .eq. 'FOR006') then
              prtfl = filnam
            elseif (keyword .eq. 'FOR026') then
              prtfl = filnam
            elseif (keyword .eq. 'AUXOUT') then
              auxfl = filnam
            elseif (keyword .eq. 'FOR011') then
              auxfl = filnam
            elseif (keyword .eq. 'DBGIN') then
              dbgifl = filnam
            elseif (keyword .eq. 'DBGOUT') then
              dbgofl = filnam
            elseif (keyword .eq. 'POSTMSTR') then
              postmstr = filnam
            else 
              call puterr (1,
     &          'GETFNAM - next line has unknown file type.')
              call puterr (2,buffer)
              call prterr ('W',2)
              goto 231
            endif 
            call putout (' ',buffer)
            call prtout (1)

C           Finished processing this line 

 231        continue
          endif
        endif
      enddo
      goto 911

 403  target = 0.0

C     Here if relying in VMS-style assign statments to get file
C     names
C     Input file names

      open (l5,
c    &      readonly,
     &      status='OLD')
      inquire (l5,name=ctrlfl)
      close (l5)

      open (l3,
c    &      readonly,
     &      status='OLD')
      inquire (l3,name=bsefl)
      close (l3)

c     Unit l9 might not be needed, so skip inquiry if savifl can't
c     be found

      open (l9,
c    &      readonly,
     &     status='OLD',
     &     err=427)
      inquire (l9,name=savifl)
      close (l9)
      if (index (savifl,'FOR009') .gt. 0) savifl = ' '

 427  continue

c     Unit l15 might not be needed, so skip inquiry if savofl can't
c     be written to

      open (l15,status='SCRATCH',err=433)
      inquire (l15,name=savofl)
      close (l15)
      if (index (savofl,'FOR015') .gt. 0) savofl = ' '

 433  continue
      open (l8,status='SCRATCH')
      inquire (l8,name=solfl)
      close (l8)
      if (index (solfl,'FOR008') .gt. 0) solfl = ' '

      open (l6,status='SCRATCH')
      inquire (l6,name=prtfl)
      close (l6)
      if (index (prtfl,'FOR006') .gt. 0) prtfl = ' '

      open (l11,status='SCRATCH')
      inquire (l11,name=auxfl)
      close (l11)

      open (l22,status='SCRATCH')
      inquire (l22,name=pltfl)
      close (l22)

      open (l23,status='SCRATCH')
      inquire (l23,name=postmstr)
      close (l23)
C
C     Here if at end of file.  
C     Display file names 

 911  write (*,'(2A)') ' Control file:        ', ctrlfl
      write (*,'(2A)') ' PF base file:        ', bsefl
      write (*,'(2A)') ' Old saved data file: ', savifl
      write (*,'(2A)') ' New saved data file: ', savofl
      write (*,'(2A)') ' History file:        ', solfl
      write (*,'(2A)') ' Printout file:       ', prtfl
      write (*,'(2A)') ' Plot file:           ', pltfl
      write (*,'(2A)') ' Auxiliary list file: ', auxfl
      write (*,'(2A)') ' PostScript Master:   ', postmstr
      if (dbgifl .ne. '0')
     &  write (*,'(2A)') ' Debug input file:    ', dbgifl
      if (dbgofl .ne. '0')
     &  write (*,'(2A)') ' Debug output file:   ', dbgofl

C     Check for critical missing names.

      close (l1)
      if (bsefl .eq. ' ') then 
        call puterr (1,'GETFNAM - no powerflow base file given.')
        call prterr ('E',1)
        iabort = 1
      endif
      if (ctrlfl .eq. ' ') then
        call puterr (1,'GETFNAM - no control file given.')
        call prterr ('E',1)
        iabort = 1
      endif
C
      if (iabort .gt. 0) call erexit
C
      return 
      end
