C    @(#)ipf_diff.f	20.3 10/21/94
C****************************************************************
C
C   	File: ipf_diff
C
C   	Purpose: Obtain a file of change differences
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: p_gtdata.f
C
C****************************************************************
C
      	program ipf_diff

        implicit none
 
        common /difference/ numtxt1, array1, text1, numtxt2, array2, 
     &                      text2, numchg, change

        integer MAXTEXT, MAXCHG
        parameter (MAXTEXT = 10000)
        parameter (MAXCHG = 4000)

        character text1(MAXTEXT)*120, text2(MAXTEXT)*120,
     &            change(MAXCHG)*120
        integer array1(MAXTEXT), numtxt1, array2(MAXTEXT), numtxt2, 
     &          numchg

        integer i, status, open_file, iostat, last, lastch
        external komp_txt1, swap_txt1, komp_txt2, swap_txt2
        logical finished
        character *60 file1, file2, file3

        file1 = 'editbus.dat'
        file2 = 'editbusn.dat'
        file3 = 'editbusc.dat'

       numtxt1 = 0
       numtxt2 = 0
       last = lastch (file1)
        status = open_file (11, file1(1:last), 'F', 'R', iostat)
        if (status .eq. 1) then
           write (*, 100) file1(1:last)
  100      format(' file ', a, 
     &            ' has read protection--request ignored')
        else if (status .eq. 3) then
           write (*, 110) file1(1:last)
  110      format(' file ', a, 
     &            ' has write protection--request ignored')
        else if (status .eq. 2 .or. status .eq. 4) then
           write (*, 120) file1(1:last), iostat
  120      format(' failure on opening file ', a,
     &           ' error code =', i2)
        else
           rewind 11
           finished = .false.
             do while (.not. finished)
              read (11, '(a)', end = 130) text1(numtxt1+1)
              numtxt1 = numtxt1 + 1
           enddo
  130      call close_file(11)
        endif

        last = lastch (file2)
        status = open_file (11, file2(1:last), 'F', 'R', iostat)
        if (status .eq. 1) then
           write (*, 100) file2(1:last)
        else if (status .eq. 3) then
           write (*, 110) file2(1:last)
        else if (status .eq. 2 .or. status .eq. 4) then
           write (*, 120) file2(1:last), iostat
        else
           rewind 11
           finished = .false.
           do while (.not. finished)
              read (11, '(a)', end = 140) text2(numtxt2+1)
              numtxt2 = numtxt2 + 1
           enddo
  140      call close_file(11)
        endif

        numchg = 0
        if (numtxt1 .gt. 0) then
           do i = 1, numtxt1
              array1(i) = i
           enddo
           call qiksrt (1, numtxt1, komp_txt1, swap_txt1)
        endif

        if (numtxt2 .gt. 0) then
           do i = 1, numtxt2
              array2(i) = i
           enddo
           call qiksrt (1, numtxt2, komp_txt2, swap_txt2)
        endif
        call merge_diff

        last = lastch (file3)
        status = open_file (11, file3(1:last), 'F', 'W', iostat)
        if (status .ne. 0) then
           write (*, 100) file3(1:last)
        else if (status .eq. 2) then
           write (*, 110) file3(1:last)
        else if (status .gt. 0) then
           write (*, 120) file3(1:last), iostat
        else
           rewind 11
           finished = .false.
           do i = 1, numchg
              write (11, '(a)') change(i)
           enddo
           call close_file(11)
        endif

        write (*, 170) numchg, file3(1:last)
  170   format (1x, i5, ' records written to change file ', a)

  900   continue
        end
