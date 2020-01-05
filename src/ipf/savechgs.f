C    @(#)savechgs.f	20.5 5/27/98
C****************************************************************
C
C   	File: savechgs.f
C
C   	Purpose: Archive all changes and comment records in /oldchg/
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: p_gtdata.f
C
C****************************************************************
C
      	integer function savechgs (chgfil, saveflnm)
        integer chgfil
        character *(*) saveflnm
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/blank.inc'
      	include 'ipfinc/changr.inc'
      	include 'ipfinc/lfiles.inc'
      	include 'ipfinc/oldchg.inc'
      	include 'ipfinc/prt.inc'
      	include 'ipfinc/qksrt.inc'

        common /scratch/ array(MAXCHG), original_code(MAXCHG)
        integer array
        character original_code*1
 
        common /is_batch / is_batch

        integer kmpoldch, oldchgfl, first, count
      	external kmpoldch, swpoldch
        logical loop
        character txpose*128, tempc*128

        savechgs = 0
      	if (numold .eq. 0) go to 900
c
c       Convert relative change indices (beginnng at "/ CHANGE" record)
c       to absolute indices
c
        do i = 1, numold
          original_code(i) = oldchg(i)(126:126)
        enddo

        i = 1
        do while (i .le. numold)
           original_code(i) = oldchg(i)(121:121)
           if (oldchg(i)(1:1) .eq. '/') then
              read (oldchg(i)(122:125), '(bz, i4)') num
              write (oldchg(i)(122:125), '(i4)') num + i - 1
              oldchg(i)(126:126) = 'E'
              j = i + 1
              do while (j .le. numold .and. oldchg(j)(1:1) .ne. '/')
                 read (oldchg(j)(122:125), '(bz, i4)') num
                 write (oldchg(j)(122:125), '(i4)') num + i - 1 
                 j = j + 1
              enddo
              i = j
           else
             write (errbuf(1), 80) 
   80        format(' CHANGE archive records does not begin with "/ CHAN
     &ges, file = *" header ')
             write (errbuf(2), 82) 
   82        format(' Change indices lost ')
             write (errbuf(3), 90) i, oldchg(i)
   90        format(' Record No. ', i4, ' (', a40, ')')
             if (is_batch .eq. 0) then
                call prterx ('E',3)
             else
                call prterx ('F',3)
             endif
             i = i + 1
           endif
        enddo
c
c       Flag duplicate pseudo-changes
c
        do i = 2, numold
           if (oldchg(i)(121:121) .eq. 'P') then
              oldchg(i)(126:126) = 'E'
           else if (index ('LERT', oldchg(i-1)(1:1)) .ne. 0 .and.
     &         index ('LERT', oldchg(i)(1:1)) .ne. 0) then
              if (index ('TU', oldchg(i)(121:121)) .ne. 0) then
                oldchg(i)(126:126) = 'E'
              else if (oldchg(i-1)(1:6) .eq. oldchg(i)(1:6) .and.
     &                 index ('TU', oldchg(i-1)(121:121)) .ne. 0 .and.
     &                 index ('TU', oldchg(i)(121:121)) .ne. 0) then
                tempc = txpose(oldchg(i-1))
                if (tempc .eq. oldchg(i)) then
                   oldchg(i)(126:126) = 'E'
                endif
              endif
           endif
        enddo
c
c       Initialize OLDCHG cross-reference array
c
        do i = 1, numold
           array(i) = i
        enddo
c
c       sort oldchg array by the following fields:
c
C             (1:120) - change record
C           (122:125) - change record numnber
C
c       Set "key" = 0 to enable kmpoldch to break ties using change 
c       index.
c
        key = 0
        call qiksrt (1, numold, kmpoldch, swpoldch)

c       Debug printout

        if (kase1(27) .ne. 0) then
           write (dbug, 110)
  110      format ('0 Dump of OLDCHG array ', /
     1         t2, 'Num', t7, 'Record', t127, 'P', t129, 'Num', /)
           do ic = 1, numold
              i = array(ic)
              write (dbug, 120) i, oldchg(i), oldchg(i)(121:128) 
  120         format (t2, i4, 1x, a80, 1x, a)
           enddo
        endif
c
c       Reduce changes as follows:
c
c       Add + Mod -> Add'
c       Add + Del -> both cancel
c       Add + Res -> Illegal - program error
c       Mod + Mod -> Mod'
c       Mod + Del -> Del'
c       Mod + Add -> Illegal - program error
c       Mod + Res -> Illegal - program error
c       Del + Add -> Mod'
c       Del + Res -> both cancel
c       Del + Mod -> Illegal - program error
c
c       Change reduction consolidates into a single change record
c       the aggregate of a set of changes. The aggregated records are
c       deleted by setting a error flag in oldchg()(126:136):
c
C           (121:121) - origination code
C                       ' ' indicates original record
C                       'P' inidcates pseudo-change record
C                       'T' indicates transpose record 
C                       'U' indicates transpose psuedo-change record 
c           (126:126) - process code 
c                       'P' = processed
c                       'E' = error
c                       ' ' = not processed
c
c       Search for the range of change sets which can be consolidated.
c       Type P and D change records cannot be consolidated and delimit
c       a change set.
c
        first = 1
        last = first + 1
        do while (last .le. numold)
           loop = .true.
           marker = 0       ! marker holds the index of the last
C                           ! '/' command record
C                           !
           do while (last .le. numold .and. loop)
              if (oldchg(array(last))(126:126) .eq. 'E') then
                 last = last + 1
              else if (oldchg(array(last))(1:1) .eq. 'P' .or.
     &                 oldchg(array(last))(1:1) .eq. 'D') then
c
c                A "P" or "D" change encountered in the first change
c                set is of no concern because these changes are always
c                processed first.
c
                 if (marker .gt. 1) then
                    last = marker - 1
                    loop = .false.
                 else
                    last = last + 1
                 endif
              else if (oldchg(array(last))(1:1) .eq. '/') then
                 marker = last
                 last = last + 1
              else if (oldchg(array(last))(1:1) .eq. ')') then
                 last = last - 1
                 loop = .false.
              else
                 last = last + 1
              endif
           enddo
           if (last .gt. numold) last = numold
c
c          Eliminate intermediate /CHANGES commands, and duplicate
c          interactive comment records.
c
           count = 0
           if (oldchg(first)(1:1) .eq. '/') then
              read (oldchg(first)(127:128), '(bz, i2)') num
              if (num .eq. 0) count = count + 1
           endif

           do i = first+1, last
              read (oldchg(array(i))(127:128), '(bz, i2)') num
              if (oldchg(array(i))(1:1) .eq. '/') then
                 if (num .eq. 0) count = count + 1
              endif
           enddo
           call redchgs (first, last)
           first = last + 1
           last = first
        enddo
c
c       Generate a new change file in absolute order (order as 
c       submitted). 
c
        numchgs = 0
        oldchgfl = 0
        do ic = 1, numold
           if (oldchg(array(ic))(121:121) .eq. ' ') then
              read (oldchg(array(ic))(127:128), '(bz, i2)') newchgfl
              if (oldchg(array(ic))(126:126) .ne. 'P') then
                 if (kase1(27) .ne. 0) then
                    write (dbug, 130) oldchg(array(ic))(1:80), 
     &                                chgfile(newchgfl)
  130               format (' Change record discarded (', a, ')', /
     &                      ' from change file ', a)
                 endif
              else
                 write (chgfil, 140) oldchg(array(ic))(1:120)
  140            format (a)
                 numchgs = numchgs + 1
              endif
           endif
        enddo

        do i = 1, numold
          oldchg(i)(126:126) = original_code(i)
        enddo

        write (outbuf, 170) numchgs, saveflnm
  170   format (1x, i5, ' records written to change file ', a)
        call prtout (1)
        errbuf(1) = outbuf
        call prterx ('I', 1)

  900   continue
        return
        end
