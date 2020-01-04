C    @(#)chg_error.f	20.2 12/21/96
        subroutine chg_error ()
 
C       This subroutine performs a change-specific check on the
C       change records.
 
        include 'ipfinc/parametr.inc'
  
        include 'ipfinc/blank.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'
 
        common /scratch/ array(MAXCHG), out_buffer(10)
        integer array
        character out_buffer*120
 
        common /is_batch / is_batch

      	external kmpchgls, swpchgls
        character record*120, linefeed*1
        integer fchtmp, crtemp, count, ptr, find_index, status,
     &          chk_aredta, chk_arefld, chk_tiedta, chk_tiefld,
     &          chk_busdta, chk_busfld, chk_cbsdta, chk_cbsfld,
     &          chk_xdtdta, chk_xdtfld, chk_brndta, chk_brnfld
        logical error_flag

        linefeed = char(10)

        lprtmp = lprtsw
        fchtmp = fichsw
        crtemp = crtsw
 
        call forbtm ()
        write (outbuf,10)
   10   format (t2, ' Change records sanity checks')
        call rpnlod ()
        call fortop ()

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

        do ix = 1, numchg
           ic = array(ix)
           if (chgcrd(ic)(1:1) .eq. '.' .or. 
     &         chgcrd(ic)(1:1) .eq. '/' .or. 
     &         chgcrd(ic)(3:3) .eq. 'D') then
           else if (chgcrd(ic)(121:121) .eq. ' ') then

              out_buffer(1) = 
     &          ' BZ field warning in following change record'
              last = min0 (lastch (chgcrd(ic)), 80)
              out_buffer(2) = '(' // chgcrd(ic)(1:last) // ')'
              count = 2
              error_flag = .false.

              if (chgcrd(ic)(1:1) .eq. 'A') then

                 ptr = find_index (chgcrd(ic))
                 if (ptr .gt. 0) then
                    status = chk_arefld (chgcrd(ic), ptr, count, 
     &                                   out_buffer)
                    if (status .ne. 0) error_flag = .true.
                 endif

              else if (chgcrd(ic)(1:1) .eq. 'I') then

                 ptr = find_index (chgcrd(ic))
                 if (ptr .gt. 0) then
                    status = chk_tiefld (chgcrd(ic), ptr, count, 
     &                                   out_buffer)
                    if (status .ne. 0) error_flag = .true.
                 endif

              else if (chgcrd(ic)(1:1) .eq. 'B') then

                 ptr = find_index (chgcrd(ic))
                 if (ptr .gt. 0) then
                    status = chk_busfld (chgcrd(ic), ptr, count, 
     &                                   out_buffer)
                    if (status .ne. 0) error_flag = .true.
                 endif

              else if (chgcrd(ic)(1:1) .eq. '+') then

                 ptr = find_index (chgcrd(ic))
                 if (ptr .gt. 0) then
                    status = chk_cbsfld (chgcrd(ic), ptr, count, 
     &                                   out_buffer)
                    if (status .ne. 0) error_flag = .true.
                 endif

              else if (chgcrd(ic)(1:1) .eq. 'X') then

                 ptr = find_index (chgcrd(ic))
                 if (ptr .gt. 0) then
                    status = chk_xdtfld (chgcrd(ic), ptr, count, 
     &                                   out_buffer)
                    if (status .ne. 0) error_flag = .true.
                 endif

              else if (index ('LERT', chgcrd(ic)(1:1)) .ne. 0) then
 
                 ptr = find_index (chgcrd(ic))
                 if (ptr .gt. 0) then
                    status = chk_brnfld (chgcrd(ic), ptr, count, 
     &                                   out_buffer)
                    if (status .ne. 0) error_flag = .true.
                 endif

              endif

              if (error_flag) then
                if (is_batch .eq. 0) then
                  do i = 1, count
                    last = min0 (lastch(out_buffer(i)), 120)
                    errbuf(i) = out_buffer(i)(1:last)
                  enddo
                  call prterx ('E', count)
                else
                  do i = 1, count
                    last = min0 (lastch(out_buffer(i)), 131)
                    outbuf = ' ' // out_buffer(i)(1:last)
                    call prtout (1)
                  enddo
                endif
              endif
           endif

        enddo

        lprtsw = lprtmp
        fichsw = fchtmp
        crtsw = crtemp

        return
        end
