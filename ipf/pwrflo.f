C    @(#)pwrflo.f	20.5 2/28/00
        subroutine pwrflo(pfc_file)
        character * 60  pfc_file

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'

        common / is_bpf / is_bpf

        common /dependency/ dependency_flag
        integer dependency_flag
        integer chkerr

        character null*1, ctemp*60
        logical finished
C
C       Force linkage of   Block   Data   modules
C
        call init_bd_all

c       initialize to blank for conditional testing
c        ( powerflow, caseid=xxx, case des=xxxxx )

        chase1(1) = '  '
        chase1(34) = '  '
        chase1(35) = '  '

        null = char(0)
c
c       Set batch flag .TRUE.
c 
        batch = .true.
        is_bpf = 1
c
c       put the file name (passed as an arguement) into common
c
        inpnm = pfc_file
c
c       Make sure inpnm has blank fill
c
        i = index( inpnm, null )
        ctemp = inpnm
        do while ( i .ne. 0 )
           ctemp(i:) = inpnm(i+1:)
           inpnm = ctemp
           i = index( inpnm, null )
        enddo

        call pflbeg
        call prtime('PFL-BEGIN')
c
c       Initialize dependency flag
c 
        dependency_flag = 0

        finished = endjob
        do while ( .not. finished ) 
           call procse
           call prtime ('PROC_CASE')
           if (endjob) then
             if (dependency_flag .eq. 0) then
               finished = .true.
             else
C
C              Skip remaining records in current case
C
               last = min0 (60, lastch(inrcd))
               write (errbuf(1), 10202) chase1(1)
10202          format (' Curent case (', a, 
     &') is skipped by /CASE_DEPENDENCY option and fatal errors')

               do while (inrcd(1:1) .ne. '(' .and. inrcd(1:1) .ne. '[')
                 write (errbuf(1), 10204) inrcd(1:last)
10204            format (' Skipping command (', a, ')')
                 call prterx ('W', 1)
                 card = inrcd(1:1)
C
C                Examine next input record
C
                 read (inp, fmt='(a)', end=10206) inrcd
                 card = inrcd(1:1)
               enddo

               go to 10208
 
10206          inrcd = '(STOP)'
               card = inrcd(1:1)

10208          continue

             endif
           endif
        enddo
 
        call pflend
        iercnt = chkerr('F') + chkerr('A')
        call exit(iercnt)
        end
