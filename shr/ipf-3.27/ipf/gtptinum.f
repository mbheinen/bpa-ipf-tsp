C    @(#)gtptinum.f	20.7 2/28/00
C****************************************************************  
C  
C     File: gtptinum.f  
C  
C     Purpose: Routine to obtain PTI area, zone, and bus numbers  
C  
C     Input parameters:  
C  
C             nb       - bus external number  
C             nptia    - PTI area index  
C             nptiz    - PTI zone index  
C             nptib    - PTI bus index  
C  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: saveptid.f  
C  
C****************************************************************  
      integer function gtptinum (nb, iptia, iptiz, iptib)  
      integer nb, iptia, iptiz, iptib  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/filnam.inc'  
      include 'ipfinc/lfiles.inc'  
      include 'ipfinc/bus.inc'  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/area.inc'  
      include 'ipfinc/arcntl.inc'  
      include 'ipfinc/pseudo_b.inc'  
      include 'ipfinc/qsdup.inc'  
      include 'ipfinc/qksrt.inc'   
   
      common /verify_pti/ num_verify, hash_verify(PTI_HASHSIZE), 
     &                    next_verify(PTI_MAXBUS), 
     &                    ptinum_verify(PTI_MAXBUS),
     &                    bpanum_verify(PTI_MAXBUS)
      integer num_verify, hash_verify, next_verify, ptinum_verify,
     &                    bpanum_verify

      integer MAXPTIRECORDS
      parameter (MAXPTIRECORDS = 16000)
      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),  
     &                 nextptr_2(MAXBUS), count_newbus,   
     &                 newbusno(MAXBUS), count_newzone,   
     &                 newzoneno(MAXCZN), count_newown,   
     &                 newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                 sort_tempc(MAXPTIRECORDS)
      integer array, count, htable_2, count_newbus, newbusno, 
     &        count_newzone, newzoneno, count_newown, newownno,
     &        sort_tempc, h, p
      character tempc*80
  
      logical finished, build_area,  found
      integer fnd_ptib, fnd_ptiy, error, add_ptib, add_ptin, add_ptiz,  
     &        fnd_ptia, fnd_ptin, fnd_ptiz, areanos(0:MAXCAR)  
      character tempc2*10  
      external kmp_ptiz, swp_ptiz  
  
      data build_area / .false. /  
  
      save build_area, areanos  
  
      error = 0           ! initialize error count  
      iptia = 0  
      iptiz = 0  
      iptib = 0  
  
      if (.not. build_area) then  
        do i = 1, MAXCAR  
          areanos(i) = 0  
          array(1,i) = i  
          array(2,i) = 0  
        enddo  
  
        key = 5  
        call qiksrt (1, num_anam, kmp_ptiz, swp_ptiz)  
        do i = 1, num_anam  
          areanos(i) = pti_anum(array(1,i))  
        enddo  
        areanos(num_anam+1) = areanos(num_anam) + 10                      
        build_area = .true.  
      endif  
c  
c     Get PTI area number  
c  
      ia = jarzn(nb)  
      tempc2 = arcnam(ia)(1:8)  
      iptia = fnd_ptia (tempc2)  
      if (iptia .le. 0) then  
        write (errbuf(1), 10041) bus(nb), base(nb),   
     &    arcnam(jarzn(nb))  
10041   format (' Bus (', a8, f7.1,   
     &          ') has a non-existant PTI area (', a,')')  
        call prterx ('E',1)  
        error = 1  
        go to 900  
      endif  
c  
c     Get PTI zone number  
c  
      iptiz = fnd_ptiy (zone(nb))  
      if (iptiz .le. 0) then  
c  
c       Search for the largest used zone number in area iptia  
c            
        minindz = 1  
        maxindz = 10 * areanos(num_anam+1) - 1  
        do i = 1, num_anam  
          if (pti_anum(iptia) .eq. areanos(i)) then  
            minindz = 10 * pti_anum(iptia)  
            maxindz = 10 * areanos(i+1) - 1  
          endif  
        enddo  
  
        numptiz = maxindz  
        finished = .false.  
        do while (numptiz .ge. minindz .and. .not. finished)  
          iptiz = fnd_ptiz (numptiz)  
          if (iptiz .le. 0) then  
            iptiz = add_ptiz (numptiz, zone(nb))  
            write (errbuf(1), 10042) bus(nb), base(nb),   
     &        zone(nb), numptiz  
10042       format (' Bus (', a8, f7.1, ') zone (', a,   
     & ') is assigned PTI Zone number (', i4, ')')  
            call prterx ('I',1)  
            finished = .true.  
          else  
            numptiz = numptiz - 1  
          endif  
        enddo  
  
        if (numptiz .lt. minindz) then  
c  
c         Set overflow zones 500-599  
c            
          minindz = 10 * areanos(num_anam+1)   
          maxindz = 10 * areanos(num_anam+1) + 99  
  
          numptiz = minindz  
          finished = .false.  
          do while (numptiz .le. maxindz .and. .not. finished)  
            iptiz = fnd_ptiz (numptiz)  
            if (iptiz .le. 0) then  
              iptiz = add_ptiz (numptiz, zone(nb))  
              write (errbuf(1), 10042) bus(nb), base(nb),   
     &          zone(nb), numptiz  
              call prterx ('I',1)  
              finished = .true.  
            else  
              numptiz = numptiz + 1  
            endif  
          enddo  
        endif  
  
        if (numptiz .lt. minindz) then  
          write (errbuf(1), 10043) bus(nb), base(nb),   
     &      zone(nb)  
10043     format (' Bus (', a8, f7.1,   
     &      ') zone (', a, ') could not be assigned a PTI zone number')  
          call prterx ('E',1)  
          error = 1  
          go to 900  
        endif            
        count_newzone = count_newzone + 1  
        newzoneno(count_newzone) = iptiz  
      endif  
c  
c     Get new PTI bus number  
c  
      iptib = fnd_ptib (bus(nb), base(nb), arcnam(ia))  
      if (iptib .le. 0) then  
c  
c       If iptib <= 0, the bus is not included the PTI   
C       hashed bus data.  
C  
        pass = 1  
        do while (pass .le. 2)  
          minindb = 1  
          maxindb = 1000 * areanos(num_anam+1) - 1  
          if (pass .eq. 1) then  
            do i = 1, num_anam  
              if (pti_anum(iptia) .eq. areanos(i)) then  
                minindb = 1000 * pti_anum(iptia)  
                maxindb = 1000 * areanos(i+1) - 1  
              endif  
            enddo  
          endif  
c  
c         If a pseudobus, search for the largest unused number in   
c         zone indz.  Otherwise, search for the smallest unused  
c         number in zone indx.  
c  
          if (bus(nb)(7:7) .eq. '&') then  
            numptib = maxindb  
          else  
            numptib = minindb  
          endif  
          finished = .false.  
          do while (numptib .ge. minindb .and.   
     &              numptib .le. maxindb .and. .not. finished)  
            iptib = fnd_ptin (numptib)  
            if (iptib .le. 0) then  
              iptib = add_ptin (numptib)  
c                write (errbuf(1), 10044) bus(nb), base(nb),   
c       &                                 arcnam(ia), numptib  
c  10044         format (' Bus (', a8, f7.1, ') area (', a,   
c       &          ') is assigned PTI bus number (', i5, ')')  
c                call prterx ('I',1)  
              finished = .true.  
              pass = 3  
              if (iptib .gt. 0) then  
                pti_name(iptib) = bus(nb)  
                pti_base(iptib) = base(nb)  
                pti_zone(iptib) = pti_znum(iptiz)  
                pti_area(iptib) = pti_anum(iptia)  
              endif  
              num = add_ptib (bus(nb), base(nb),   
     &                        arcnam(ia), iptib)        
            else  
              if (bus(nb)(7:7) .eq. '&') then  
                numptib = numptib - 1  
              else  
                numptib = numptib + 1  
              endif  
            endif  
          enddo  
          if (numptib .lt. minindb .or. numptib .gt. maxindb) then  
            if (pass .eq. 1) then  
              pass = 2  
            else if (pass .eq. 2) then  
              write (errbuf(1), 10045) bus(nb), base(nb)  
10045         format (' Bus (', a8, f7.1,   
     &            ') could not be assigned a viable PTI number')  
              call prterx ('E',1)  
              error = 1  
              go to 900  
            endif  
          else  
            pass = 3  
          endif  
        enddo  
        count_newbus = count_newbus + 1  
        newbusno(count_newbus) = iptib  
      endif  
      if (iptib .gt. 0) then
        h = mod (pti_num(iptib), PTI_HASHSIZE) + 1
        p = hash_verify(h)
        found = .false.
        do while (p .gt. 0 .and. .not. found)
          if (ptinum_verify(p) .eq. pti_num(iptib)) then
            found = .true.
          else
            p = next_verify(p)
          endif
        enddo
        if (p .eq. 0) then
          num_verify = num_verify + 1
          p = num_verify
          next_verify(p) = hash_verify(h)
          hash_verify(h) = p
          ptinum_verify(p) = pti_num(iptib)
          bpanum_verify(p) = nb
        else if (bpanum_verify(p) .ne. nb) then
          nbx = bpanum_verify(p)
          write (errbuf(1), 10050) pti_num(iptib), bus(nb), base(nb),
     &      bus(nbx), base(nbx)  
10050     format (' Duplicate buses assigned same bus number (', i5, 
     &      ') in Translation File: (', a8, f7.1, ') and (', a8, f7.1,
     &      ')')
          call prterx ('W',1)  
        endif
      endif
  
  900 gtptinum = error   ! return status (0/1) = (successful/failure)  
  
      return  
      end  
