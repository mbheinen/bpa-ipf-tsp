C    %W% %G%
C****************************************************************
C
C     File: clnup_ge.f
C
C     Purpose: Routine to complete PTI data processing from raw data 
C              file.
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      subroutine clnup_ge (error)
      integer error

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/arcntl.inc' 
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/alpha.inc' 
      include 'ipfinc/qsdup.inc' 
      include 'ipfinc/qksrt.inc' 
      include 'ipfinc/alt_case.inc' 
      include 'ipfinc/owner_cm.inc' 
      include 'ipfinc/tx_misc.inc'

      common /is_batch / is_batch

      integer MAXWYEDELTA
      parameter (MAXWYEDELTA = 500)
      common /wye_delta/ num_wye_delta, lwye_delta(3,MAXWYEDELTA),
     &                   wye_delta(MAXWYEDELTA)

      integer MAXPTIRECORDS
      parameter (MAXPTIRECORDS = 16000)
      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                 nextptr_2(MAXBUS), count_newbus, 
     &                 newbusno(MAXBUS), count_newzone, 
     &                 newzoneno(MAXCZN), count_newown, 
     &                 newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                 sort_tempc(MAXPTIRECORDS), txflag(MAXBUS),
     &                 txindex(2,MAXWYEDELTA), procnode(MAXBUS),
     &                 vangle(MAXBUS), itemp(MAXBUS)
      integer array, count, htable_2, count_newbus, newbusno, 
     &        count_newzone, newzoneno, count_newown, newownno, 
     &        sort_tempc, txflag, txindex, procnode
      character tempc*80

      character zn*2, xbuf*120, bus_name*8, id*2

      integer HASHSIZE
      parameter (HASHSIZE = MAXCZN - 1)

      external kmpzone, swpzon, kmparc, swparc, kmpari, 
     &         swpari, kmp_ptiz, swp_ptiz, kmp_wydt, swp_wydt
      integer find_zon, find_ara, fnd_ptiz, fnd_ptib, fnd_ptic, 
     &        status, rename_bus, rebldzon, h, p, pold, 
     &        fnd_ptio, ptr, ftn_atoi, ptr_section(12), ptr9, ptr10, 
     &        areanos(MAXCAR), find_hash, fnd_ptian, add_ptiz,
     &        fnd_ptiy
      logical finished
      character tempc1*10, tempc2*10, cbown*3,
     &          lastid*1, oldrcd(2)*120, newrcd*120

      error = 0
c
c     Rename any duplicate names
c
      do i = 1, num_znam
        num1 = fnd_ptiz (pti_znum(i))
        num2 = fnd_ptiy (pti_znam(i))
        if (num1 .ne. i .or. num2 .ne. i) then
          status = new_znnm (tempc1(1:2))
          if (status .eq. 0) then
c
c           Hash new name into old location
c
            h = 0
            do j = 1, 2
              h = h + h + ichar (tempc1(j:j))
            end do
            h = mod (h, MAXCZN-1) + 1
            p = htable_y(h)
            do while (p .gt. 0)
              if (tempc1(1:2) .ne. pti_znam(p)) then
                p = nextptr_y(p)
              else
                p = -p
              endif
            enddo
            if (p .eq. 0) then
              p = i
              pti_znam(p) = tempc1(1:2)
              nextptr_y(p) = htable_y(h)
              htable_y(h) = p
            endif
            write (errbuf(1), 10000) pti_znum(i), pti_znam(i),
     &        pti_znum(i), tempc1, pti_znum(p),
     &        pti_znam(p)
10000       format(' Duplicate zone names: ', i3, 1x, a, ' and ',
     &         i3, 1x, a, ' First pair renamed to ', i3, 1x, a)
            call prterx ('W', 1)
          endif
        endif
      enddo
c
c     Sort pti_anum() to generate missing zone, owner, and bus numbers
c
      do i = 1, MAXCAR
        areanos(i) = 0
        array(1,i) = i
        array(2,i) = 0
      enddo
c           
c       key = sorts on the following fields:  
c             1. bus(zone)  
c             2. population  
c             3. area  
c             4. pti_znum  
c             5. pti_anum  
c             6. pti_znum, pti_anum  
c  
      key = 5
      call qiksrt (1, num_anam, kmp_ptiz, swp_ptiz)
      do i = 1, num_anam
        areanos(i) = pti_anum(array(1,i))
      enddo
      areanos(num_anam+1) = areanos(num_anam) + 10                    
c
c     Test zone-area uniqueness
c
      do i = 1, MAXBUS
         htable_2(i) = 0
         array(1,i) = 0
         array(2,i) = 0
         array(3,i) = 0
         array(4,i) = 0
         nextptr_2(i) = 0
      enddo
      count = 0
      do i = 1, num_hashn
         p = find_hash (pti_zone(i), pti_area(i))
         p = iabs (p)
         if (p .gt. 0) array(3,p) = array(3,p) + 1
      enddo

      dupsw = .false.
      key = 6                ! Set key to sort as follows:
c                            ! 1. Array(1,*) - PTI zone number
c                            ! 2. Array(3,*) - Zone-Area population
c                            ! 3. Array(2,*) - PTI area number
 
      call qiksrt (1, count, kmp_ptiz, swp_ptiz)
c
c     Rebuild zone-area hash tables
c
      do i = 1, MAXBUS
         htable_2(i) = 0
         nextptr_2(i) = 0
      enddo
      do i = 1, count
        h = mod (array(1,i), HASHSIZE) + 1
        p = htable_2(h)
        do while (p .gt. 0)         !search for existing entities
          if (array(1,i) .ne. array(1,p) .or.
     &        array(2,i) .ne. array(2,p)) then
            p = nextptr_2(p)
          else
            p = -p                   
          endif

        enddo
        if (p .eq. 0) then
          nextptr_2(i) = htable_2(h)
          htable_2(h) = i
        endif
      enddo
c
c     Find bus zone numbers missing in the zone data, create a 
c     corresponding and unique name, and insert the number-name pair 
c     into the number-name zone-hash tables.
c
      do i = 1, count
        num = fnd_ptiz (array(1,i)) ! Test if zone number exists
        if (num .eq. 0) then
          status = new_znnm (tempc1(1:2))
          if (status .eq. 0) then
            num = add_ptiz (array(1,i), tempc1(1:2))
            write (errbuf(1), 10010) array(1,i), tempc1(1:2)
10010       format(' Missing name for zone number ', i4, 
     &        ' is given name ', a)
            call prterx ('W', 1)
          endif
        endif
      enddo
    
c     Flag incompatible zone-area designations
c
      last = 1
      next = last + 1
      ndup = 0
      do while (next .le. count + 1)
        if (next .le. count .and. array(1,next) .eq. array(1,last)) 
     &    then
          ndup = ndup + 1
        else
          if (ndup .gt. 0) then
c
c           First pass - flag duplicates zones (areas are unique)
c
            do i = 1, ndup+1
              j = next - (ndup + 2 - i) 
              nptiz1 = fnd_ptiz (array(1,j))
              nptia1 = fnd_ptian (array(2,j))
              write (errbuf(1), 10020) array(1,j), pti_znam(nptiz1), 
     &          array(2,j), pti_anam(nptia1), array(3,j)
10020         format(' Duplicate zone ', i4, 1x, a, ' area ', i4, 
     &          1x, a, ' Population ', i4)
              call prterx ('W', 1)
            enddo
c
c           Second pass - rename duplicate zone of first, second, ...,
c           but not the last which is most populous.
c
            do i = 1, ndup
              j = next - (ndup + 2 - i) 
              nptiz1 = fnd_ptiz (array(1,j))
              nptia1 = fnd_ptian (array(2,j))
c
c             Create a unique zone number higher than any existing 
c             zone number
c
              numzone = 0
              do jx = 1, num_znam
                numzone = max0 (numzone, pti_znum(jx))
              enddo
              numzone = numzone + 1
c
c             Create a unique zone name
c
              status = new_znnm (tempc1(1:2))
c
c             Hash the number-name pair into the zone hash tables
c
              if (status .eq. 0) then
                num = add_ptiz (numzone, tempc1(1:2))
                if (num .gt. 0) then
                  write (errbuf(1), 10030) array(1,j), 
     &              pti_znam(nptiz1), array(2,j), pti_anam(nptia1), 
     &              pti_znum(num), pti_znam(num)
10030             format(' Zone ', i4, 1x, a, ' in area ', i4, 
     &              1x, a, ' renamed to ', i4, 1x, a)
                  call prterx ('W', 1)
                  array(4,j) = num
                endif
              endif
            enddo
            ndup = 0
          endif
          last = next
        endif
        next = next + 1
      enddo
          
      do nb = 1, ntot
        if (jarzn(nb) .eq. 0) then
          tempc1 = ' '
          numarea = 0
        else
          nptia = fnd_ptic (jarzn(nb))
          jarzn(nb) = nptia
          if (nptia .gt. 0) then
            tempc1 = pti_anam(nptia)
            numarea = pti_anum(nptia)
          else
            tempc1 = ' '
            numarea = 0
          endif
        endif
        nptib = fnd_ptib (bus(nb), base(nb), tempc1)
        if (nptib .gt. 0) then
          numzone = pti_zone(nptib)
          alf2inp(nb) = numzone             ! Temporarily hold zone num
          nptiz = fnd_ptiz (numzone)
          if (nptiz .gt. 0) then
            p = find_hash (pti_znum(nptiz), numarea)
            p = iabs (p)
            if (array(4,p) .gt. 0) then
              write (errbuf(1), 10040) bus(nb), base(nb), 
     &          pti_anam(nptia), pti_znam(nptiz), pti_znam(array(4,p))
10040         format(' Bus ', a, f6.1, ' in area ', a, 
     &          ' has duplicate zone renamed from ', a, ' to ', a)
              call prterx ('W', 1)
              pti_zone(nptib) = pti_znum(array(4,p))
              zone(nb) = pti_znam(array(4,p))
            else
              zone(nb) = pti_znam(nptiz)
            endif
          else
            write (zone(nb), '(i2.2)') numarea
          endif

          nptio = fnd_ptio (pti_owner(nptib))
          if (nptio .gt. 0) then
            write (owner(nb), fmt='(i3.3)') pti_onum(nptio)
          else
            owner(nb) = '0'
          endif
        else
          alf2inp(nb) = 0
          jarzn(nb) = 0
          write (errbuf(1), 10050) bus(nb), base(nb)
10050     format(' Bus ', a, f6.1, ' has no assigned bus number ')
          call prterx ('W', 1)
        endif
      enddo

      status = rebldzon (error)
c
c     Sort zones (ACZNAM array)
c
      dupsw = .false.    
      call qiksrt(1, nztot, kmpzone, swpzon) 
      if (dupsw) then                            
                                                 
c       Flag and remove duplicate records       
                                                 
        j = 1                                   
        k = 2                                   
        do while (k .le. nztot) 
          if ( kmpzone(j,k) .lt. 0 ) then       
            j = j + 1                                            
            if (j .lt. k) call swpzon (j,k)                       
            k = k + 1                                            
          else                                                    
            write (errbuf(1), 10060) acznam(k)
10060       format('0 Duplicate zones (', a, '). Case Aborted!')
            if (is_batch .eq. 0) then
              call prterx ('E',1)
            else
              call prterx ('F',1)
            endif
            k = k + 1 
            error = 1
          endif  
        enddo
      endif

c     Sort area interchange (ARCNAM array)
 
      dupsw = .false.    
      call qiksrt (1, ntotc, kmparc, swparc) 
      if (dupsw) then                            
                                                 
c       Flag and remove duplicate records       
                                                 
        j = 1                                   
        k = 2                                   
        do while (k .le. ntotc) 
          if ( kmparc(j,k) .lt. 0 ) then       
            j = j + 1                                            
            if (j .lt. k) call swparc (j,k)                       
            k = k + 1                                            
          else                                                    
            write (errbuf(1), 10070)                                
10070       format('0 Duplicate area interchange records..', 
     &             ' Case Aborted!')         
            call bcdarc (j,xbuf)                               
            write (errbuf(2), 10080) xbuf(1:80) 
10080       format(1x, '(', a, ')')                                 
            call bcdarc (k,xbuf)                               
            write (errbuf(3), 10080) xbuf(1:80) 
            if (is_batch .eq. 0) then
              call prterx ('E',3)
            else
              call prterx ('F',3)
            endif
            k = k + 1 
            error = 1
          endif  
        enddo
      endif

c     Build array(*,*) of zone-area cross-reference
                            
      do nb = 1, ntot
        if (jarzn(nb) .gt. 0) then
          tempc1 = pti_anam(jarzn(nb))
        else
          tempc1 = ' '
        endif
        nptib = fnd_ptib (bus(nb), base(nb), tempc1)
        if (nptib .gt. 0) then
          array(1,nb) = find_zon (zone(nb))        
        else
          array(1,nb) = 19999
          zone(nb) = ' '
        endif
        if (jarzn(nb) .gt. 0) then
          array(2,nb) = find_ara (pti_anam(jarzn(nb))(1:8))
        else
          array(2,nb) = 19999
        endif
      enddo

      count = ntot
      dupsw = .false.
      key = 2                ! Set key to sort as follows:
c                            ! 1. zone
c                            ! 2. area

      call qiksrt (1, count, kmp_ptiz, swp_ptiz)

      finished = .false.
      do while (count .gt. 0 .and. .not. finished)
        if (array(1,count) .ne. 19999) then 
          finished = .true.
        else
          count = count - 1
        endif
      enddo

      if (dupsw) then
        j = 1                                   
        k = 2                                   
        do while (k .le. count) 
          if (array(1,j) .eq. array(1,k) .and.
     &        array(2,j) .eq. array(2,k)) then
            k = k + 1 
          else
            if (array(1,j) .eq. array(1,k) .and. 
     &          array(2,j) .lt. array(2,k)) then       
              if (array(2,j) .lt. 19999) then
                tempc1 = arcnam(array(2,j))
              else
                tempc1 = ' '
              endif
              if (array(2,k) .lt. 19999) then
                tempc2 = arcnam(array(2,k))
              else
                tempc2 = ' '
              endif
              write (errbuf(1), 10090) acznam(array(1,j)), 
     &          tempc1, tempc2
10090         format(' Zone ', a, ' belongs to areas (', a,
     &           ') and (', a, ')')
              call prterx ('W', 1)
              error = 1                            
            endif
            j = j + 1                                            
            if (j .lt. k) then
              array(1,j) = array(1,k)
              array(2,j) = array(2,k)
            endif
            k = k + 1                                            
          endif  
        enddo
        count = j
      endif

      do i = 1, nztot
        acznum(i) = 0
      enddo

      lastzone = 0
      lastarea = 0
      do i = 1, count 
        izone = array(1,i)
        iarea = array(2,i)
        if (izone .eq. lastzone) then
          if (lastarea .lt. 19999) then
            tempc1 = arcnam(lastarea)
          else
            tempc1 = ' '
          endif
          if (iarea .lt. 19999) then
            tempc2 = arcnam(iarea)
          else
            tempc2 = ' '
          endif
          write (errbuf(1), 10100) acznam(izone), tempc1, tempc2
10100     format(' Zone ', a, ' belongs to areas (', a,
     &           ') and (', a, ')')
          call prterx ('W', 1)
          error = 1                            
        else
          acznum(izone) = iarea
        endif 
        lastzone = izone
        lastarea = iarea
      enddo
                                              
      if (count .gt. nztot) then 
        write (errbuf(1), 10110) count, nztot
10110   format(' Inconsistent PTI Zone-Area matching: ',i4, 
     &          ' zone-areas encountered, ', i4, 
     &          ' zones-area specified.')
        call prterx ('W',1)               
        error = 1                            
      endif 

      do i = 1, nztot
        iarea = acznum(i)
        if (iarea .gt. 0 .and. iarea .lt. 19999) then
          j = 1
          finished = .false.
          do while (j .le. MAXCZN .and. .not. finished)
            zn = arczns(j,iarea)                                    
            if (zn .eq. '  ') then               
              arczns(j,iarea) = acznam(i)
              finished = .true.
            else
              j = j + 1
            endif
          enddo
          if (.not. finished) then
            write (errbuf(1), 10120) MAXCZN, acznam(i), arcnam(iarea)
10120       format(' More than ', i4, ' zones in assigning zone (', a, 
     &           ') to area (', a, ')')
            call prterx ('W', 1)
            error = 1                            
          endif
        endif
      enddo

      call space(1)
      write (outbuf,10130)
10130 format(t10,'AREA 1',t22,'AREA 2',t34,'SCHEDULED',t46,
     &           '   NET   ',t58,'  CONTROL BUS',t78,'ZONES(S)')
      call prtout(1)
 
      write (outbuf, 10140)
10140 format (t35,'EXPORT ',t46,' EXPORT ',t58,'NAME       BASE')
      call prtout(1)
 
      write (outbuf, 10150)
10150 format(t34,'  (MW)  ',t46,'  (MW)  ',t78,10('-- '))
      call prtout(1)

      do i=1,ntotc
         call space(1)
         call list_are (i)
      enddo
 
      outbuf= '0'
      call prtout(1)
c
c     Duplicate PTI owner data into /owner_cm/ 
c
      do i = 1, num_onam
	owner_number(i) = pti_onum(i)
        owner_code(i) = pti_onam(i)
      enddo
c
c     1. Delete buses without branches
c     2. Sort branches 
c     3. Convert bus owners from ASCII numbers to names
c     4. Remove section numbers from non-sections
C     5. Add pi-equivalent for valid sections
C     6. Consolidate excessive sections (> 10)
c     7. Change brid() for "R" records to blank
c
      do nb = 1, ntot
        if (kbsdta(16,nb) .eq. 0 .and. bus(nb) .ne. srtlst) then
c
c         Remove isolated bus from bus hash table by renaming
c
          write (errbuf(1), 10160) bus(nb), base(nb), zone(nb)
10160     format (' Bus (', a8, f7.1, 1x, a2, 
     &      ') has no branches and is deleted')
          call prterx ('W',1)

          bus_name = srtlst
          bus_base = 9999.0
          status = rename_bus (nb, bus_name, bus_base)
c
c         Flag nb as deleted
c
          do i = 1, 11
             kbsdta(i,nb) = 0
          enddo
          capcor(1,nb) = 0.0
          capcor(2,nb) = -9.0e10
        endif
      enddo

      call sortbus ()
      call srtbrnch ()

      do nb = 1, ntot
        if (kbsdta(16,nb) .eq. 0 .and. bus(nb) .ne. srtlst) then
        else
c
c         Convert bus owners from ASCII numbers to names
c
          numowner = ftn_atoi (owner(nb))
          nptio = fnd_ptio (numowner)
          if (nptio .gt. 0) then
            owner(nb) = pti_onam(nptio)
          else
            owner(nb) = ' '
          endif
c
c         Convert +bus owners from ASCII numbers to names
c
          ptr = kbsdta(15,nb)
          do while (ptr .gt. 0)
            call getchr (3, cbown, kbctbl(10,ptr))
            numowner = ftn_atoi (cbown)
            nptio = fnd_ptio (numowner)
            if (nptio .gt. 0) then
              cbown = pti_onam(nptio)
            else
              cbown = ' '
            endif
            call putchr (3, cbown, kbctbl(10,ptr))
            ptr = bctbl_nxt(ptr)
          enddo
c
c         3. Convert branch owners from ASCII numbers to names
c         4. Remove section numbers from non-sections
C         5. Add pi-equivalent for valid sections
c
          lastk2 = 0
          lastid = ' '
          numsec = 0
          ptr = kbsdta(16,nb)
          pold = 0
          do while (ptr .gt. 0)
            if (brtype(ptr) .eq. 4) then
              brid(ptr) = ' '
            else
              nbr = iabs (brnch_ptr(ptr))
              call getchr (3, cbown, kbrnch(3,nbr))
              numowner = ftn_atoi (cbown)
              nptio = fnd_ptio (numowner)
              if (nptio .gt. 0) then
                cbown = pti_onam(nptio)
              else
                cbown = ' '
              endif
              call putchr (3, cbown, kbrnch(3,nbr))
c
c             If non-zero section encountered, look ahead for additional
c             sections
c
              if (ky(ptr) .ne. lastk2 .or. brid(ptr) .ne. lastid) then
                numsec = 1
                ptr_section(numsec) = ptr
                p = brnch_nxt(ptr)
                do while (p .gt. 0 .and. ky(p) .eq. ky(ptr) .and. 
     &                    brid(p) .eq. brid(ptr)) 
                  numsec = numsec + 1
                  ptr_section(numsec) = p
                  p = brnch_nxt(p)
                enddo
                if (numsec .gt. 1) then
                  ltot2 = ltot2 + 1
                  if (nb .lt. ky(ptr)) then
                    if (ltot+1 .ge. MAXBRN) then
                      write (errbuf(1), 10170) MAXBRN
10170                 format ('More than ',i5,
     &                 ' branch records. Overflow occurred adding pi-equ
     &ivalent section for branch:')
                      write (errbuf(2), 10180) bus(kx(ptr)), 
     &                  base(kx(ptr)), bus(ky(ptr)), base(ky(ptr)),
     &                  brid(ptr), brsect(ptr)
10180                 format(11x, '(', a, f6.1, 1x, a8, f6.1, 1x, a, i2,
     &                  ')')
                      call prterx ('E',2)
                      ltot = 1
                      error = 1
                      go to 900
                    endif
                    ltot = ltot + 1
                    kbrnch(1,ltot) = 1
                    do i = 2, 18
                      kbrnch(i,ltot) = 0
                    enddo
                    nbr = ltot
                  else
                    p = numbrn (ky(ptr), kx(ptr), brid(ptr), 0)
                    if (p .le. 0) then
                      write (errbuf(1), 10190) bus(ky(ptr)), 
     &                  base(ky(ptr)), bus(kx(ptr)), base(kx(ptr)),
     &                  brid(ptr), brsect(ptr)
10190                 format ('Error renaming section (', a8, f6.1, 1x,
     &                  a8, f6.1, 1x, a1, i2, 
     &                  ') - could not locate branch')
                      call prterx ('E', 1)
                      error = 1
                      go to 900
                    else
                      nbr = -brnch_ptr(p)
                    endif
                  endif
      
c                 Link up branch.  

                  if (pold .eq. 0) then
                    kbsdta(16,nb) = ltot2
                  else
                    brnch_nxt(pold) = ltot2
                  endif                  
                  brnch_nxt(ltot2) = ptr
                  brnch_ptr(ltot2) = nbr

                  kx(ltot2) = nb
                  ky(ltot2) = ky(ptr)
                  brid(ltot2) = brid(ptr)
                  brsect(ltot2) = 0
                  nbr = iabs (brnch_ptr(ltot2))
                  brtype(ltot2) = kbrnch(1,nbr)
c
c                 if more than 9 sections, consolidate 9 and 10
c
                  if (numsec .gt. 9) then
                    if (brnch_ptr(ltot2) .gt. 0) then
                      ptr9 = ptr_section(9)
                      ptr10 = ptr_section(10)
                      call bcdbrn (ptr9, oldrcd(1), ierror) 
                      call bcdbrn (ptr10, oldrcd(2), ierror) 
                      numtemp = 2
                      status = gtpieqiv (numtemp, oldrcd, newrcd)
                      if (status .eq. 0) then
                        write (outbuf, 10200)
10200                   format (' Old sections 9 and 10:')
                        call prtout (1)
                        write (outbuf, 10210) oldrcd(1)(1:80)
10210                   format (' (', a, ')')
                        call prtout (1)
                        write (outbuf, 10210) oldrcd(2)(1:80)
                        call prtout (1)
                        write (outbuf, 10220)
10220                   format (' New equivalent section 9:')
                        call prtout (1)
                        write (outbuf, 10210) newrcd(1:80)
                        call prtout (1)
                        nbr = iabs(brnch_ptr(ptr9))
                        read (newrcd, 10230, err=900) (brnch(k,nbr),
     &                    k=5,10)
10230                   format (bz, 38x, 6f6.5)
                        kbrnch(1,nbr) = 8
                        brtype(ptr9) = kbrnch(1,nbr)
                      endif
c
c                     De-link ptr10
c
                      brnch_nxt(ptr9) = brnch_nxt(ptr10)
                      write (errbuf(1), 10240) bus(kx(ptr10)), 
     &                  base(kx(ptr10)), bus(ky(ptr10)), 
     &                  base(ky(ptr10)), brid(ptr10), brsect(ptr10)
10240                 format(' Consolidating section (', a, f6.1, 
     &                  1x, a8, f6.1, 1x, a, 1x, i2, ')')
                      call prterx ('W', 1)
                    else
c
c                     De-link ptr10
c
                      brnch_nxt(ltot2) = ptr_section(2)
                      ptr10 = ptr_section(1)
                      write (errbuf(1), 10240) bus(kx(ptr10)), 
     &                  base(kx(ptr10)), bus(ky(ptr10)), 
     &                  base(ky(ptr10)), brid(ptr10), brsect(ptr10)
                      call prterx ('W', 1)
                    endif
                  endif
                else
                  brsect(ptr) = 0
                endif
                lastk2 = ky(ptr)
                lastid = brid(ptr)
              
              endif
            endif
            pold = ptr
            ptr = brnch_nxt(ptr)
          enddo  

        endif

      enddo
c
c     Check consistency of neutral buses in 3-terminal Tx's.
c
      do i = 1, num_3term
        k1 = tx_3term(1,i)                 ! from bus
        k2 = tx_3term(2,i)                 ! to bus
        k3 = tx_3term(3,i)                 ! reg bus
        k4 = tx_3term(4,i)                 ! 3-winding pt bus
        k5 = tx_3term(5,i)                 ! tertiary bus
        id = char (int (tx_3term(6,i)))
        ptr = kbsdta(16,k4)
        do while (ptr .gt. 0)
          if (ky(ptr) .eq. k1 .or. ky(ptr) .eq. k2 .or. ky(ptr) .eq. k5)
     &       then
          else
            write (errbuf(1), 10250) bus(k4), base(k4), bus(k1), 
     &         base(k1), bus(k2), base(k2), bus(k5), base(k5)
10250       format (' Neutral bus ', a8, f6.1, ' in 3-winding Tx ',
     &              3(a8, f6.1, 1x))
            write (errbuf(2), 10260) bus(ky(ptr)), base(ky(ptr))
10260       format (' has an illegal branch to ', a8, f6.1)
            call prterx ('W', 2)
            error = 1
          endif
          ptr = brnch_nxt(ptr)
        enddo
      enddo
c
c     Correct wye-delta voltage transformation
c
      if (num_wye_delta .gt. 0) then
c
c       Create double entry entities, sort, and remove duplicates
c
        do i = 1, num_wye_delta
          lwye_delta(1,i+num_wye_delta) = lwye_delta(2,i)
          lwye_delta(2,i+num_wye_delta) = lwye_delta(1,i)
          wye_delta(i+num_wye_delta) = -wye_delta(i)
        enddo
        num_wye_delta = 2 * num_wye_delta
        call qiksrt (1, num_wye_delta, kmp_wydt, swp_wydt)
        i = 1
        do while (i .lt. num_wye_delta)
          j = i + 1
          do while (j .le. num_wye_delta)
            if (lwye_delta(1,i) .eq. lwye_delta(1,j) .and.
     &          lwye_delta(2,i) .eq. lwye_delta(2,j)) then
              do k = j, num_wye_delta - 1
                lwye_delta(1,k) = lwye_delta(1,k+1)
                lwye_delta(2,k) = lwye_delta(2,k+1)
                wye_delta(k) = wye_delta(k+1)
              enddo
              num_wye_delta = num_wye_delta - 1
            else
              j = num_wye_delta + 1
            endif
          enddo
          i = i + 1
        enddo
c
c       Initialize processing arrays
c
        do i = 1, ntot
          txflag(i) = 0
          procnode(i) = 0
          vangle(i) = 0.0
        enddo
        do i = 1, num_wye_delta
          txindex(1,i) = 0
          txindex(2,i) = 0
        enddo
        num_txflag = 0
        do i = 1, num_wye_delta
          num_txflag = num_txflag + 1
          k = lwye_delta(1,i)
          m = lwye_delta(2,i)
          next = txflag(k)
          last = 0
          do while (next .gt. 0)
            last = next
            next = txindex(2,next)
          enddo
          if (last .eq. 0) then
            txflag(k) = num_txflag 
          else
            txindex(2,last) = num_txflag 
          endif
          txindex(1,num_txflag) = i
        enddo
c
c       Propagate from highest base kV in lwye_delta array
c
        do i = 1, num_wye_delta
          k = lwye_delta(1,i)
          m = lwye_delta(2,i)
          if (procnode(k) .eq. 0) then
c
c           Flag all primary neighors (node "k") with same angle
c
            procnode(k) = i
            next = 1           ! "next" is next index to process
            last = 1           ! "last" is last available index in stack
            itemp(last) = k
            do while (next .le. last)
              nb = itemp(next) ! "nb" is kernel node
              ptr = kbsdta(16,nb)
              do while (ptr .gt. 0)
                mb = ky(ptr)
                if (procnode(mb) .eq. 0) then
c
c                 Test whether branch (nb,mb) is in wye_delta
c
                  jx = txflag(mb)
                  finished = (jx .eq. 0)
                  do while (.not. finished)
                    jy = txindex(1,jx)
                    if (lwye_delta(1,jy) .eq. mb .and. 
     &                  lwye_delta(2,jy) .eq. nb) then
                      procnode(mb) = jy
                      vangle(mb) = vangle(nb) + wye_delta(jy)
                      finished = .true.
                      last = last + 1
                      itemp(last) = mb
                    else
                      jx = txindex(2,jx)
                      finished = (jx .eq. 0)
                    endif
                  enddo
                  if (procnode(mb) .eq. 0) then
                    procnode(mb) = procnode(nb)
                    vangle(mb) = vangle(nb)
                    last = last + 1
                    itemp(last) = mb
                  endif
                endif
                ptr = brnch_nxt(ptr)
              enddo
              next = next + 1
            enddo
          endif              
          if (procnode(m) .eq. 0) then
            write (errbuf(1), 10270) bus(k), base(k), bus(m), 
     &        base(m)
10270       format (' Wye-delta TX ', a8, f6.1, 1x, a8, f6.1,
     &        ' improperly processed')
            call prterx ('W', 1)
            error = 1
          endif
        enddo
c
c       Flag non-processed buses
c
        write (outbuf, 10280)
10280	format (' The following isolated buses could not have their vol
     &tages compensated')
        call prtout (1)
        do i = 1, ntot
          if (procnode(i) .ne. 0) then
            vmag = dsqrt (e(i) ** 2 + f(i) ** 2)
            angle = datan2 (f(i), e(i)) - 0.0174532 * vangle(i)
            e(i) = vmag * cos (angle)
            f(i) = vmag * sin (angle)
          else if (kbsdta(1,i) .gt. 0) then
            write (outbuf, 10290) bus(i), base(i)
10290       format (1x, a8, f7.1)
            call prtout (1)
          endif
        enddo
c
c       Test completedness of processing wye_delta
c
        do i = 1, num_wye_delta
          k = lwye_delta(1,i)
          m = lwye_delta(2,i)
          if (procnode(k) .eq. 0) then
            write (errbuf(1), 10300) 'Primary', bus(k), base(k)
10300       format (a, ' node of wye-delta Tx not processed ',
     &        a8, f7.1)
            call prterx ('W', 1)
          endif
          if (procnode(m) .eq. 0) then
            write (errbuf(1), 10300) 'Secondary', bus(m), base(m)
            call prterx ('W', 1)
          endif
          if (procnode(k) .ne. 0 .and. procnode(m) .ne. 0 .and.
     &        abs(vangle(k)-vangle(m)-wye_delta(i)) .gt. 0.01) then
            write (errbuf(1), 10310) bus(k), base(k), bus(m), base(m),
     &        vangle(k), vangle(m), wye_delta(i)
10310       format ('Angle error in wye-delta Tx ', a8, f7.1, 1x,
     &         a8, f7.1, ' Nodal bias', 2f7.1, ' Wye-delta ', f7.1) 
            call prterx ('W', 1)
          endif
        enddo
      endif

  900 continue
      return
      end

