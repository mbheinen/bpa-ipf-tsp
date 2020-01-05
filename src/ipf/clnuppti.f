C    @(#)clnuppti.f	20.14 2/28/00
C****************************************************************  
C  
C     File: clnuppti.f  
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
      subroutine clnuppti (options, error)  
      integer options(*), error  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/arcntl.inc'   
      include 'ipfinc/bus.inc'  
      include 'ipfinc/branch.inc'  
      include 'ipfinc/area.inc'  
      include 'ipfinc/alpha.inc'   
      include 'ipfinc/qsdup.inc'   
      include 'ipfinc/qksrt.inc'   
      include 'ipfinc/alt_case.inc'   
      include 'ipfinc/owner_cm.inc'   
  
      common /is_batch / is_batch  
  
      integer maxptirecords
      parameter (MAXPTIRECORDS = 16000)
      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),  
     &                 next_ptr2(MAXBUS), count_newbus,   
     &                 newbusno(MAXBUS), count_newzone,   
     &                 newzoneno(MAXCZN), count_newown,   
     &                 newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                 sort_tempc(MAXPTIRECORDS)
      integer array, count, htable_2, count_newbus, newbusno, 
     &        count_newzone,  newzoneno, count_newown, newownno,
     &        sort_tempc
      character tempc*80
  
      character zn*2, xbuf*120, bus_name*8  
  
      integer hashsize  
      parameter (HASHSIZE = MAXCZN - 1)  
  
      external kmpzone, swpzon, kmparc, swparc, kmpari,   
     &         swpari, kmp_ptiz, swp_ptiz  
      integer find_zon, find_ara, fnd_ptiz, fnd_ptin, find_bus,   
     &        fnd_ptib, status, rename_bus, rebldzon, findoldbus, h,   
     &        p, pold, fnd_ptic, fnd_ptiy, add_ptiz, areanos(MAXCAR)  
      logical finished, found  
      character tempzone*2, newzone*2, tempc2*10  
  
      error = 0  
c  
c     Sort pti_anum() to generate missing zone, owner, and bus numbers  
c  
      do i = 1, MAXCAR  
        areanos(i) = 0  
        array(1,i) = i  
        array(2,i) = 0  
      enddo  
c  
c     if options(9) = 1, reinitialize zone hash arrays  
c  
      if (options(9) .eq. 1) then  
  
        write (errbuf(1), 10000)  
10000   format(' All zones renamed to BPA area number ')  
        call prterx ('W', 1)  
  
        do i = 1, MAXCZN  
          htable_z(i) = 0  
          htable_y(i) = 0  
          nextptr_y(i) = 0  
        end do  
        num_znam = 0  
      endif  
  
      key = 5                ! Set key to sort as follows:  
c                            ! 1. bus(zone)  
c                            ! 2. bus(area)  
c                            ! 3. area name  
c                            ! 4. zone number  
c                            ! 5. area number  
  
      call qiksrt (1, num_anam, kmp_ptiz, swp_ptiz)  
      do i = 1, num_anam  
        areanos(i) = pti_anum(array(1,i))  
      enddo  
      areanos(num_anam+1) = areanos(num_anam) + 10                      
c  
c     Set up zone-owner hash arrays to check against inconsistent  
c     zone-area assignments.  
c  
      do i = 1, MAXBUS  
         htable_2(i) = 0  
         array(1,i) = 0  
         array(2,i) = 0  
         array(3,i) = 0  
         array(4,i) = 0  
         next_ptr2(i) = 0  
      enddo  
  
      numhashza = 0  
      do nb = 1, ntot  
        if (jarzn(nb) .eq. 0) then  
          tempc2 = ' '  
          write (errbuf(1), 10002) bus(nb), base(nb), zone(nb)  
10002     format(' Bus ', a, f6.1, ' Zone ', a,   
     &      ' is not assigned to an area')  
          call prterx ('W', 1)  
        else  
          nptia = fnd_ptic (jarzn(nb))  
          jarzn(nb) = nptia  
          if (nptia .gt. 0) then  
            tempc2 = pti_anam(nptia)  
          else  
            tempc2 = ' '  
          endif  
        endif  
        nptib = fnd_ptib (bus(nb), base(nb), tempc2)  
        if (nptib .gt. 0) then  
          numzone = pti_zone(nptib)  
          numarea = pti_area(nptib)   
          alf2inp(nb) = numzone             ! Temporarily hold zone num  
          numbpa = findoldbus (bus(nb), base(nb))  
          nptiz = fnd_ptiz (numzone)  
  
c         Temporary fix - override zones as area numbers  
  
          if (options(9) .eq. 1) then  
            write (zone(nb), '(i2.2)') numarea    
          else if (numbpa .gt. 0) then  
            zone(nb) = oldzone(numbpa)  
          else if (nptiz .gt. 0) then  
            zone(nb) = pti_znam(nptiz)  
          else  
            write (zone(nb), '(i2.2)') numarea  
          endif  
  
          h = 2*ichar(zone(nb)(1:1)) + ichar(zone(nb)(2:2))           
          h = mod (h, HASHSIZE) + 1  
          p = htable_2(h)  
          pold = 0  
          found = .false.  
          do while (p .gt. 0 .and. p .le. numhashza .and. .not. found)  
            if (zone(array(1,p)) .eq. zone(nb)) then  
              if (array(2,p) .eq. jarzn(nb)) then  
                found = .true.  
                array(3,p) = array(3,p) + 1  
              else  
                pold = p  
                p = next_ptr2(p)  
              endif  
            else  
              pold = p  
              p = next_ptr2(p)  
            endif  
          enddo  
          if (.not. found) then  
            numhashza = numhashza + 1  
            if (pold .eq. 0) then  
              htable_2(h) = numhashza  
            else  
              next_ptr2(pold) = numhashza  
              p = numhashza  
            endif  
            array(1,numhashza) = nb  
            array(2,numhashza) = jarzn(nb)  
            array(3,numhashza) = 1  
          endif  
        else  
          alf2inp(nb) = 0  
          jarzn(nb) = 0  
        endif  
      enddo  
  
      count = numhashza  
      dupsw = .false.  
      key = 1                ! Set key to sort as follows:  
c                            ! 1. bus(zone)  
c                            ! 2. bus(area)  
c                            ! 3. area name  
c                            ! 4. zone number  
c                            ! 5. area number  
   
      call qiksrt (1, count, kmp_ptiz, swp_ptiz)  
  
      do p = 1, count-1  
        nb = array(1,p)            
        if (zone(nb) .eq. zone(array(1,p+1))) then  
          write (errbuf(1), 10010) zone(nb), pti_anam(jarzn(nb)),   
     &       array(3,p), pti_anam(array(2,p+1)), array(3,p+1)   
10010     format(' Zone ', a, ' is duplicated in areas (', a,   
     &       ' pop:', i4, ') and (', a, ' pop:', i4, ')')  
          call prterx ('I', 1)  
  
          newzone(1:1) = '&'  
c  
c         Generate a zone name which is unique to the first area  
c  
          ichr = mod (jarzn(nb) + ichar('0') - 1, 128)  
          newzone(2:2) = char(ichr)  
          tempzone = zone(nb)  
  
          do i = 1, ntot  
            if (jarzn(i) .eq. jarzn(nb) .and.   
     &          zone(i) .eq. tempzone) then  
              zone(i) = newzone  
              write (errbuf(1), 10020) bus(i), base(i), tempzone,   
     &          zone(i)  
10020         format(' Zone for bus ', a, f7.1, ' renamed from ', a,   
     &          ' to ', a)  
              call prterx ('I', 1)  
  
            endif  
          enddo  
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
            write (errbuf(1), 10030) acznam(k)  
10030       format('0 Duplicate zones (', a, '). Case Aborted!')  
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
  
      do ix = 1, nztot                   
        iptiz = fnd_ptiy(acznam(ix))   
        if (iptiz .eq. 0) then  
          nptia = 0  
          j = 1  
          do while (j .le. ntot .and. nptia .eq. 0)  
            if (zone(j) .eq. acznam(ix)) then  
              nptia = jarzn(j)  
              if (nptia .eq. 0) then  
                write (errbuf(1), 10041) acznam(ix)  
10041           format (' Zone (', a,   
     &            ') has an unassigned area number')  
                call prterx ('E',1)  
                error = 1  
                iptiz = 1  
                go to 100  
              endif  
            else  
              j = j + 1  
            endif  
          enddo                
c  
c         Search for the largest used zone number in area nptia  
c            
          if (nptia .eq. 0) then  
            write (errbuf(1), 10042) acznam(ix)  
10042       format (' Zone (', a,   
     &        ') could not be assigned a PTI zone number')  
            call prterx ('E',1)  
            error = 1  
            iptiz = 1  
            go to 100  
          endif  
          minindz = 1  
          maxindz = 10 * areanos(num_anam+1) - 1  
          do i = 1, num_anam  
            if (nptia .eq. areanos(i)) then  
              minindz = 10 * pti_anum(nptia)  
              maxindz = 10 * areanos(i+1) - 1  
            endif  
          enddo  
  
          numptiz = maxindz  
          finished = .false.  
          do while (numptiz .ge. minindz .and. .not. finished)  
            iptiz = fnd_ptiz (numptiz)  
            if (iptiz .le. 0) then  
              iptiz = add_ptiz (numptiz, newzone)  
              write (errbuf(1), 10043) acznam(ix), numptiz  
10043         format (' Zone (', a,   
     &   ') is assigned PTI Zone number (', i4, ')')  
              call prterx ('I',1)  
              finished = .true.  
            else  
              numptiz = numptiz - 1  
            endif  
          enddo  
  
          if (numptiz .lt. minindz) then  
c  
c           Set overflow zones 500-599  
c            
            minindz = 10 * areanos(num_anam+1)   
            maxindz = 10 * areanos(num_anam+1) + 99  
  
            numptiz = minindz  
            finished = .false.  
            do while (numptiz .le. maxindz .and. .not. finished)  
              iptiz = fnd_ptiz (numptiz)  
              if (iptiz .le. 0) then  
                iptiz = add_ptiz (numptiz, newzone)  
                write (errbuf(1), 10043) acznam(ix), numptiz  
                call prterx ('I',1)  
   
                finished = .true.  
              else  
                numptiz = numptiz + 1  
              endif  
            enddo  
          endif  
  
          if (numptiz .lt. minindz) then  
            write (errbuf(1), 10043) acznam(ix)  
            call prterx ('E',1)  
            error = 1  
            iptiz = 1  
            go to 100  
          endif            
        endif  
  100   continue  
        zone_number(ix) = pti_znum(iptiz)        
      enddo                           
  
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
            write (errbuf(1), 10040)                                  
10040       format('0 Duplicate area interchange records..',   
     &             ' Case Aborted!')           
            call bcdarc (j,xbuf)                                 
            write (errbuf(2), 10050) xbuf(1:80)   
10050       format(1x, '(', a, ')')                                   
            call bcdarc (k,xbuf)                                 
            write (errbuf(3), 10030) xbuf(1:80)   
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
          nptia = jarzn(nb)  
          jarzn(nb) = find_ara (pti_anam(nptia)(1:8))  
          if (jarzn(nb) .eq. 0) then  
            tempc2 = ' '  
          else  
            tempc2 = arcnam(jarzn(nb))  
          endif  
          nptib = fnd_ptib (bus(nb), base(nb), tempc2)  
          if (nptib .gt. 0) then  
            array(1,nb) = find_zon (zone(nb))          
          else  
            array(1,nb) = 19999  
            zone(nb) = ' '  
          endif  
          if (jarzn(nb) .gt. 0) then  
            array(2,nb) = jarzn(nb)  
          else  
            array(2,nb) = 19999  
          endif  
        else  
          array(1,nb) = 19999  
          array(2,nb) = 19999  
        endif  
      enddo  
  
      count = ntot  
      dupsw = .false.  
  
      key = 2                ! Set key to sort as follows:  
c                            ! 1. bus(zone)  
c                            ! 2. bus(area)  
c                            ! 3. area name  
c                            ! 4. zone number  
c                            ! 5. area number  
  
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
              write (errbuf(1), 10060) acznam(array(1,j)),   
     &          arcnam(array(2,j)), arcnam(array(2,k))  
10060         format(' Zone ', a, ' belongs to areas (', a,  
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
          write (errbuf(1), 10070) acznam(izone), arcnam(iarea),   
     &       arcnam(lastarea)  
10070     format(' Zone ', a, ' belongs to areas (', a,  
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
        write (errbuf(1), 10080) count, nztot  
10080   format(' Inconsistent PTI Zone-Area matching: ',i4,   
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
            write (errbuf(1), 10090) MAXCZN, acznam(i), arcnam(iarea)  
10090       format(' More than ', i4, ' zones in assigning zone (', a,   
     &           ') to area (', a, ')')  
            call prterx ('W', 1)  
            error = 1                              
          endif  
        endif  
      enddo  
  
      call space(1)  
      write (outbuf,10100)  
10100 format(t10,'AREA 1',t22,'AREA 2',t34,'SCHEDULED',t46,  
     &           '   NET   ',t58,'  CONTROL BUS',t78,'ZONES(S)')  
      call prtout(1)  
   
      write (outbuf, 10110)  
10110 format (t35,'EXPORT ',t46,' EXPORT ',t58,'NAME       BASE')  
      call prtout(1)  
   
      write (outbuf, 10120)  
10120 format(t34,'  (MW)  ',t46,'  (MW)  ',t78,10('-- '))  
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
      num_owners = num_onam                     
c  
C     1. Convert remotely controlled buses on BG buses from PTI numbers  
c        to BPA numbers, and transfer voltage limits to remote bus.  
c     2. Delete buses without branches  
c  
      do nb = 1, ntot  
        if (kbsdta(1,nb) .eq. 8 .or. kbsdta(1,nb) .eq. 11) then  
          npti = kbsdta(13,nb)  
          if (npti .gt. 0) then  
            kp1 = fnd_ptin (npti)  
            if (kp1 .le. 0) then  
              write (errbuf(1), 10130) bus(nb), base(nb), npti  
10130         format (' BG bus (', a8, f7.1,   
     &         ') has a non-existant remote bus (', i6, ')')  
              call prterx ('W',1)  
              kbsdta(13,nb) = 0  
              error = 1  
            else  
              k1 = find_bus (pti_name(kp1), pti_base(kp1))  
              if (k1 .le. 0) then  
                write (errbuf(1), 10140) bus(nb), base(nb), npti,  
     &            pti_name(kp1), pti_base(kp1)  
10140           format (' BG bus (', a8, f7.1,   
     &            ') has a non-existant remote bus (', i6,   
     &            1x, a8, f7.1, ')')  
                call prterx ('W',1)  
                kbsdta(13,nb) = 0  
                error = 1  
              else if (k1 .ne. nb) then  
                kbsdta(13,nb) = k1  
                busdta(11,k1) = busdta(11,nb)  
                busdta(12,k1) = busdta(12,nb)  
                call vltlim (k1, vlimn(k1), vlimx(k1), vstart(k1))  
                busdta(11,nb) = 0.0  
                busdta(12,nb) = 0.0  
                call vltlim (nb, vlimn(nb), vlimx(nb), vstart(nb))  
              endif  
            endif  
          endif  
        endif  
        if (kbsdta(16,nb) .eq. 0 .and. bus(nb) .ne. srtlst) then  
c  
c         Remove bus from bus hash table by renaming  
c  
          write (errbuf(1), 10150) bus(nb), base(nb), zone(nb)  
10150     format (' Bus (', a8, f7.1, 1x, a2,   
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
c  
C     Convert arcint() arrays from 8-character PTI names to 10-character  
C     BPA names.  
c  
      do i = 1, ntotic  
        k1 = find_ara (arcint(1,i)(1:8))  
        if (k1 .gt. 0) then  
          arcint(1,i) = arcnam(k1)  
        else  
          write (errbuf(1), 10160) arcint(1,i)  
10160     format (' PTI area1 (', a10, ') is not in system.')  
          call prterx ('W',1)                  
          error = 1                             
        endif  
        k2 = find_ara (arcint(2,i)(1:8))  
        if (k2 .gt. 0) then  
          arcint(2,i) = arcnam(k2)  
        else  
          write (errbuf(1), 10170) arcint(2,i)  
10170     format (' PTI area2 (', a10, ') is not in system.')  
          call prterx ('W',1)                  
          error = 1                             
        endif  
      enddo  
  
  900 continue  
      return  
      end  
  
  
