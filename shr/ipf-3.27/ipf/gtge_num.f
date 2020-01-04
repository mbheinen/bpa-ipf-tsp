C    @(#)gtge_num.f	20.12 5/3/00
C****************************************************************  
C  
C     File: gtge_num.f  
C  
C     Purpose: Routine to obtain DEI area, zone, and bus numbers  
C  
C     Input parameters:  
C  
C             nb       - bus external number  
C             nptia    - GE area index  
C             nptiz    - GE zone index  
C             nptib    - GE bus index  
C  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: saveptid.f  
C  
C****************************************************************  
      integer function gtge_num (nb, iptia, iptiz, iptib)  
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
     &        sort_tempc
      character tempc*80
  
      common /bpa_num / user_rule, num_area_rule, num_zone_rule, 
     &                  num_owner_rule, num_default_rule, 
     &                  area_rule(3,MAXCAR), zone_rule(3,MAXZON), 
     &                  owner_rule(3,MAXOWN), default_rule(2,100), 
     &                  owner_code_ge(MAXOWN)
      integer user_rule, num_area_rule, num_zone_rule, num_owner_rule,
     &        num_default_rule, area_rule, zone_rule, owner_rule, 
     &        default_rule
      character owner_code_ge*4

      common /verify_pti/ num_verify, hash_verify(PTI_HASHSIZE), 
     &                    next_verify(PTI_MAXBUS), 
     &                    ptinum_verify(PTI_MAXBUS),
     &                    bpanum_verify(PTI_MAXBUS)
      integer num_verify, hash_verify, next_verify, ptinum_verify,
     &                    bpanum_verify

      logical finished, build_area, found
      integer fnd_ptib, fnd_ptiy, error, add_ptin, fnd_ptia, fnd_ptin, 
     &        fnd_ptiz, fnd_ptiq, fnd_ptio, h, p, areanos(0:MAXCAR), 
     &        rule, add_ptiz, add_ptio
      character tempc2*10  
      external kmp_ptiz, swp_ptiz  
  
      data build_area / .false. /  
  
      save build_area, areanos  
  
      error = 0           ! initialize error count  
      iptia = 0  
      iptiz = 0  
      iptib = 0  
  
      if (.not. build_area) then  
        do i = 1, ntot
          bus_number(i) = 0
        enddo
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
c     Get GE area number  
c  
      ia = jarzn(nb)  
      tempc2 = arcnam(ia)
      iptia = fnd_ptia (tempc2)  
      if (iptia .le. 0) then  
        write (errbuf(1), 10041) bus(nb), base(nb),   
     &    arcnam(jarzn(nb))  
10041   format (' Bus (', a8, f7.1,   
     &          ') has a non-existant GE area (', a, ')')  
        call prterx ('E',1)  
        error = 1  
        go to 900  
      endif  
c  
c     Get default GE zone number  
c  
      iptiz = fnd_ptiy (zone(nb))  
      if (iptiz .eq. 0) then
        last = 0
        do i = 1, num_znam
          last = max0 (pti_znum(i), last)
        enddo
        iptiz = add_ptiz (zone(nb), last+1)
      endif
      iptiz_def = iptiz
c  
c     Get default GE owner number  
c  
      iptio = fnd_ptiq (owner(nb))  
      if (iptio .eq. 0) then
        last = 0
        do i = 1, num_onam
          last = max0 (pti_onum(i), last)
        enddo
        iptio = add_ptio (last+1, owner(nb))
      endif
      iptio_def = iptio
c  
c     Get new GE bus number  
c  
      if (bus_number(nb) .eq. 0) then
        iptib = fnd_ptib (bus(nb), base(nb), arcnam(ia))  
        if (iptib .gt. 0) then
          iptiz = fnd_ptiz (pti_zone(iptib))
          if (iptiz .eq. 0) then
            write (errbuf(1), 10000) pti_zone(iptib), bus(nb), base(nb)
10000       format (' Zone number (', i4, 
     &        ') in translation file for bus (', a8, f7.1, 
     &        ') is missing a corresponding name')  
            call prterx ('W',1)  
            iptiz = iptiz_def
          endif
          iptio = fnd_ptio (pti_owner(iptib))
          if (iptio .eq. 0) then
            write (errbuf(1), 10010) pti_owner(iptib), bus(nb), base(nb)
10010       format (' Owner number (', i4, 
     &        ') in translation file for bus (', a8, f7.1, 
     &        ') is missing a corresponding name')  
            call prterx ('W',1)  
            iptio = iptio_def
          endif
        endif
      else
        numptib = bus_number(nb)
        iptib = fnd_ptin (numptib)  
        iptiz = fnd_ptiz (pti_zone(iptib))
        iptio = fnd_ptio (pti_owner(iptib))
      endif
      if (iptib .le. 0) then  
        rule = 1               ! Step through all rules
        if (rule .eq. 1) then
          indx = 0
          do i = 1, num_area_rule
            if (area_rule(1,i) .eq. pti_anum(iptia)) indx = i
          enddo
          if (indx .gt. 0) then
            finished = .false.
            numptib = area_rule(2,indx)
            do while (.not. finished)
              iptib = fnd_ptin (numptib)  
              if (iptib .le. 0) then  
                area_rule(2,indx) = numptib + 1
                iptib = add_ptin (numptib)  
                pti_name(iptib) = bus(nb)  
                pti_base(iptib) = base(nb)  
                pti_zone(iptib) = pti_znum(iptiz)  
                pti_area(iptib) = pti_anum(iptia)  
                pti_owner(iptib) = pti_onum(iptio)  
                bus_number(nb) = numptib
                finished = .true.  
              else
                numptib = numptib + 1
                finished = (numptib .gt. area_rule(3,indx))
              endif
            enddo
          endif
        endif
        if (iptib .eq. 0 .and. rule .eq. 1) rule = 2
        if (iptib .eq. 0 .and. rule .eq. 2) then
          indx = 0
          do i = 1, num_zone_rule
            if (zone_rule(1,i) .eq. pti_znum(iptiz)) indx = i
          enddo
          if (indx .gt. 0) then
            finished = .false.
            numptib = zone_rule(2,indx)
            do while (.not. finished)
              iptib = fnd_ptin (numptib)  
              if (iptib .le. 0) then  
                zone_rule(2,indx) = numptib + 1
                iptib = add_ptin (numptib)  
                pti_name(iptib) = bus(nb)  
                pti_base(iptib) = base(nb)  
                pti_zone(iptib) = pti_znum(iptiz)  
                pti_area(iptib) = pti_anum(iptia)  
                pti_owner(iptib) = pti_onum(iptio)  
                bus_number(nb) = numptib
                finished = .true.  
              else
                numptib = numptib + 1
                finished = (numptib .gt. zone_rule(3,indx))
              endif
            enddo
          endif
        endif
        if (iptib .eq. 0 .and. rule .eq. 2) rule = 3
        if (iptib .eq. 0 .and. rule .eq. 3) then
          if (iptio .gt. 0) then  
            indx = 0
            do i = 1, num_owner_rule
              if (owner_rule(1,i) .eq. pti_onum(iptio)) indx = i
            enddo
          else
            indx = 0
          endif
          if (indx .gt. 0) then
            finished = .false.
            numptib = owner_rule(2,indx)
            do while (.not. finished)
              iptib = fnd_ptin (numptib)  
              if (iptib .le. 0) then  
                owner_rule(2,indx) = numptib + 1
                iptib = add_ptin (numptib)  
                pti_name(iptib) = bus(nb)  
                pti_base(iptib) = base(nb)  
                pti_zone(iptib) = pti_znum(iptiz)  
                pti_area(iptib) = pti_anum(iptia)  
                pti_owner(iptib) = pti_onum(iptio)  
                bus_number(nb) = numptib
                finished = .true.  
              else
                numptib = numptib + 1
                finished = (numptib .gt. owner_rule(3,indx))
              endif
            enddo
          endif
        endif
        if (iptib .eq. 0 .and. rule .eq. 3) rule = 4
        if (iptib .eq. 0 .and. rule .eq. 4) then
          indx = 0
          if (num_default_rule .gt. 0) indx = 1
          if (indx .gt. 0) then
            finished = .false.
            numptib = default_rule(1,indx)
            do while (.not. finished)
              iptib = fnd_ptin (numptib)  
              if (iptib .le. 0) then  
                default_rule(1,indx) = numptib + 1
                iptib = add_ptin (numptib)  
                pti_name(iptib) = bus(nb)  
                pti_base(iptib) = base(nb)  
                pti_zone(iptib) = pti_znum(iptiz)  
                pti_area(iptib) = pti_anum(iptia)  
                pti_owner(iptib) = pti_onum(iptio)  
                bus_number(nb) = numptib
                finished = .true.  
              else
                numptib = numptib + 1
                finished = (numptib .gt. default_rule(2,indx))
              endif
            enddo
          endif
        endif
        if (iptib .le. 0) then  
          write (errbuf(1), 10045) bus(nb), base(nb)  
10045     format (' Bus (', a8, f7.1,   
     &        ') could not be assigned a viable GE number')  
          call prterx ('E',1)  
          error = 1  
          go to 900  
        endif
c
c       Store bus number.  Future calls will use this shortcut.
c
        bus_number(nb) = pti_num(iptib)

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
10050     format (' Duplicate number (', i5, 
     &      ') shared by two buses (', a8, f7.1, ') and (', a8, f7.1,
     &      ')')
          call prterx ('W',1)  
        endif
      endif  
  
  900 gtge_num = error   ! return status (0/1) = (successful/failure)  
  
      return  
      end  
