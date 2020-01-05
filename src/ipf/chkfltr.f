C    %W% %G%
        logical function chkfltr (area, zone, owner, basekv, type, 
     &                            bus_num)
        integer bus_num
        character area*10, zone*2, owner*3, type*2
        real basekv
c
c       This function checks if bus nb passes the filters
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/sortuvov.inc'

	external ownsch, find_zon
        integer  ownsch, find_zon

        integer f1, f2, f3, f4, f5, f6
        character wildtype*2
        logical accept(6)
c
c       Apply filters
c
        do j = 1, 6
           if (filter(j) .eq. 0) then
               accept(j) = .true.
           else
               accept(j) = .false.
           endif
        enddo

        if (area .eq. '**********') then
           if (filter(1) .gt. 0) accept(1) = .true.
        else
           f1 = 1
           do while (f1 .le. filter(1) .and. .not. accept(1))
              if (area_filter(f1) .eq. area) accept(1) = .true.
              f1 = f1 + 1
           enddo
        endif

        if (zone .eq. '**') then
           if (filter(2) .gt. 0) accept(2) = .true.
        else
           f2 = 1
           do while (f2 .le. filter(2) .and. .not. accept(2))
              if (zone_filter(f2) .eq. zone) accept(2) = .true.
              f2 = f2 + 1
           enddo
        endif

        if (owner .eq. '***') then
           if (filter(3) .gt. 0) accept(3) = .true.
        else
           f3 = 1
           do while (f3 .le. filter(3) .and. .not. accept(3))
              if (owner_filter(f3) .eq. owner) accept(3) = .true.
              f3 = f3 + 1
           enddo
        endif

        if (basekv .eq. 0.0) then
           if (filter(4) .gt. 0) accept(4) = .true.
        else
           f4 = 1
           do while (f4 .le. filter(4) .and. .not. accept(4))
              if (voltage_filter(1,f4) .gt. 0.0 .and. 
     &            voltage_filter(2,f4) .gt. 0.0) then
                if (basekv .ge. voltage_filter(1,f4) .and.
     &              basekv .le. voltage_filter(2,f4)) accept(4) = .true.
              else 
                if (voltage_filter(1,f4) .eq. basekv) accept(4) = .true.
              endif
              f4 = f4 + 1
           enddo
        endif

        if (type .eq. '**') then
           if (filter(5) .gt. 0) accept(5) = .true.
        else
           f5 = 1
           do while (f5 .le. filter(5) .and. .not. accept(5))
              wildtype = type
              if (wildtype(1:1) .eq. '*') then
                 wildtype = type_filter(f5)
              else if (wildtype .eq. 'A*' .and.
     &                (type_filter(f5)(1:1) .eq. '*' .or.
     &                 type_filter(f5)(1:1) .eq. 'A' .or.
     &                 type_filter(f5)(1:1) .eq. 'I')) then
                 wildtype = type_filter(f5)
              else if (wildtype .eq. 'B*' .and.
     &                 index ('B+XQLRET', type_filter(f5)(1:1)) .ne. 0)
     &            then
                 wildtype = type_filter(f5)
              else if (wildtype .eq. 'B?' .and.
     &                 type_filter(f5)(1:1) .eq. 'B') then
                 wildtype = type_filter(f5)
              else if (wildtype .eq. 'L*' .and.
     &                 index ('LRET', type_filter(f5)(1:1)) .ne. 0)
     &            then
                 wildtype = type_filter(f5)
              else if (type_filter(f5)(1:1) .eq. '*') then
                 wildtype = type_filter(f5)
              else if (type_filter(f5) .eq. 'A*' .and.
     &                (wildtype(1:1) .eq. '*' .or.
     &                 wildtype(1:1) .eq. 'A' .or.
     &                 wildtype(1:1) .eq. 'I')) then
                 wildtype = type_filter(f5)
              else if (type_filter(f5) .eq. 'B*' .and.
     &                 index ('B+XQLRET', wildtype(1:1)) .ne. 0)
     &            then
                 wildtype = type_filter(f5)
              else if (type_filter(f5) .eq. 'B?' .and.
     &                 wildtype(1:1) .eq. 'B') then
                 wildtype = type_filter(f5)
              else if (wildtype .eq. '+*' .and.
     &                 type_filter(f5)(1:1) .eq. '+') then
                 wildtype = type_filter(f5)
              else if (type_filter(f5) .eq. 'L*' .and.
     &                 index ('LRET', wildtype(1:1)) .ne. 0)
     &            then
                 wildtype = type_filter(f5)
              else if ((type_filter(f5)(2:2) .eq. '*' .or.
     &                  wildtype(2:2) .eq. '*') .and.
     &                  type_filter(f5)(1:1) .eq. wildtype(1:1))
     &            then
                 wildtype = type_filter(f5)
              endif
              if (type_filter(f5)(1:2) .eq. wildtype) then
                 accept(5) = .true.
              endif
              f5 = f5 + 1
           enddo
        endif

        if (bus_num .eq. 0) then
           if (filter(6) .gt. 0) accept(6) = .true.
        else
           f6 = 1
           do while (f6 .le. filter(6) .and. .not. accept(6))
              if (bus_filter(f6) .eq. bus_num) accept(6) = .true.
              f6 = f6 + 1
           enddo
        endif

        chkfltr = accept(1) .and. accept(2) .and. accept(3) .and.
     &            accept(4) .and. accept(5) .and. accept(6)
        return
        end
