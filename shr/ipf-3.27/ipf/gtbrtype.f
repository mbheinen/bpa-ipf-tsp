C    @(#)gtbrtype.f	20.3 2/13/96
	integer function gtbrtype (type)
	character type * (*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/prt.inc'

        character brtype(9) * 2
        logical found

        data brtype /'E*','LM','L ','R ','T ','TP','LD','E ','RZ'/

        i = 1
        gtbrtype = 0
        found = .false.
        do while (i .le. 9 .and. .not. found)
           if (brtype(i) .eq. type) then
              found = .true.
              gtbrtype = i
           else
              i = i + 1
           endif
        enddo
        if (.not. found) then
           if (index('RP$RQ$RM$RN$RO', type) .ne. 0) then
              found = .true.
              gtbrtype = 4
           endif
        endif
        if (.not. found) then
           write (errbuf(1), 100) type
  100      format (' illegal parameter to gtbrtype (', a, ')')
           call prterx ('W', 1)
        endif
        return
        end
