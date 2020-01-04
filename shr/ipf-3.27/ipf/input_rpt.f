C    @(#)input_rpt.f	20.3 2/13/96
	subroutine input_rpt (scrfil)
        integer scrfil

        character query * 1, ljstfy * 2, capital * 2, fmt * 10

        query = '?'

        do while (query .ne. ' ')
           write (*, 90)
   90      format (' Input reports > 1) Bus and Branch data for a select
     *ed bus ', /,
     *             '                 2) Filtered Bus data ', /,
     *             '                 3) Filtered Data Sanity Checks', /,
     *             '                 Q) Quit ', /,
     *             ' Enter selection : ',$)

           read (*, 100) query
  100      format (a)
           query = capital (query)
           query = ljstfy (query)
           if (query .eq. 'Q') go to 900
           iquery = 0
           read (query, '(i1)', err=110) iquery
  110      if (iquery .eq. 0 .or. iquery .eq. 1) then
              call get_inrpt3 (scrfil)
           else if (iquery .eq. 2) then
              call get_inrpt4 (scrfil)
           else if (iquery .eq. 3) then
              call get_inrpt5 (scrfil)
           else
              write (*, 120)
  120         format (' * Illegal value - reenter selection :',$)
           endif
        enddo

  900   continue
        return
        end
