C    @(#)output_rpt.f	20.6 7/18/96
C****************************************************************
C
C   File: output_rpt.f
C
C   Purpose: Driver routine for various filtered output reports.
C
C   Author: Walt Powell  Date: 15 July 1994
C   Called by: ipf_reports.f
C
C****************************************************************
C
	subroutine output_rpt (scrfil)
        integer scrfil

	character query * 2, ljstfy * 2, capital * 2, null * 1, 
     1            linefeed * 1, tempc*2

        null = char(0)
        linefeed = char(10)

        query = '?'
        do while (query .ne. 'Q')
           write (*, 90)
   90      format (' * Output reports > 1) Selected bus/branches ',
     *        /,   '                    2) Undervoltage buses ',
     *        /,   '                    3) Overvoltage buses ',
     *        /,   '                    4) Under/overvoltage buses ',
     *        /,   '                    5) Transmission line loadings',
     *        /,   '                    6) Transformer loadings',
     *        /,   '                    7) LTC transformers ',
     *        /,   '                    8) Filtered bus/branches ',
     *        /,   '                    9) Area Interchange summary ',
     *        /,   '                   10) Tie Line summary ',
     *        /,   '                   11) D-C Input (not available)',
     *        /,   '                   12) D-C Output (not available)',
     *        /,   '                   13) Phase-shifter ',
     *        /,   '                   14) Network changes ',
     *        /,   '                   15) Bone pile ',
     *        /,   '                   17) User analysis ',
     *        /,   '                   18) Base case comparison ',
     *        /,   '                   19) Ownership Interchange ',
     *        /,   '                   20) Zone Interchange ',
     *        /,   '                   21) Generator summary ',
     *        /,   '                   22) Sensitivities ',
     *        /,   '                    Q) Quit ', 
     *        /,   ' > Enter selection : ',$)

           read (*, 100) query
  100      format (a)
           query = capital (query)
           query = ljstfy (query)
           if (query .eq. 'Q') go to 900
           iquery = 0
           if (query(2:2) .eq. ' ') then
              tempc = ' ' // query
              query = tempc
           endif
           read (query, '(i2)', err=110) iquery
  110      if (iquery .eq. 0 .or. iquery .eq. 1) then
              call get_otrpt1 (scrfil)
           else if (iquery .eq. 2) then
              call get_otrpt2 (scrfil)
           else if (iquery .eq. 3) then
              call get_otrpt3 (scrfil)
           else if (iquery .eq. 4) then
              call get_otrpt4 (scrfil)
           else if (iquery .eq. 5) then
              call get_otrpt5 (scrfil)
           else if (iquery .eq. 6) then
              call get_otrpt6 (scrfil)
           else if (iquery .eq. 7) then
              call get_otrpt7 (scrfil)
           else if (iquery .eq. 8) then
              call get_otrpt8 (scrfil)
           else if (iquery .eq. 9) then
              call get_otrpt9 (scrfil)
           else if (iquery .eq. 10) then
              call get_orpt10 (scrfil)
           else if (iquery .eq. 13) then
              call get_orpt13 (scrfil)
           else if (iquery .eq. 14) then
              call get_orpt14 (scrfil)
           else if (iquery .eq. 15) then
              call get_orpt15 (scrfil)
           else if (iquery .eq. 17) then
              call get_orpt17 (scrfil)
           else if (iquery .eq. 18) then
              call get_orpt18 (scrfil)
           else if (iquery .eq. 19) then
              call get_orpt19 (scrfil)
           else if (iquery .eq. 20) then
              call get_orpt20 (scrfil)
           else if (iquery .eq. 21) then
              call get_orpt21 (scrfil)
           else if (iquery .eq. 22) then
              call get_orpt22 (scrfil)
           else
              write (*, 120)
  120         format (' * Illegal value - reenter selection :',$)
           endif
        enddo

  900   continue
        return
        end
