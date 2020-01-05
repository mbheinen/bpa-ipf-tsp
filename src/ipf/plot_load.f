C    @(#)plot_load.f	20.4 1/7/99
c********************************************************************
c file: plot_load.f
c purpose: This is basically the "main" routine for the "ipfplot"
c          The "C" routines are just to get the command line args
c********************************************************************

        subroutine plot_load( coord_file, base1_file, base2_file )
        character * 60  coord_file, base1_file, base2_file

        include 'ipfinc/parametr.inc'
        include 'ipfinc/lfiles.inc'

        common /is_batch / is_batch

        character * 1   null, lf
        character  inbuf * (MAXBUFFER), outbuf * (MAXBUFFER+200),
     &     initcmd * 30, bas1cmd * 100, bas2cmd *100, plotcmd * 200
        character  crd_filnam * 60, crd_filext * 20
        character  bse_filnam * 60, bse_filext * 20
        character  text * 120
        integer    p_pfinit, p_gtbase, p_gtdata, p_plot

        null = char(0)
        lf = char(10)
        lco = lastch0( coord_file )
        lb1 = lastch0( base1_file )
        lb2 = lastch0( base2_file )

        if ( lco .eq. 0 ) then
           print *, ' No coordinate file specified - exiting'
           call exit
        endif

        call prs_filnam( crd_filnam, crd_filext, coord_file(1:lco) )
        if ( crd_filnam .eq. ' ' ) then
           print *, ' Invalid coordinate file specified - exiting'
           write(*,'(a,a)') ' coordinate file = ', coord_file
           call exit
        endif
        lcrd = lastch( crd_filnam )

        if ( lb1 .ne. 0 ) then
           call prs_filnam( bse_filnam, bse_filext, base1_file(1:lb1) )
           if ( bse_filnam .eq. ' ' ) then
              print *, ' Invalid base case1 file specified - exiting'
              write(*,'(a,a)') ' base case1 file = ', base1_file
              call exit
           endif
           lbse = lastch( bse_filnam )
        endif

        call initlz( istatus )
        call pfinit
        is_batch = 1           ! 0 = interactive, 1 = batch

        outbuf(1:2) = lf // null

        initcmd = '/initialize'                  // lf //
     &            '(END)'                        // null

        if ( lb1 .eq. 0 ) then

           i = lastch0( initcmd )
           inbuf(1:i) = initcmd(1:i) // null
           call cpyinbfil( inbuf, inp )
           istatus = p_pfinit( inbuf, outbuf(2:) )
           if ( istatus .ne. 0 ) goto 900

           plotcmd = '/plot'                            // lf //
     &               coord_file(1:lco)                  // lf //
     &               crd_filnam(1:lcrd) // '.ps'        // lf //
     &               '(END)'                            // null

        else

           plotcmd = '/plot'                            // lf //
     &               coord_file(1:lco)                  // lf //
     &               crd_filnam(1:lcrd) // '_' //
     &               bse_filnam(1:lbse) // '.ps'        // lf //
     &               '(END)'                            // null

           bas1cmd = '/oldbase,file=' //  base1_file(1:lb1)  // lf //
     &               '(END)'                                 // null

           i = lastch0( bas1cmd )
           inbuf(1:i) = bas1cmd(1:i) // null
           call cpyinbfil( inbuf, inp )
           istatus = p_gtbase( inbuf, outbuf(2:) )
           if ( istatus .ne. 0 ) goto 900

           if ( lb2 .ne. 0 ) then
              bas2cmd = '/getdata,type=load_ref_base,file='        //
     &                            base2_file(1:lb2)          // lf //
     &                  '(END)'                              // null
              i = lastch0( bas2cmd )
              inbuf(1:i) = bas2cmd(1:i) // null
              call cpyinbfil( inbuf, inp )
              istatus = p_gtdata( inbuf, outbuf(2:) )
              if ( istatus .ne. 0 ) goto 900
           endif
        endif

        i = lastch0( plotcmd )
        inbuf(1:i) = plotcmd(1:i) // null
        call cpyinbfil( inbuf, inp )
        istatus = p_plot( inbuf, outbuf(2:) )
        if ( istatus .ne. 0 ) goto 900

        call ipf_fexit

  900   i = lastch0( outbuf )
        j = index( outbuf, lf // ' ***' )
        if ( j .gt. 0  .and.  j .lt. i ) then
           print *, ' '
           print *, ' IPFPLOT Error Messasges'
           print *, ' '
           ix = 0
           istat = init_bufln( outbuf(j:), ix, text, lc )
           do while ( istat .eq. 0 )
              istat = nxt_bufln( outbuf(j:), ix, text, lc )
              write ( *, '(a)' ) text(1:lc)
           enddo
        endif

        call ipf_fexit
        end
