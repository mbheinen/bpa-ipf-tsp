C    %W% %G%
	real function check_tc (tc, tlim, num, type, machname, machbase, 
     &                          machid)
        character *(*) type, machid, machname
        real tc, tlim, machbase
        integer num

        include 'tspinc/params.inc'
        include 'tspinc/param.inc'
        include 'tspinc/prt.inc'
        
        check_tc = tc
        if (tc .lt. tlim .and. tc .gt. 0.0) then
           write (errbuf(1), 100) machname, machbase, machid, type, 
     &                           num, tc
  100      format (3x, a8, 2x, f6.1, 2x, a1, 5x, a, 
     &        ' time constant ', i2, e12.3, ' is set to zero ')
           call prterr ('W', 1)
        endif
        if (check_tc .lt. tlim) then
           check_tc = 0.0
        endif
        return
        end
