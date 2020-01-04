C    @(#)outage_lst.f	20.4 2/13/96
      subroutine outage_lst(in_buffer, out_buffer)
      character text * 120, in_buffer*(*), out_buffer*(*)

      character null*1, linefeed*1

******************** merge coord and base stuff **************
      integer error, first          
******************** merge coord and base stuff **************

      save

      null = char(0)
      linefeed = char(10)
      out_buffer(1:1) = null
c
c     Initialize program variables
c
      error = 0
      inpfil = 21
      first = 1
      text = 'simulated_pf_data.dat'
      last = lastch(text)
      close(unit = inpfil, err = 100)
  100 continue
      open(unit=inpfil, file=text(1:last), status='old',
     1       form='formatted', err=120, iostat = error)
      rewind inpfil
*********************** debug stuff *******************
*     print 111
* 111 format ('Simulated outage data used for this plot ')
*********************** debug stuff *******************
      go to 130
  120 continue
*********************** debug stuff *******************
* 120 print  121 
* 121 format ('Simulated outage data **NOT** used for this plot ')
*********************** debug stuff *******************
      go to 150

  130 read (inpfil, 131, end=150) text(1:80)
  131 format(a80)
      text(81:) = linefeed
      last = first + 80 
      out_buffer(first:last) = text(1:81)
      first = last + 1
      go to 130
  150 out_buffer(first:) = null 
      return
****************** find if a parallel branch is still in service ********* 
      entry service_query(in_buffer, out_buffer)
      rewind inpfil
          out_buffer(1:1) = '0' 
          out_buffer(2:2) = linefeed 
          out_buffer(3:3) = null 
**************** debug stuff ********************* 
*     print 9901, in_buffer(1:50)
*9901 format('in service_query, in_buffer = :',a)
**************** debug stuff ********************* 
  200 read (inpfil, 201, end = 210) text(1:40)
  201 format(a)
**************** debug stuff ********************* 
*     print 9911, text(1:50)
*9911 format('in service_query, text = :',a)
**************** debug stuff ********************* 
      if (in_buffer(7:31) .eq. text(7:31) .and. text(3:3) .eq. '1')then
          out_buffer(1:1) = '1' 
          go to 210
      else
          go to 200
      endif
  210 return

      end

