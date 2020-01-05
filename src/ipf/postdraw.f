C    @(#)postdraw.f	20.3 2/13/96
*2345678911234567892123456789312345678941234567895123456789612345678971234567898
      subroutine postdraw(record)
      character record*(*)

      include 'ipfinc/postdta.inc'
      include 'ipfinc/pasprm.inc'

      dimension xdraw(6), ydraw(6), lpen(6)
      character mvlnto*8 

**************** debug stuff ********************
*     print 9901
*9901 format('entering  postdraw') 
**************** debug stuff ********************
  110 read (record, 111, err=900) (xdraw(i1), ydraw(i1), lpen(i1), 
     1 i1 = 1,6)
  111 format(bz, 2x, 6(2f6.2,i1))
      write (lpost, 121)
  121 format('newpath ')
      do i1 = 1,7 
           if (lpen(i1) .eq. 1) then
                mvlnto = ' lineto ' 
           elseif (lpen(i1) .eq. 2) then 
                mvlnto = ' moveto '
           else
                go to 140
           endif
           write (lpost, 131) xdraw(i1), ydraw(i1), mvlnto
  131      format(2(f7.2, ' cmtr '), a8)
      enddo
  140 write (lpost, 141)
  141 format(' stroke ')
**************** debug stuff ********************
*     print 9921
*9921 format('leaving  postdraw') 
**************** debug stuff ********************
      go to 1000
  900 print 901, record
  901 format (' Error in Draw Record: ', a)

 1000 return
      end


