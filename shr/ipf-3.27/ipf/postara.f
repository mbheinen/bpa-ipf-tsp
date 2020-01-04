C    @(#)postara.f	20.3 2/13/96
      subroutine postara(record)

      include 'ipfinc/postdta.inc'

      character record*(*)
      character busnm*10, name*10, arashap*2 

*************** debug stuff *******************      
*     print 9951
*9951 format(' entering postara')
*     print 9901, record(1:100)
*9901 format(' in postara:'a)
*     print 9901, text
*     print 9901, fmtnam
*************** debug stuff *******************      

      namary = ' '
      araary = ' '
      corary = ' '

***** read area record from coordinate file *************
      read (text,111) kdspflg, name, xb, yb, arashap, arasz1, arasz2
  111 format(bz, 1x, i1, t14, a10, t24, 2f6.2, a2, 2f4.2)
******** read bus record from powerflow file *************
      read (record, 121) pgen, pload, ploss, sint
  121 format(t14, 4e15.7)

      write (corary, 201) xb, yb, arashap, arasz1, arasz2, kdspflg 
  201 format('[ ', 2(f6.2, ' cmtr '), ' (',a2, ') ', 2(f7.2, ' cmtr ')
     1, i3, ' ]')

      write (namary, 211) name
  211 format('(', a10, ')') 
      write (araary, 221) pload, ploss, pgen, sint 
  221 format('[ (LOAD', f8.1, ')', ' (LOSS', f8.1, ')',
     1' (GEN', f9.1, ')',' (SI', f10.1, ') ] areaBub')
      return
      end

