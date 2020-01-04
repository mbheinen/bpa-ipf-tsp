C    @(#)calsysf.f	20.3 2/13/96
      integer function calsysf(arg1)
****************** NOTE FOR UPDATING TOKENS *************
* The size of lenarg(xx), tok(xx)*10, and syscal(xx)*40 
* must be at least  as long as tokmax /xx/  
* The indicies of lenarg(xx), tok(xx), and syscal(xx) 
* must not repeat.
* 
****************** NOTE FOR UPDATING TOKENS *************

      integer calsys, error, tokmax, lenarg(8)
      character arg1*(*), tok(8)*10, syscal(8)*40

      data tok( 1)(1: 3) , lenarg(1), syscal( 1)(1:35)
     1/'cat'       , 3, 'cat pfmaster.post YYY.post>ZZZ.ps' /
*     data tok( 2)(1: 5) , lenarg(2), syscal( 2)(1:17)
*    1/'print'     , 5, 'lpr -Pps ZZZ.ps &'                   /
      data tok( 2)(1: 5) , lenarg(2), syscal( 2)(1:12)
     1/'print'     , 5, 'lpr  ZZZ.ps &'                   /
      data tok( 3)(1: 4) , lenarg(3), syscal( 3)(1:26)
     1/'view'      , 4, '/usr/bin/dxpsview ZZZ.ps &'          /
      data tok( 4)(1: 5) , lenarg(4), syscal( 4)(1:11)
     1/'rmYYY'     , 5, 'rm YYY.post'                       /
      data tok( 5)(1: 5) , lenarg(5), syscal( 5)(1:17)
     1/'PRINT'     , 5, 'lpr -Pps ZZZ.ps &'                   /
*     data tok( 5)(1: 5) , lenarg(5), syscal( 5)(1:12)
*    1/'PRINT'     , 5, 'lpr ZZZ.ps &'                   /
      data tok( 6)(1: 4) , lenarg(6), syscal( 6)(1:26)
     1/'VIEW'      , 4, '/usr/bin/dxpsview ZZZ.ps &'          /
      data tok( 7)(1: 5) , lenarg(7), syscal( 7)(1:09)
     1/'rmZZZ'     , 5, 'rm ZZZ.ps'                         /
      data tok( 8)(1: 8) , lenarg(8), syscal( 8)(1:15)
     1/'savepost'     , 5, 'echo file saved'                   /
      data tokmax /8/

*********************** debug stuff ************************
*     print 9141, arg1
*9141 format (' in calsysf, arg1 = *',a,'*')
*********************** debug stuff ************************
      do 120 i1 = 1, tokmax
      length = lenarg(i1)
*********************** debug stuff ************************
*     print 9161, i1, length, tok(i1)
*9161 format (' in calsysf, i1, length, tok(i1) = ',2i5,'*' a,'*')
*********************** debug stuff ************************
      if (arg1(1:length) .eq. tok(i1)(1:length)) go to 130
  120 continue
      error = 999
      go to 140
  130 continue
      error =  calsys(syscal(i1))
  140 continue
*********************** debug stuff ************************
*     print 9151, error
*9151 format (' all done, error = ', i5)
*********************** debug stuff ************************
      calsysf = error
      return
      end
