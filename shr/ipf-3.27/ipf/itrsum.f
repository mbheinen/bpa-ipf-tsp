C    @(#)itrsum.f	20.3 2/13/96
      subroutine itrsum
C                                                                      *
C     This subroutine prints out the solution iteration summary.       *
C                                                                      *
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/errbus.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/itrhis.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tran.inc'
 
      integer firitr, lasitr
 
      do 202 ix = 1, 2
      if (itrhis(ix) .eq. 0) go to 202
      if (ix .eq. 1) then
         firitr = 1
         lasitr = itrhis(1)
         call forbtm
         write (outbuf, 90) 'Decoupled'
   90    format ('0 ',a,' solution iteration history of ',
     1           'residuals (eight buses with the largest ',
     2           'error at each iteration')
         call shdlod(1)
      else
         call forbtm
         firitr = itrhis(1) + 1
         lasitr = itrhis(2)
         write (outbuf, 90) 'Newton-Raphson'
         call shdlod(1)
      endif
      write (outbuf,100 )
  100 format (
     1   '0 Iteration Bus              Transformer          ------ Error
     2 -------  Bus               Transformer        ----- Correction --
     3---')
      call shdlod(2)
      write (outbuf,110 )
  110 format (53x, '(MW)        (MVAR)', 40x, '(radians)   (PU volts)')
      call shdlod(3)
 
      outbuf = ' '
      call shdlod(4)
      call shdlod(5)
      call fortop
 
      do 200 iter = firitr, lasitr
         if (ibuser(1,1,iter) + ibuser(5,1,iter) .eq. 0) go to 200
         do 190 i = 1, 8
            if (i .eq. 1) then
               write (outbuf,130 ) iter - firitr + 1
  130          format ('0', i5)
            else
               outbuf = ' '
            endif
            do 180 n = 1, 5, 4
               kt = ibuser(n,i,iter)
               if (kt .eq. 0) then
               else if (kt .le. ntota) then
                  k = ltran(1,kt)
                  m = ltran(9,kt)
                  if (n .eq. 1) then
                     em = buser(n+2,i,iter) * bmva
                     write (outbuf(7:),140 ) intbus(k), intbas(k),
     1                  intbus(m), intbas(m), em
  140                format (6x, a8, f7.1, 2x, a8, f7.1, f13.3)
                  else
                     em = buser(n+2,i,iter)
                     write (outbuf(73:),150 ) intbus(k), intbas(k),
     1                  intbus(m), intbas(m), em
  150                format (a8, f7.1, 2x, a8, f7.1, f13.4)
                  endif
               else
                  kt = kt - ntota
                  if (n .eq. 1) then
                     em = buser(3,i,iter) * bmva
                     fm = buser(4,i,iter) * bmva
                     write (outbuf(7:),160 ) intbus(kt), intbas(kt),
     1                  em, fm
  160                format (6x, a8, f7.1, 17x, 2f13.3)
                  else
                     em = buser(n+2,i,iter)
                     fm = buser(n+3,i,iter)
                     write (outbuf(73:),170 ) intbus(kt), intbas(kt),
     1                  em, fm
  170                format (a8, f7.1, 17x, 2f13.4)
                  endif
               endif
  180       continue
            call prtout (1)
  190    continue
  200 continue
  202 continue
 
      do 210 i = 1, 5
         outbuf = ' '
         call shdlod(i)
  210 continue
      return
      end
