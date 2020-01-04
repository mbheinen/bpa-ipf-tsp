C    %W% %G%
      subroutine prtresid (imx, acperr, dcrepm)

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbc.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/ecstbj.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/gentblc.inc'
      include 'tspinc/dateq.inc'
      include 'tspinc/busdta.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/vrgov.inc'

      character*1 id, id1
      character*8 name, name1

      i1 = igentn(1,imx)
      id = igentc(imx)
      name = bname(i1)
      bkv = buskv(i1)
      if (imax .le. nmx) then
         id1 = 'L'
         i1=imax
      else
         imax4 = imax-nmx
         i1 = igentn(1,imax4)
         id1 = igentc(imax4)
      endif
      name1 = bname(i1)
      bkv1 = buskv(i1)
      tt = to + edt
      write (outbuf,2640) tt, name, bkv, id, dcrepm, acperr,
     1                    name1, bkv1, id1, cmax, dsum, lppwr
 2640 format(1x,f5.1,'CYCLE',3x,a8,f7.1,a1,f11.4,'DP',f11.4,'SUMDP',2x,
     1  a8,f7.1,a1,f11.4,'DI',f11.4,'SUMDI',i3,'ITER')
      call prtout (1)
      return
      end
