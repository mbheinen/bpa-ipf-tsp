C    @(#)in1n.inc	4.3 9/21/93
C     -                    IN1N.INC
      common /in1n/ irea1n(150),jbypsn(2,63),
     1  jexnmn(maxbus),
     1  jnetn(maxgen),tempn(44),
     1  lrly1n(200),lrly2n(200),
     1  lrly3n(2,200),lrep2n(maxbus),lrod1n(100),
     1  lrod2n(100),lrod3n(2,100),lrod4n(100),
     1  lrod5n(100),lrod6n(100),lshd1n(mxlshd),lshed2(mxlshd),
     1  lsrc1n(100),lsrc2n(100),lsrc3n(2,100),
     1  izn1n(25),izn2n(25),izn3n(2,25),
     1  mach1n(4,maxmac),mach2n(maxmac),                                !dem
     1  lufrqn(300),kfreq(300),iugenn(maxgen),kgenf(maxgen),
     1  exbase(maxbus),
     1  idznen(150),iaracn(14,60),
     1  lrep1(maxbus),
     1  idcinn(2,150),idcbun(150),
     1  reappx(63,3)
C     -                 end of IN1N.INC
      integer irea1n,jbypsn,
     1  jexnmn,
     1  jnetn,
     1  lrly1n,lrly2n,
     1  lrly3n,lrep2n,lrod1n,
     1  lrod2n,lrod3n,lrod4n,
     1  lrod5n,lrod6n,lshd1n,lshed2,
     1  lsrc1n,lsrc2n,lsrc3n,
     1  izn1n,izn2n,izn3n,
     1  mach1n,mach2n,
     1  lufrqn,kfreq,iugenn,kgenf,
     1  idznen,iaracn,
     1  lrep1,
     1  idcinn,idcbun
      real tempn, reappx, exbase
