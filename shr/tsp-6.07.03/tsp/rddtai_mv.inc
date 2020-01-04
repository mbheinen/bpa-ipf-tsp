c     %W% %G%
      integer kk,ktempn,ixn,indo2n,indn2o,indo2x,indx2o
      real vold1, vold2, capold
      integer jphid
      real dcdtan, dcline, dcx
      integer kdxtan, kdcx, kdclin
      dimension jphid(8,50)
      dimension indn2o(MAXBUS),indo2x(MAXBUS),indx2o(MAXBUS),
     1          indo2n(MAXBUS),
     2          ktempn(3,MAXBUS),
     3          ixn(MAXBUS),
     5 vold1(MAXBUS), vold2(MAXBUS),
     6 capold(2,MAXBUS), kk(2,MAXBUS),
     7 dcdtan(45,10), kdxtan(45,10), 
     8 kdcx(36,40), dcx(36,40), dcline(10,50), kdclin(10,50)

      common /rddtai_mvx/ indo2n, indn2o, indo2x, indx2o,
     1    vold1, vold2, capold, kk,
     2    ktempn, ixn, jphid,
     3    dcdtan, dcline, kdcx  

      equivalence (kdxtan,dcdtan)
      equivalence (dcline, kdclin)
      equivalence (dcx, kdcx)

