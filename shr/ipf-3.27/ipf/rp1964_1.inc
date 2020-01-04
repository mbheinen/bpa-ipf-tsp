
      integer EPIIC
      parameter (EPIIC = 8000)
 
      common /rp1964_1/ jbus(EPIIC), ixcros(EPIIC), ibname(EPIIC,3),
     &                  ibnexx(EPIIC)
      integer jbus, ibname, ibnexx
      character * 4 bsname(EPIIC,3), cname(EPIIC,3)
      equivalence (ibname, bsname, cname)
 
