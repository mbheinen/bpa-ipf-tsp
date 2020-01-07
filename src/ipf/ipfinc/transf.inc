C
      parameter (MXTRSF = 100)
      common /transf/ numt, numl, numf, fdata(22,MXTRSF),
     1                tdata(8,MXTRSF), ldata(14,MXTRSF),
     2                fymtrx(2,MXTRSF),lymtrx(2,MXTRSF)
      real ldata
      complex fymtrx, lymtrx
      integer kfdata(22,MXTRSF), ktdata(8,MXTRSF), kldata(14,MXTRSF)
      equivalence (fdata,kfdata), (tdata,ktdata), (ldata,kldata)
C
