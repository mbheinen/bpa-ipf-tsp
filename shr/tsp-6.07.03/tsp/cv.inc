c ********** start of cv.inc *********

      real  cvin(MAXCV*CVSLOTS/2), cvout(MAXCV*CVSLOTS/2)
      integer cvindx(MAXCV), cvnum(MAXCV), cvpts(MAXCV),
     &        cvref(MAXCV), cvtot

      real inpt(HCCVS,CVSLOTS), outpt(HCCVS,CVSLOTS)
      integer totalpts(HCCVS)


      common /cvstuff/ cvin, cvout, cvindx, cvnum, cvpts, cvref,
     &                 cvtot, inpt, outpt, totalpts

c **********  end of cv.inc  *********
