C
       parameter (MXYCMP = 20)
       common /ycomp/ nycomp, ycomp(48,MXYCMP)
       dimension kycomp(48,MXYCMP)
       equivalence (ycomp,kycomp)
C
