
      common /pasprma/
     1 errbuff, postmstr, postdynm

      common /pasprmi/
     1 lcord, lbase, errcord, errbase, corfil, lpost, morcord, 
     2 mcrdfls(5), icrdfls, icrdmax, multprc, multplt

      common /pasprml/
     1 blrplate
 
      integer errcord, errbase, corfil 
      character errbuff*80, postmstr*80, postdynm*80 
      logical blrplate

*     multplt - Total number of plot files to be processed for multiple coord files
*     multprc - number of plot file being processed for multiple coord files
         
