*BEGIN REREAD
C     -                     reread.inc
c - Revs
c     Jul/30/92
c       Added BUFNXT to store next control card, i.e., read in from
C         control file but not yet processed.
c     May/04/92  -  DEM
c       Added CTLFMT to distinguish control file formats
      COMMON /RREADC/ BUFFER,BUFNXT,CTLFMT                              !dem
c     COMMON /RREADC/ BUFFER
      CHARACTER*80 BUFFER
      CHARACTER*80 BUFNXT                                               !dem
      CHARACTER*10 BUF10
      DIMENSION BUF10(8)
      EQUIVALENCE (BUF10,BUFFER)
      CHARACTER CTLFMT*3                                                !dem
      COMMON /RREADN/ NEND8
C     -                 end of reread.inc
