c     -- start of gov.inc

c     see govinp routine for info on govdat structure
      real    govdat(MAXGEN*GOVSLOTS)
      integer govtyp(MAXGEN), govsource(MAXGEN), govmode(MAXGEN),
     &        govindx(MAXGEN), govlpi(MAXGEN), govoffset, govcv(MAXGEN)
      character*8 govname(MAXGEN)

      common/govdata/ govdat, govtyp, govsource, govmode,
     &                govindx, govlpi, govoffset, govcv, govname

      real govmn(MAXGEN), govmx(MAXGEN)
      common /govminmax/ govmn, govmx

C     -- end of  gov.inc
