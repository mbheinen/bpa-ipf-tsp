
      integer MATROWSIZE
      parameter (MATROWSIZE = 50)
      common /matrow/ matrow(3*MATROWSIZE+3), idell, idelh, 
     &                locl(MATROWSIZE), gil(MATROWSIZE), 
     &                bil(MATROWSIZE),  loch(MATROWSIZE), 
     &                gih(MATROWSIZE), bih(MATROWSIZE)
      dimension atrow(3*MATROWSIZE+3)
      equivalence (matrow,atrow)

