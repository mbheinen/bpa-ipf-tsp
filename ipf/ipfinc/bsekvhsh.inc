
      integer   BSE_HASHSIZE
      parameter (BSE_HASHSIZE = 199)     !number of linked lists
      integer   MAXBASEKVS
      parameter (MAXBASEKVS = 200)    !max # local symbols
      
      common /bsekvhsh1/ htable_k, nextptr_k, numbases, basekvs

      integer   htable_k(BSE_HASHSIZE)   !table of pointers to lists
      integer   nextptr_k(MAXBASEKVS)    !ptr to next list element
      real      basekvs(MAXBASEKVS) 

