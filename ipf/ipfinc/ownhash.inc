
      integer   OWN_HASHSIZE
      parameter (OWN_HASHSIZE = 199) !number of linked lists
      integer   MAXOWNERS
      parameter (MAXOWNERS = 250)    !max # local symbols
      
      common /ownhash/ htable_o, nextptr_o, numown, alf2own, own2alf

      integer   htable_o(OWN_HASHSIZE)   !table of pointers to lists
      integer   nextptr_o(MAXOWNERS)     !ptr to next list element
      integer   alf2own(MAXOWNERS)       !Alpha to Owner sort index
      integer   own2alf(MAXOWNERS)       !Owner to Alpha sort index

      common /ownhashc/  owner_o

      character * 3   owner_o(MAXOWNERS)

