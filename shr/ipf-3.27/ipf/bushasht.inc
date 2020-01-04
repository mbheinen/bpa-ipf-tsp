
      integer   BUS_HASHSIZE
      parameter (BUS_HASHSIZE = 1999)     !number of linked lists

      integer   htable_b(BUS_HASHSIZE)    !table of pointers to lists
      integer   nextptr_b(MAXBUS)         !ptr to next list element
      integer   nextsymbol_b

      common /bushasht/ htable_b, nextptr_b, nextsymbol_b

