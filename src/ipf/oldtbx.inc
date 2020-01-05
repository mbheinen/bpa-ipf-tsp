
      integer   TBX_HASHSIZE
      parameter (TBX_HASHSIZE = 999)     !number of linked lists
      integer   TBX_MAXSYMBOL
      parameter (TBX_MAXSYMBOL = MAXTBX) !max # local symbols

      integer       oldtbx(8,MAXTBX)
      integer       htable_tbx(TBX_HASHSIZE) !table of pointers to lists
      integer       nextptr_tbx(TBX_MAXSYMBOL) !ptr to next list element
      integer       nextsymbol_tbx
      integer       numtbx
      integer       tbxxref(TBX_MAXSYMBOL)
      real          tbxbase(TBX_MAXSYMBOL)

      common /oldtbx/  oldtbx, htable_tbx, tbxxref, nextsymbol_tbx, 
     1                 nextptr_tbx, numtbx, tbxbase


      character*8   tbxbus(TBX_MAXSYMBOL)

      common /oldtbx_c/  tbxbus
