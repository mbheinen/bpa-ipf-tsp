
      integer   HASHSIZE_Z
      parameter (HASHSIZE_Z = 999)     !number of linked lists
      
      common /zonehash/ htable_z, nextptr_z

      integer   htable_z(HASHSIZE_Z)       !table of pointers to lists
      integer   nextptr_z(MAXCZN)          !ptr to next list element

