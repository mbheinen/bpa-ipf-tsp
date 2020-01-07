
      integer   PTI_HASHSIZE
      parameter (PTI_HASHSIZE = 1999)   !number of linked lists
      integer   PTI_MAXBUS
      parameter (PTI_MAXBUS = 16000)    !number of PTI buses
c
c     Bus hash arrays
c
      integer   num_hashn               !counter for pti_num
      integer   htable_n(PTI_HASHSIZE)  !table of pointers to pti_num
      integer   nextptr_n(PTI_MAXBUS)   !ptr to next htable_n element
      integer   num_hashb               !counter for pti_bus/base/zone
      integer   htable_b(PTI_HASHSIZE)  !table of pointers to pti_num
      integer   nextptr_b(PTI_MAXBUS)   !ptr to next htable_n element
      integer   pti_num(PTI_MAXBUS)     !PTI bus number
      character pti_name(PTI_MAXBUS)*8  !PTI bus name
      real      pti_base(PTI_MAXBUS)    !PTI bus base kv
      integer   pti_area(PTI_MAXBUS)    !PTI bus area number
      integer   pti_zone(PTI_MAXBUS)    !PTI bus zone number
      integer   pti_owner(PTI_MAXBUS)   !PTI bus owner number
c
c     Zone hash arrays
c
      integer   num_znam                !counter for pti_znam
      integer   htable_z(MAXCZN)        !table of pointers to pti_znam
      integer   htable_y(MAXCZN)        !table of pointers to pti_znum
      integer   nextptr_y(MAXCZN)       !ptr to next htable_y element
      integer   nextptr_z(MAXCZN)       !ptr to next htable_z element
      character pti_znam(MAXCZN)*2      !PTI zone names
      integer   pti_znum(MAXCZN)        !PTI zone numbers 
c
c     Owner hash arrays
c
      integer   num_onam                !counter for pti_onam
      integer   htable_o(MAXOWN)        !table of pointers to pti_onam
      integer   htable_q(MAXOWN)        !table of pointers to pti_onum
      integer   nextptr_o(MAXOWN)       !ptr to next htable_o element
      integer   nextptr_q(MAXOWN)       !ptr to next htable_q element
      character pti_onam(MAXOWN)*3      !PTI owner names
      integer   pti_onum(MAXOWN)        !PTI owner numbers 
c
c     Area hash arrays
c
      integer   num_anam                !counter for pti_anam
      integer   htable_a(MAXCAR)        !table of pointers to pti_anam
      integer   nextptr_a(MAXCAR)       !ptr to next htable_a element
      integer   htable_c(MAXCAR)        !table of pointers to pti_anum
      integer   nextptr_c(MAXCAR)       !ptr to next htable_c element
      character pti_anam(MAXCAR)*10     !PTI area names
      integer   pti_anum(MAXCAR)        !PTI area numbers

      integer   sort(PTI_MAXBUS)        !sorted cross-reference array

      common /pti_hasht/ num_hashn, htable_n,  nextptr_n,
     &                   num_hashb, htable_b,  nextptr_b,
     &                   pti_num,   pti_base,  pti_area,
     &                   pti_zone,  pti_owner, num_znam,  htable_z,  
     &                   htable_y,  nextptr_y, nextptr_z, 
     &                   pti_znum,  num_onam,  htable_o,  htable_q,
     &                   nextptr_o, nextptr_q, pti_onum,
     &                   num_anam,  htable_a,  nextptr_a, htable_c,  
     &                   nextptr_c, pti_anum,  sort

      common /pti_hashc/ pti_name,  pti_znam,  pti_onam,  pti_anam

