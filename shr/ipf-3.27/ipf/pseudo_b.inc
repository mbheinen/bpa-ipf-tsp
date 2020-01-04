

        integer MAX_PSEUDO                !number of pseudo-candidates
        parameter (MAX_PSEUDO = 2000)

        integer HASHSIZE
        parameter (HASHSIZE = 1999)     !number of linked lists

        common /pseudosections/ num_pseudo, htable, nextptr, pseudo

        integer   num_pseudo            !counter to array pseudo
        integer   htable(HASHSIZE)      !table of pointers to lists
        integer   nextptr(MAX_PSEUDO)   !ptr to next list element
        integer   pseudo(8,MAX_PSEUDO)  !array of pseudo-buses
C
C       pseudo(1,*) = ptr
c             (2,*) = section code
C                     0: no sections
c                     1: current entity is section 1
c                     2: current entity is section 2, ..., n-1
c                     3: current entity is section n
c             (3,*) = bus 1 #
c             (4,*) = bus 2 #
c             (5,*) = type_e code
C                     0: no type_e
c                     1: current entity has a tie-branch at bus1
c                     2: current entity has a tie-branch at bus2
c             (6,*) = bus 1 #
c             (7,*) = bus x #
c             (8,*) = bus 2 #
C
