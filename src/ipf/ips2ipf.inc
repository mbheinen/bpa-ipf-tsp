c
c       This include is part of ips2ipf
c
        integer MAXBUSES
        parameter (MAXBUSES = 8000)    !number of buses

        integer HASHSIZE
        parameter (HASHSIZE = 1999)     !number of linked lists

        integer   htable(0:HASHSIZE)      !table of pointers to lists
        integer   nextptr(0:MAXBUSES)      !ptr to next list element

        real oldbase(0:MAXBUSES)

        common /hasht/ numcvt, htable, nextptr, numbus, oldbase


        character rename(2,0:MAXBUSES) * 12, oldbus(0:MAXBUSES) * 12

        common /hashtc/  rename, oldbus

