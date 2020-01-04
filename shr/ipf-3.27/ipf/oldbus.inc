c
        common /oldbus/ olde(MAXBUS), oldf(MAXBUS), 
     &                  numobs, oldbas(MAXBUS)
c
        integer numobs
        real oldbas, olde, oldf

        common /oldbusc/  oldbus(MAXBUS)
        character oldbus * 8
C
