c
        common /pfstates/ ostates
        integer ostates
c
c       ostate = pf state:
c
c              0 : pf has not been initialized
c              1 : no base case data in residence
c              2 : raw base case data in residence
c              3 : Y-matrix has been rebuilt
c              4 : Y-matrix data has been reordered
c              5 : Y-matrix data has been solved successfully
c              6 : solved base case has been saved on history file
c              7 : Y-matrix data has been solved, diverged
c              8 : solved, diverged base case has been saved on
c                     history file
c
