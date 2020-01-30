
        common /branch/ ltot2, kx(0:MAXBRN2), ky(0:MAXBRN2), 
     &                  brsect(0:MAXBRN2), 
     &                  brtype(0:MAXBRN2), brnch(18,MAXBRN), 
     &                  rateln(3,MAXBRN), brnch_ptr(0:MAXBRN2), 
     &                  brnch_nxt(0:MAXBRN2)
        integer kbrnch(18,MAXBRN), brnch_ptr, brnch_nxt, brsect, brtype
        integer ltot2, kx, ky
        equivalence (brnch,kbrnch)
        real brnch, rateln

        common /branchc/ brid(0:MAXBRN2)
        character brid*1

