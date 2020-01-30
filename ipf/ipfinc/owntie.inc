        integer MAXOWNTIE
        parameter (MAXOWNTIE = 30 * MAXOWNERS)
        common /owntie/ numtie, lowntie(6,MAXOWNTIE), 
     &                  owntie(2,MAXOWNTIE), 
     &                  numref, 
     &                  own1_inx(0:MAXOWNERS), 
     &                  own1_own2(0:MAXOWNTIE), 
     &                  own1_nxt(0:MAXOWNTIE), 
     &                  own1_ref(0:MAXOWNTIE), 
     &                  own1_tie_ptr(0:MAXOWNTIE),
     &                  own1_tie_nxt(0:MAXOWNTIE),
     &                  numptr
        integer numtie, lowntie, numref, own1_inx, own1_own2, 
     &          own1_nxt, own1_ref, own1_tie_nxt, own1_tie_ptr, numptr

