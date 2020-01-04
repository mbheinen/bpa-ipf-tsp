
        common /bus_x/ e(MAXBUS), f(MAXBUS),capcor(2,MAXBUS), !R*8
     &                 busdta(16,MAXBUS), base(MAXBUS),       !R*4
     &                 inp2alf(MAXBUS), alf2inp(MAXBUS),      !I*4, R*4
     &                 vstart(MAXBUS),inp2opt(MAXBUS),	      !R*4, I*4
     &                 opt2inp(MAXBUS), bus_number(MAXBUS)

        common /bus_c/ bus(MAXBUS), zone(MAXBUS), owner(MAXBUS)

	double precision e, f, capcor
        real busdta, base, vstart
        integer kbsdta(16,MAXBUS), alf2inp, inp2alf, inp2opt, opt2inp,
     &          bus_number
        character bus*8, zone*2, owner*3

        equivalence (busdta,kbsdta)

