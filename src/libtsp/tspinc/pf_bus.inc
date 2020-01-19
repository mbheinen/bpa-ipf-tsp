
        common /bus_x/ pf_e(PF_MAXBUS), pf_f(PF_MAXBUS), 
     &                 pf_capcor(2,PF_MAXBUS),
     &                 pf_busdta(16,PF_MAXBUS), pf_base(PF_MAXBUS), 
     &                 pf_inp2alf(PF_MAXBUS), pf_alf2inp(PF_MAXBUS),
     &                 pf_vstart(PF_MAXBUS),
     &                 pf_inp2opt(PF_MAXBUS), pf_opt2inp(PF_MAXBUS),
     &                 pf_bus_number(PF_MAXBUS)
        double precision pf_e, pf_f, pf_capcor
        real pf_busdta, pf_base, pf_vstart
        integer pf_inp2alf, pf_alf2inp, pf_inp2opt, pf_opt2inp,
     &          pf_bus_number
        integer pf_kbsdta(16,PF_MAXBUS)
        equivalence (pf_busdta, pf_kbsdta)

        common /bus_c/ pf_bus(PF_MAXBUS), pf_zone(PF_MAXBUS), 
     &                 pf_owner(PF_MAXBUS)
        character pf_bus*8, pf_zone*2, pf_owner*3

