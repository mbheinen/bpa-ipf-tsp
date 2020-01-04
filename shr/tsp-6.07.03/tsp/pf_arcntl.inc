C
        common /arcntl/ pf_arcbas(PF_MAXCAR), pf_arcnet(PF_MAXCAR), 
     &                  pf_acznum(PF_MAXCZN), pf_arcinp(5*PF_MAXCAR), 
     &                  pf_ntotic, pf_area_number(PF_MAXCAR),
     &                  pf_zone_number(PF_MAXCZN)
        integer pf_acznum, pf_ntotic, pf_area_number, pf_zone_number
        real pf_arcbas, pf_arcnet, pf_arcinp

        common /arcntc/ pf_arcnam(PF_MAXCAR), pf_arcbus(PF_MAXCAR), 
     &                  pf_arczns(PF_MAXCAZ,PF_MAXCAR), 
     &                  pf_acznam(PF_MAXCZN), 
     &                  pf_arcint(2,5*PF_MAXCAR), 
     &                  pf_zone_name(PF_MAXCZN)
        character pf_arcnam*10, pf_arcbus*8, pf_arczns*2, 
     &            pf_acznam*2, pf_arcint*10, pf_zone_name*32
C
