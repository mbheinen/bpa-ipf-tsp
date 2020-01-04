C
        common /arcntl/ arcbas(MAXCAR), arcnet(MAXCAR), acznum(MAXCZN),
     &                  arcinp(5*MAXCAR), ntotic, area_number(MAXCAR),
     &                  zone_number(MAXCZN)
        integer acznum, ntotic, area_number, zone_number
        real arcbas, arcnet, arcinp

        common /arcntc/ arcnam(MAXCAR), arcbus(MAXCAR), 
     &                  arczns(MAXCAZ,MAXCAR), acznam(MAXCZN), 
     &                  arcint(2,5*MAXCAR), zone_name(MAXCZN)
        character arcnam*10, arcbus*8, arczns*2, acznam*2, arcint*10,
     &            zone_name*32
C
