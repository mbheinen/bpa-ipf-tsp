C
        common /epridc/ nepbus, nepctl, mepbus, mepctl
        integer nepbus, nepctl, mepbus, mepctl
C
        common /epridc_c/  epbus(100), epctl(100)
        character epbus * 80, epctl *  120
C
        common /epridc_x/  i2pfbf(4000)
        integer * 2 i2pfbf
C
C       NEPBUS = counter of d-c records in EPBUS: BD, BZ, LD, MD.
C       NEPCTL = counter of d-c records in EPCTL: CC, CD, CN, CR.
C       MEPBUS = mofification flag:
C                .TRUE.  - data in EPBUS has been modified;
C                .FALSE. - data in EPBUS has not been modified.
C       MEPCTL = mofification flag:
C                .TRUE.  - data in EPCTL has been modified;
C                .FALSE. - data in EPCTL has not been modified.
C
C       EPBUS = character array containing BD, BZ, LD, MD records.
C       EPCTL = character array containing CC, CD, CN, CR records.
C
C       I2PFBF = integer * 2 scratch array
