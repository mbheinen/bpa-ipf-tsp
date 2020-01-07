
c*** changing "BUS_HASHSIZE" means "RDDTAI" subroutine must be modified
c*** to allow for a new version (hash table is written to the base file)
        integer   BUS_HASHSIZE
        parameter (BUS_HASHSIZE = 1999)
c*******************************************************************
        integer   MAX_OWN
        parameter (MAX_OWN = 200)

        common /alt_case/ 
     &     olde(MAXBUS), oldf(MAXBUS), opnetu(MAXBUS),
     &     oqnetu(MAXBUS), oploadu(MAXBUS), oqloadu(MAXBUS),
     &     ontot, oltot, oltot2, okdtot, omtdcln, omtdcbs, 
     &     oldbase(MAXBUS),oarzn(MAXBUS), oalf2inp(MAXBUS), 
     &     oinp2alf(MAXBUS), obusdta(16,MAXBUS), 
     &     ohtable_b(BUS_HASHSIZE), onextptr_b(MAXBUS), 
     &     orateln(3,MAXBRN), obrnch_ptr(0:MAXBRN2), 
     &     obrnch_nxt(0:MAXBRN2), obrnch(18,MAXBRN), 
     &     okx(0:MAXBRN2), oky(0:MAXBRN2), obrtype(0:MAXBRN2),
     &     obrsect(0:MAXBRN2), odc2t(45,MAX2DC), 
     &     odcmtbs(36,MAXMDC), odcmtln(10,MAXDCL), 
     &     ontot_alf,ontypu(MAXBUS), ontotb, otbx(8,MAXTBX), 
     &     okxtot, ontot2, obctbl(12,MAXCBS), 
     &     obctbl_nxt(0:MAXCBS), oinp2opt(MAXBUS), 
     &     oxdata(22,MAXXDT), oarcnet(MAXCAR), onztot, 
     &     ontotc, ozsum(26,MAXZON), ontotic, ojtie,
     &     oarcinp(5*MAXCAR), otie(10,MAXTIE),ojowner, 
     &     oownlos(2,MAX_OWN), obusxdtptr(MAXBUS), oxdt_flag,
     &     oinetr(MAXBUS), oineti(MAXBUS)

        real oldbase, obusdta, orateln, obrnch, odc2t, odcmtbs, 
     &       odcmtln, otbx, obctbl, oxdata, oarcnet, 
     &       ozsum, oarcinp, otie, oownlos, olde, oldf, opnetu, 
     &       oqnetu, oploadu, oqloadu, oinetr, oineti

        integer ontot, oltot, oltot2, okdtot, omtdcbs, omtdcln,
     &     ohtable_b, onextptr_b, oalf2inp, oinp2alf,
     &     obrnch_ptr, obrnch_nxt, okbrnch(18,MAXBRN), okx, oky, 
     &     obrtype, obrsect, okdc2t(45,MAX2DC), okbsdta(16,MAXBUS),
     &     okdcmtb(36,MAXMDC), okdcmtl(10,MAXDCL), oarzn, ojtie,
     &     ontot_alf, ontypu, oltbx(8,MAXTBX), okxtot, ontot2,
     &     okbctbl(12,MAXCBS), obctbl_nxt, oinp2opt, ontotb, 
     &     okxdata (22,MAXXDT), onztot, ontotic, oltie(10,MAXTIE),
     &     ontotc, ojowner, obusxdtptr

        logical oxdt_flag

        equivalence (obrnch, okbrnch), (odc2t, okdc2t),
     &     (okdcmtb, odcmtbs), (odcmtln, okdcmtl), (otbx, oltbx),
     &     (obusdta, okbsdta), (obctbl, okbctbl), (oxdata, okxdata),
     &     (oltie, otie)

        common /alt_case_c/ 
     &     ofilename, ocase, oldbus(MAXBUS), obrid(0:MAXBRN2), 
     &     oldzone(MAXBUS), oldowner(MAXBUS),oarcnam(MAXCAR),
     &     oarczns(MAXCAZ,MAXCAR),oacznam(MAXCZN),
     &     oarcint(2,5*MAXCAR), obasval(9), oownnam(MAX_OWN) 

        character ofilename*60, ocase*10, oldbus*8, obrid*1, 
     &     oldzone*2, oldowner*3, oarcnam*10, oarczns*2, oacznam*2,
     &     oarcint*10, obasval*20, oownnam*3

