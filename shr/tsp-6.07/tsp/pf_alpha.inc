      common /alpha/   pf_gkmu(PF_MAXYE),  pf_bkmu(PF_MAXYE),         
     &                 pf_gkku(PF_MAXBUS),  pf_bkku(PF_MAXBUS),   
     &                 pf_pnetu(PF_MAXBUS), pf_qnetu(PF_MAXBUS),     
     &                 pf_inetr(PF_MAXBUS), pf_ineti(PF_MAXBUS), 
     &                 pf_ploadu(PF_MAXBUS), pf_qloadu(PF_MAXBUS),
     &                 pf_vlimn(PF_MAXBUS), pf_vlimx(PF_MAXBUS),  
     &                 pf_km(PF_MAXBUS+1), pf_kmlen(PF_MAXBUS),
     &                 pf_ikmu(PF_MAXYE),
     &                 pf_ntypu(PF_MAXBUS), pf_nspar(PF_MAXBUS),
     &                 pf_yptr    
      double precision pf_inetr, pf_ineti,
     &                 pf_gkmu,  pf_bkmu,       
     &                 pf_pnetu, pf_qnetu,    
     &                 pf_ploadu, pf_qloadu,
     &                 pf_gkku, pf_bkku  
      real             pf_vlimn,pf_vlimx
      integer          pf_km, pf_kmlen, pf_ikmu, 
     &                 pf_ntypu, pf_nspar, pf_yptr 
