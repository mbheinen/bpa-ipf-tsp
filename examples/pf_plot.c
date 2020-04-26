pf_load_oldbase("A98CY94.BSE");
pf_load_refbase("J98CY94.BSE");
pf_plot("500BUS_DIF.COR", "A98CY94.PS", "");

system("print/queue=EOHQMS_PS A98CY94.PS");