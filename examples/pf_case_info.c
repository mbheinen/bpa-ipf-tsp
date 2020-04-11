int error;

pf_case_stats ci;

error = pf_case_info (&ci);

if (!error) 
{
    fprintf (out, "Number of areas = %d\n", ci.num_areas, 
                  "Number of zones = %d\n", ci.num_zones, 
                  "Number of buses = %d\n", ci.num_buses,
                  "Number of connections = %d\n", ci.num_circuits, /* Not including parallels*/
                  "Number of lines = %d\n", ci.num_branches);

    if (ci.case_soln_status == SOLVED)
        fprintf(out, "This was a solved case.\n\n");
}
