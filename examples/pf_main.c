#include "cflowlib.h"

int main(int argc, char *argv[])
{
    pf_cflow_init(argc, argv);
    printf("pf_load_netdata=%d\n", pf_load_netdata("bench.net"));
    printf("pf_solution=%d\n", pf_solution());
    pf_cflow_exit();
}
