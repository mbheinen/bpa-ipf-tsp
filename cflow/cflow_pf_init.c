/*****************************************************************************
* file: cflow_pf_init.c
* author: Walter Powell
* purpose: 
*
*   This routine replaces entirely cflow_lib_ipc.c with a direct call
*   to srv_cmdprs, obviating any need of an IPC socket to the powerflow
*   server program ipfsrv.
*
******************************************************************************/


int initlz_cw (void);
void pfinit_cw (void);

/*****************************************************************************
* function: cflow_pf_init
* author: Walter Powell
*
* purpose: initialize srv_fcmdprs for direct communication
*        
******************************************************************************/

void cflow_pf_init()
{
   int  status;

   status = initlz_cw ();   /* init for cpyinbuf */
   pfinit_cw();

   return;
}


