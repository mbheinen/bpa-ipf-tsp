# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/bin
ENV=$HOME/.bashrc
USERNAME=""

export USERNAME ENV PATH

mesg n

UIDPATH="/shr/ipf-3.24/exe/%U:/usr/lib/X11/uid/%U"
export UIDPATH
# path to locate help and sample data files
IPFDIRS="/shr/ipf-3.24/dat/"         
# optional alternative IPFDIRS=<IPF base case directory>
IPFSRV="/shr/ipf-3.24/exe/ipfsrv"
IPFSRV_CF="shr/ipf-3.24/exe/ipfsrv" 
# IPF_SOCK_PATH=<IPF temporary directory to hold socket locking files>
IPF_SOCK_PATH="/shr/ipfsocktemp"
export IPFDIRS IPFSRV IPFSRV_CF IPF_SOCK_PATH
RUN_IPFSRV="/shr/ipf-3.24/exe/ipfsrv"
export RUN_IPFSRV
rm -f IPF_EXE
ln -s /shr/ipf-3.24/exe IPF_EXE

/usr/games/fortune

startx
