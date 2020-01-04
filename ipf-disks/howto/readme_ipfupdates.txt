
--------------------------------------------------------------------------------------
Part I. FTP Downloading Updates to IPF-3.27 Program with Netscape Navigator

1.  The ftp retrieval is best performed as su root . First, log in as su root,
cd to the top directory, "cd /", and use "ls -l" check to see that you have a
directory called:  /ipfupdates.  

- If you do not, create one by typing: "mkdir ipfupdates". 

- Change to the ipfupdates subdirectory by typing:  "cd /ipfupdates"

2. Open Netscape Navigator, and direct it to the following External BPA URL: 

		ftp://ftp.bpa.gov/downloads/software/linux/

	- You will see a list of files for download and some subdirectories.

	- The file you want to download for IPF updates, has the form:

		22-Oct-2000.tar  

	- First, set up the download directory: <shift-click> on the filename.
	  An FTP file download window will open; 

	  <double-click> to get to the download directory, "/ipfupdates"
	  <click> CANCEL

	- Now, <shift-click> on the file again, and it will download.

Part II. Running the Updates to the IPF-3.27 Program Package 

1.  The ipf rebuild, with the update, must be done as su root.  
	Open a Terminal, login as "su" and type "cd /ipfupdates"
	Type "ls -l"; you will see the file you downloaded, 22-Oct-2000.tar
    	Type "tar -xvf 22-Oct-2000.tar to extract the update files.

	This will create a subdirectory, /ipfupdates/22-Oct-2000.
	Type "cd 22-Oct-2000" to go to the specific IPF update.

2.	Type "make", and watch the rebuild process take place.

3.  With each update, the rebuild process creates a subdirectory in
/shr/ipf-3.27 with the same date name, e.g. 22-Oct-2000, as a continuous log
of the updates you have applied to your installation of IPF-3.27.

--------------------------------------------------------------------------------------

wlp 22-Oct-2000
jgs rev. 5-Nov-2000
jgs rev. 5-Dec-2000
