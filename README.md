# Interactive Power Flow & Transient Stability Program
This is a fork of the Bonneville Power Administration's Interactive Power Flow and Transient Stability Programs. Both of these are public domain and were originally obtained from ftp://ftp.bpa.gov (which no longer seems to be available). My hope is to get this codebase to a point where it can be compiled and run in hopes to use it for benchmarking future projects related to power flow or transient stability analysis. 

Much of the content in this README was taken from PDFs in the original codebase and therefore contain very dated content (e.g. setup of Red Hat 6.2 from a CD). Eventually, this will be cleaned up but for now has been copied for completeness of the initial import of the original codebase.

Note that the original programs had a GUI component, but given how dated it is, it is very unlikely it will ever run without significant effort. The initial goal is to get these ipf and tsp programs operational enough to be able to be used from the command line.

# Installation

Setup PC with Redhat Linux 6.2 for Interactive Powerflow Program (IPF)

Additional Program Installation Guidelines (Install after you set up Redhat Linux 6.2, below):

## Adobe Acrobat Reader 4.05 for Linux
1. Login as "root" and type "startx" to run the Gnome desktop.
2. Insert the CD and use Gnome File manager to copy files.
3. Copy to the top directory "/" the file "adobe405.tar.gz" from the CD.
4.  Unzip and untar it by typing "gunzip adobe405.tar.gz" , and "tar -xvf adobe405.tar".
5. "cd" to the directory "/Adobe/ILINXR.install" and type "./INSTALL"
6. Follow the prompts and allow it to use the default install directory.
7. When installation is complete, "cd" to the directory "/usr/local/Acrobat4/bin" and type "cp ./acroread  /usr/bin"
8.  You can now run Acrobat Reader from the commandline by typing:    "acroread"

##  GhostScript 6.50 Postscript/PDF Converter
1. Login as "root" and type "startx" to run the Gnome desktop.
2. Insert the CD and use Gnome File manager to copy files.
3. Copy to the top directory "/" the file "ghostscript650.tar.gz" from the CD.
4. Unzip and untar it by typing "gunzip ghostscript650.tar.gz" , and "tar -xvf ghostscript650.tar".
5. "cd" to the directory where you will find four (4) RPM files:
6. Type "rpm -U ghostscript-6.50-2.i386.rpm" and follow the prompts.
7. Then, type "rpm -U ghostscript-fonts-6.0-2.noarch.rpm" and follow the prompts.
8. The upgrade from ghostscript-5.xx to ghostscript-6.50 installs the ghostscript executables in a different location, and as a result the Redhat 6.2 PrinterTool cannot find them.  When you try to use PrinterTool to set up a printer, there is an error message stating that "Ghostscript does not appear to be installed on this system".  The easiest way to fix this is to copy all the ghostscript6.50 executables from the new install location, "/usr/local/bin" to "/usr/bin".  Login as "root" or "su", and change to the new ghostscript location by typing:  "cd /usr/local/bin".  Then type "cp ./* /usr/bin".  Now, you will find that PrinterTool can set up your printer.
9. PrinterTool:  If you delete a printer you had already set up, PrinterTool will remind you to remember to remove the old spool directory, "/var/spool/lpd/lp". Do this by typing, " rm -fR /var/spool/lpd/lp".

## Add Items to Motif FrontPanel "Pop-Up" Menus
1.  With an editor of your choice, edit the file ".fpanelrc" in the /home/ipf327user subdirectory. Examine the structure of the existing menu items.  Add menu items and save the file.
You can add a menu item for Adobe Acrobat by adding the line:           "Adobe Acrobat 405"     f.exec "acroread"
You can add a menu item for X-Windows Calculator by adding the line:           "X-Calculator"                f.exec "xcalc"
D.  Enliven the "ipf327user" Desktop with Snow
1. Being logged in as "root", "cd" to the directory /ipf-disks/howto"
2. Type "cp  ./x*  /usr/bin
3.  The next time you login as "ipf327user", you will notice the difference.     (you copied two additional programs to /usr/bin that were not part of the default Redhat 6.2 Installation, and one of them is programmed to start when you login as "ipf327user", per the login resource file .xinitrc) The programs are:  "xsnow" and "xearth". Open a terminal window and type "xearth &" to see the other program.


# Setup PC with Redhat Linux 6.2 for Interactive Powerflow Program (IPF)
## Install Redhat Linux 6.2 for Single-boot or Multi-boot system.
1.  Have available and installed in your PC, an additional hard drive for Linux, or have an existing hard drive re-partitioned with at least 2.1-gigabytes of space available for Linux and IPF.
2.  If your PC has an existing bootable internal IDE or SCSI CD-ROM, boot up the PC with the bootable Redhat 6.2 install CD.  If the CD-ROM device is not bootable, use the Redhat 6.2 bootable install floppy.  If your PC is a laptop or notebook without an internal CD-ROM, and your CD-ROM is an external PCMCIA device with a Linux-supported PCMCIA interface card, boot up with a floppy disk created with the "pcmcia.img" Linux boot image (this disk can be created on a DOS/Windows PC using instructions located in the "dosutils" subdirectory on the Redhat 6.2 CD).
3.  At the initial Redhat install screen, choose "text" for the install mode, and type "text" at the command line prompt.
4. Select the your preference of Language.
5. Select keyboard type.
6. Select Installation type:  "Install Custom System"  [Do NOT follow any other Installation Method].
7. Run Disk Druid (or fdisk, if you prefer):
8. Select "Add", set mount point to "/boot", with a size of 15-MB
9. Select "Add", leave mount point blank, set size to equal or larger than your system RAM, but no smaller than 64-MB, and set the type to "Linux Swap".
10. Select "Add", set mount point to "/", with a large size but less than the remaining free space, and check the "Grow to fill disk?" box.
11. Save changes to the partition table.
12. Format the partitions, use defaults, select OK.
13. Lilo configuration, use defaults, select OK.
14. Lilo configuration, "Where to install the bootloader?"  If you are installing Linux on your PC alone with no other operating system, or to co-exist in multi-boot mode with MS-DOS, MS-Windows95, MS-Windows98, or MS-WindowsME, select "Master Boot Record (MBR)".    If you are installing Linux to to co-exist in multi-boot mode with MS-WindowsNT or 2000, or with a boot manager such as "System Commander", select "First sector of boot partition".
15. For Hostname Configuration, type in your system name in the form:  "thissystem.abc.org" or "thissystem.xyz.com" or other similar form.
16. For Mouse Selection, select Generic 3-button  and Check the box for "Emulate 3-buttons?"
17. Set the Time Zone to correspond with your location.
18. Enter a password for the "root" system administrator login account.  Be sure and write it down somewhere, in case you forget it.
19. Create at least one standard user account.
20. Authentication Configuration -- use the defaults, and select OK.
21. For Package Group Selection, go to the bottom of the list and select "Everything".
22. X-probe results -- select OK, and OK to begin the installation.
23. Bootdisk:  enter "Yes" to create a backup bootdisk.  You can make more of these later with a command line utility.
24. Monitor Setup:  Set up the display/monitor for 1024x768, 16-bit color.
25. When X-configurator runs the test and asks if you can see the display, and then asks if you want to use a Graphical Login, answer "No" to the Graphical Login.  Later, you will manually set up user accounts for IPF users based on the Motif Window Manager, and if your default is the Gnome Desktop/Enlightenment Window Manager, you will not be able to create the Motif accounts.
26. Complete:  Remove the backup boot floppy, and select OK to reboot the system.

##  Install Redhat Linux 6.2 Update CD (Optional)
1.  Redhat makes available an Update CD-ROM on a monthly basis, containing the latest versions of applications and kernels for the particular distributions (5.2, 6.2, and 7.x).  Obtain and Update CD, and install it.
2.  Application Update:   Login as "root", do NOT run "startx". Insert the Update CD, and type:  "mount -t iso9660 /dev/cdrom /mnt/cdrom"
3.  Type "cd /mnt/cdrom" to change to the CD-ROM.
4.  Type "./install-updates" and follow the prompts, to update all the applications.  This process can take as long as 1-hour.  When that is done, you can update the kernel to the latest one on the Update CD following the procedure described later.
5.  When it is done, type "cd /" and then type "umount /dev/cdrom".  Now you can remove the CD, unless you plan to update the kernel next.  You might get a "device busy" message.  If so, leave it alone, and shutdown anyway.
6. When your are done, shutdown and reboot, by typing "shutdown -r now".
7.   Kernel Update:   First, login as "root" and make a note of the version number and type of kernel you are presently running, for example, "Kernel 2.2.14-5.0 on an i686".  Assuming you are running 6.2, type "cd /mnt/cdrom/6.2" to change to the 6.2 area on the CD-ROM.
8. Type "ls kernel*.rpm" to get a list of the kernel RPM packages.  Update the RPMs that apply to your situation.  For example, If you are running 6.2 on a laptop that has an i686 processor, at the minimum, you would upgrade two kernel RPMs.  Type:
"rpm  -U  kernel-2.2.17-14.i686.rpm"   and when that is complete, type: "rpm  -U  kernel-pcmcia-cs-2.2.17-14.i386.rpm"
9. When that is complete, you will need to update both your "boot-up" LILO installation, and any boot floppies you are using.  Edit the file "/etc/lilo.conf", and modify the Kernel identification to correspond to the new kernel, as installed in the "/boot" subdirectory.  Save the lilo.conf file, and install it by typing "lilo".
10.   Shutdown and reboot.  When you login, you should note the message indicating that the new kernel is operational.   Now, use the command, "mkbootdisk" to update your backup boot floppies.  For example, type "mkbootdisk -device  /dev/fd0  -v  2.2.17-14" and insert your existing boot floppy, or a blank disk for a new boot floppy.

## Install Redhat Linux 6.2 Documentation Set (Optional):
1.  Login as "root", and type "startx" to open the Gnome Desktop
2.  Insert the CD in the drive, and allow Gnome to mount the CD
3.  Select "system, gnorpm" and select "Install".
4.  When the list of RPM packages comes up, select the ones you want to install by package name, but do not select the corresponding package name ending with (s).  Click the Install icon.
5.  When installation is complete, close the Install Window, the GnomeRPM Window, and FileManagerWindow , and the  "/mnt/cdrom" Window.  Type:  "umount /dev/cdrom".  Now you can remove the CD.

##  Install Metrolink Motif Complete! Motif Window Manager and X-Windows Development Kit (XDK):
1.  Logon as root, and then run "startx" to start the X-Windows environment and Gnome desktop.
2. Insert the MetroLink Motif Complete! CD.  The Gnome desktop will automatically mount the CD, in most cases.  If so, you can skip to Step-5.
3.  Open a terminal window, Xterm, and type "cd  /"
4.  Mount the CD by typing, "mount  -t  iso9660  /dev/cdrom  /mnt/cdrom
5.  Change directory to the CD by typing, "cd  /mnt/cdrom"
6.  Get a directory listing of what is there by typing, "ls  -l"
7. There should be an install script, "install-motif", listed there. 8.  Run the install script by typing, "./install-motif"
9.  When the "Welcome to Metro Link Motif Complete!" window comes up, click "next".
10.  In the next window, read the license, and click the the "I accept ..." button, then click "next".
11.  In the next window, select "Install both Runtime and Development packages".
12.  In the next window, select Motif 1.2.4 as the Primary Motif Version, and click "next".
13.  In the next window, select both "mwm 2.0.1" and "mwm 1.2.4", and click "next".
14.  In the next window, select both "Motif 2.0.1" and "1.2.4" Runtime Libraries, and click "next".
15.  In the next window, select "Motif 2.0.1" as the Secondary Development Environment, and click "next".
16.  In the next window, select Install "libc5...", and click "next".
17.  In the next window, select "RPM" package format, and click "next".
18.  In the next window, click "next" to proceed with installation.
19.  When it is all done with the installation, type "cd  /" to get back off the CD to your hard drive, and then unmount the CD by typing, "umount  /dev/cdrom", and then remove the CD.

##  Create New "ipf327user" Login Account Specifically for Running IPF-3.27
1.  Login as "root" and type "startx" to run the Gnome Desktop.
2.  Select KDEmenus,System,UserManager.
3.  Select Add, and type "ipf327user" in the space provided, then click "OK".
4.  Type "IPF-3.27 User" in the space provide for "Full Name".
5.  Select "/bin/bash" for "Login Shell" -- [THIS IS REQUIRED]
6.  Click "Set Password" and put in a password of your choice [be sure to write it down]
7.  Click "OK","OK", File, Save, Exit.
8.  This creates the "/home/ipf327user" account and subdirectory Template, which will     be filled in with the required Motif setup files, etc. by the following "Step-F".

## INSTALL IPF-3.27.18 SOURCE CODE AND COMPILE
1.  Login as "root" and type "startx" to run the Gnome Desktop.
2.  Insert the CD with the Program Source Code ".tar.gz package" (either or both of the following files, or files with similar names: ipf-3.27.18generic_customer.tar.gz ipf-3.27.18generic_export.tar.gz
3.  Copy either of these files to the top directory "/".
4.  Unzip and untar it by typing: "gunzip <filename.tar.gz> <cr> " tar -xvf <filename.tar><cr>
5.  This will install the proper MOTIF "ipf327user" setup files in /home/ipf327user, and will install all the required program and related files and subdirectories in the IPFStandard directory structure [This is for a fresh, first-time install].
6.  Change to the /root subdirectory, and replace the existing .bash_profile with the proper version that is updated with required "path"information for IPF-3.27.17 and later versions. Do this by typing "cd /root" and then type "cp ./bash_profile .bash_profile_original"' (This makes a backup of your original .bash_profile).
Now, replace it with the new one, included as "sample.bash_profile"  or "sample.bash_profile_RH62" for a Redhat 6.x installation, or "sample.bash_profile_RH71" for a Redhat 7.x installation.
Type:  cp /updatedisks/sample.bash_profile   /boot and then:   cp /updatedisks/sample.bash_profile  /boot/.bash_profile
Type:  cp /updatedisks/sample.bash_profile_RH62   /boot and then:   cp /updatedisks/sample.bash_profile_RH62   /boot/.bash_profile
OR
Type:  cp /updatedisks/sample.bash_profile_RH71   /boot and then:   cp /updatedisks/sample.bash_profile_RH71   /boot/.bash_profile
Copy the same corresponding special "sample.bash_profile" to replace the file ".bash_profile" residing in  "/home/ipf327user" and any other user account (either Motif or Gnome-based) that is used to run the IPF program.
7.  Logout of the Gnome Desktop, and type "exit" to logoff completely as "root".  Now, log back in as "root".  This will activate the new path information installed by update provided by the copy of "sample.bash_profile".  If you do not do this, you will not be able to compile IPF.
8.  Change to the /ipfupdates directory by typing "cd /ipfupdates, and then change to the most recent update subdirectory, /1-April-2001 by typing "cd /1-April-2001".
9.  Type "make" to compile the IPF-3.27.18 version of the powerflow program.
10.  When compilation of the source code is complete, logout of the Gnome desktop, and leave the "root" login account by typing "exit".
11.  Login as "ipf327user" with the password you have assigned to that account in Part E. above.
12.  Now you are in the Motif Window Manager desktop environment from where you can  run the IPF-3.27 GUI, and all the associated batch programs from the command line, or from the "FrontPanel Pop-up Menus".

## Motif Window Manager for Running IPF
a)  Set Up Additional User Login Accounts for Motif Window Manager
1.  It is helpful to have more than one user account, each running a different Window Manager.  For example, I set up two accounts for myself, one running the default window manager for the particular Linux Installation, and the other one to run Motif, so that I can run IPF. .....  User Login Accounts:  jack and jackm    .... the one with the "m" is set up to use Motif Window Manager.
2.  When you set up a new account to run Motif Window Manager, DO NOT run "startx" in that account until after you have set up the proper start-up files in that account directory.  When you first run "startx", a new .Xauthority file is created, along with other setup files, which are then used by the chosen window manager for that account.
3.  First, login to the new Motif user directory, (e.g. login as "jackm").  Type "ls  -al" to get a listing of all the files, including the "hidden"files.  In Linux and Unix, the hidden files always start with a "."   You will find a file called ".bash_profile".... make a backup of the original version of the file by copying it to a new name:  type "cp  .bash_profile  .bash_profile_original".
4.  There are some Motif user setup files you will need to use.  They can be found on the CD in the file
"ipf-disksH.tar.gz".    As part of the preliminary installation, you would have copied this file to the / directory of the hard drive, unzipped and untarred it.  The files you need are named and located in these locations:
/updatedisk/sample.bash_profile_RH62 or RH71 to replace the default .bash_profile /ipf-disks/newdotfiles/dot.mwm /ipf-disks/newdotfiles/dot.xin /ipf-disks/newdotfiles/dot327.fpa /ipf-disks/XGUI
5.  Copy these five (5) files to your "Motif User Account" directory.
6.  You will need to replace the .bash_profile with the new one containing IPF-specific path information. Do this by typing, "cp  ./sample.bash_profile_RH62 (or RH71)  .bash_profile".  If it asks if you want to overwrite the existing file, respond  with a "y" for yes.
7.  Make the proper versions of the other files by typing,  "cp  ./dot.mwm  .mwmrc",  "cp  ./dot.xin  .xinitrc", and  "cp  ./dot327.fpa  .fpanelrc".
7a.  Assuming you have already installed  IPF, bring a copy of the IPF XGUI resource file into the Motif user directory by typing, "cp /ipf-disks/XGUI  ."   This can also be done at a later time, if you have not yet installed IPF.  Without the XGUI file in your home directory, you will not get the proper colors on the screen when running the IPF GUI.
8.  Log out by typing, "exit".
9.  Log back in as "jackm", type the command, "startx", and you should have a Motif Window Manager desktop in front of you with an mxterm, terminal window open.  You can open another terminal window by clicking on the "tools" icon in the fpanel control panel bar at the bottom of the screen.
10.  If you have IPF-3.27 already installed, you can run it by going to an open mxterm window and typing, the following command:  "gui  &"
b)  Set Up IPF-Capable User Login Accounts for Gnome Desktop Users:
1)  If you like, you can modify  any existing or new standard Default, Gnome Desktop User accounts to that they can run the IPF GUI, and the batch routines.  Unless you add the features to the Gnome Desktop User Menus yourself, you will lose access to the various IPF Pop-Up Help files, and other items available from the Motif Front Panel (FPanel).  The Motif Runtime Libraries are still required to be installed for you to be able to run the GUI, even if using the Gnome desktop & window manager, instead of the Motif Window Manager.
2)  There are basically three requirements:
First:  You must have run Xconfigurator as "root" and set up the X-windows color scheme to use 16-bit color depth at 1024x768 screen resolution.   This will not work with 8-bit (256-color) color depth.  You may have already set up during installation for the screen to operate at 1024x768 and 16-bit color depth.
Second:  You must replace the default ".bash_profile" with the file listed as "sample.bash_profile" in the /updatedisk directory,  using either the RH62 or the RH71 version of sample.bash_profile, which is setup to define the required paths, etc. for IPF.
Third:    Bring a copy of the IPF XGUI resource file into the Gnome Desktop User directory by typing, "cp /ipfdisks/XGUI  ."    Without the XGUI file in the Gnome Desktop User directory, you will not get the proper colors on the screen when running the IPF GUI.
Optionally you can run the various IPF programs from the command line by typing the respective commands followed by "&", or you can add them to the Gnome User Menus by using the Gnome Menu Editor, and entering each command followed by "&"..
For example, to run the GUI, you would open a terminal window and type "gui  &".  You would do the same, with bpa, ipfbat, etc.
c)  Install StarOffice 5.2:  Login as "root", insert the StarOffice 5.2 CD;  change to the directory "/linux/office52/program" and type " ./setup  /net".  Select  "/usr/office52" as the install directory.  With this "base" installation, any user can be set up with the minimum configuration files as a "local user" copy, by logging in as that user, opening a terminal window, and typing: /usr/office52/program/setup and following the prompts to install the local user fileset.

## COR Files Fix -- IPF Coordinate Files and Powerflow Plot Title Blocks -- HowTo Ver. 1.00  3/9/2001
1) IPF Coordinate Files: For the .COR files to work properly with IPF on Linux, the .COR file data cannot contain Carriage Returns at the ends of the lines.  .COR files transferred for a DOS or MS-Windows environment, will have had Carriage Returns inserted automatically, and these must be removed.
In order to remove the carriage returns, use the IPF utility, "dos-linux.bash"
Examples of Use:
dos-linux.bash  EWEB.COR <cr> dos-linux.bash  ewebppl.cor <cr> dos-linux.bash  *.COR <cr>
This utility also renames the files with "UPPER CASE" filenames, to "lower case" filenames.
2)   Powerflow Plot Title Blocks/User Comments:
There are some specific steps necessary to properly setup Title Blocks and User Comments on the IPF Plots. Described here is the step-by-step procedure to be followed:
After you have loaded a study case, and loaded either an existing plot coordinate file or created a new one from either the Landscape (L85X11.COR) or Portrait (P85X11.COR), you will want to insert Title Block User Comments and Descriptive Information:
a.  Click "File,Plot Options, User Comments"
b.  Click with the mouse in the white area for Additional User Comments
c.  Do two (2) Carriage Returns <cr> <cr> in order to provide space to not overwrite comments inserted by IPF itself.
d.  Type your comments, using as many lines as you need, each followed by a carriage return <cr>, up to the limits of space available.
c.  Click "OK"
d.  Look at the Title Block on the screen.  You will see that the comments do not yet appear.
e.  Click "File,PlotOptions,PageOptions" and click the "OK" button.
d.  Now you will see your Additional User Comments appear in the title block.  This process is necessary to update the Additional User Comments information, each time you make a change.
f.  Now, click "File,Plot Options, User Comments,ViewPlot"  or  "File, Print Plot" to view the Postscript IPF Plot with the Ghostview Postscript file viewer.
g.  From Ghostview, you can view the plot up close, and print copies as needed.
I.  Create Plot Coordinate COR FILES from Scratch
IPF Plot File -- How to Create a Plot Coordinate File from Scratch
1)  Run the Graphic User Interface (gui) for IPF
2)  Use File,Open to select a base case or network data file.
3)  Use File,Open to select a blank .COR file, either L85X11.COR or P85X11.COR.
4)  Load selections.  Solve the case.
5)  Activate the "Input Data Edit Box" for new data (6th button down on left column).
6)  Close the "Input Data Edit Box" window. You should now have a "square" cursor    on the screen.
7)  Select Edit,AlphaList.  Set your caps-lock key to "on".  Click in the data window, and type the first letter of the bus you want.
8)  From the list of buses that comes up, select the bus you want as the starting point for your new plot coordinate file, for example, LEBANON  115.  Close the window.
9)  With the "square" cursor, click to place it on the diagram.
10) Activate the "Explode" button (2nd button down on right column), and click on the    newly-placed bus.  This will display all first adjacent branches.
11) Activate the "Move" button (5th button down on right column), to move the buses and    labels around on the diagram.
12) Activate the "bus-delete-from-diagram" button (3rd button down on left column) to    remove any unwanted buses from the diagram.
13) When you get the diagram to the stage you want, use File,Save-As,Select to assign a    name and directory location to your new coordinate file (using"save_file_dialog_popup"    window).
14) Then, click the Save button in the "save_file_dialog_popup" window to actually save    the file.

## IPF-3.27 GUI -- Linux Version
Coordinate File Modifications: December 7, 2000 ver. 1.00
Discussion
For Studies, the use of Powerflow Plots showing proposed new generation facilities and associated system improvements is essential.  Generally the diagrams available are the Standard Powerflow Plots (.COR files) developed for the Budget Cases.  Creation of new plots or modification of these existing .COR files is necessary to show the additional facilities.
Manual modification using a text editor is time consuming and is not intuitive.  Use of the IPF GUI is a very effective way to update the poweflow plots with the new system information, or to create new plots from scratch.
The following step-by-step procedure is a guide to the steps necessary to create:
(1) an updated .COR plot file based on an existing Budget Case Powerflow Plot .COR file, or
(2) to create a new .COR plot file from scratch using the IPF GUI.
Procedure
1) Modify Existing Budget Case .COR File:
Run the IPF GUI and click the appropriate "select" and "apply", and "load selections" buttons to load the base case file (\*.BSE) already containing the added Generation and associated system improvements and the Budget Case coordinate file (\*.COR) for the part of the system you are going to show. Then click the  "load selections" buttons to load them both.  Then "File, Save-As" to save the .COR file with a name of your choice in your user directory.
After that, activate the "Explode" function by clicking the 2nd Button in the 2nd Column, and click on the busses with the new generation and facilities you have added to the system model in the .BSE file.  Then, activate the "Symbol Move" function by clicking the 5th Button in the 2nd Column, and use the mouse to rearrange the symbols on the diagram representing the new generation and facilities.
When you are done, use "File, Save-As" to save your new .COR file.
Click "Select" for Base Case File directory list Click the .BSE file you want to use Click "Apply"
Click "Select" for Coordinate File directory list Click the .COR file you want to use Click "Apply"
Click "Load Selections" After the .BSE and .COR files are loaded, click View, Solution Data On"
Click "File, Save-As".
For "Coordinate", click "Select" and enter your new name for this coordinate file. Click "OK" , then click "Save" to save the .COR file; then click "Close".
2) Create New .COR File from scratch:
Run the IPF GUI and click the appropriate "select" and "apply", and "load selections" buttons to load the base case file (\*.BSE) already containing the added Generation and associated system improvements and the Blank Template coordinate file (L85X11.COR or P85X11.COR) to show the part of the system you are going to show. Then click the "load selections" buttons to load them both.
Then "File, Save-As" to save the .COR file with a name of your choice in your user directory.
Activate the "Bus Input Data" function by clicking the 6th Button in the 1st Column.  The cursor will change from an arrow to an open square.  Close the "Bus Input Data" window.
Click "Edit, Alpha List"  to bring up a list of all the busses in the .BSE case.  Now select the bus you will use to start the diagram.  Click in the "Search Bus Name kV" window and with CapsLock on, type in the 1st letter of the bus name, then the 2nd, etc. until the bus name appears in the list.  Select the desired bus and click Close.
Then, with the mouse, locate the 1st bus at the appropriate location on the blank diagram, and click to place it there.
After that, activate the "Explode" function by clicking the 2nd Button in the 2nd Column, and click on the busses with the new generation and facilities you have added to the system model in the .BSE file.  Then, activate the "Symbol Move" function by clicking the 5th Button in the 2nd Column, and use the mouse to rearrange the symbols on the diagram representing the new generation and facilities.
When you are done, use "File, Save-As" to save your new .COR file.
Click "Select" for Base Case File directory list Click the .BSE file you want to use Click "Apply"
Click "Select" for Coordinate File directory list Click the .COR file you want to use Click "Apply"
Click "File, Save-As". For "Coordinate", click "Select" and enter your new name for this coordinate file. Click "OK", then click "Save" to save the .COR file; then click "Close".
K.  PROPER USE OF "VIEW REPORTS" with the GUI
When you use the GUI to view output reports for "Over-Loaded Line Report" or "Overvoltage/Undervoltage", etc. you must first Set the "Limits" values, after you select "View, Report".  You must set both values first, so they are usable with any of the reports you request after that.  If you do not, it will not work correctly.
For Example: Limits:  Set % Line Load = 95     Set PU Volt Relax = 0.05
This will give you report results showing Line or Transformer Loading more than 95%, and it will give you report results for voltages above 105% and below 95%.

## IPF UPGRADE GUIDE -- EXAMPLE Upgrade to version IPF-3.17.16b with the 1-Feb-2001 Update Ver. 2.01 6/3/2001
UPGRADE EXISTING IPF 3.27 PROGRAM INSTALLATION WITH NEW SOURCE CODE PACKAGE:
1. Boot up system and login as "root"; type "startx" to start the Gnome Desktop GUI.
2. Type "cd /shr" and then remove the existing /ipf-3.27 subdirectory and its contents by typing, "rm -fR ./ipf-3.27.
3. Insert the CDROM with the newer version of IPF.  It should mount automatically
4. Use the graphical file manager to copy the file "ipf-3.27.16bSRC.tar.gz" to the /shr directory, unzip and untar it, as described below.  Use cd to go to /shr/ipf-3.27.  There will be a copy of the most recent program update file, such as 1-Feb-2001.tar.
5. Use the graphical file manager or the command line copy command to copy the file "1-Feb2001.tar" to the /ipfupdates directory.  cp ./1-Feb-2001.tar  /ipfupdates.
6. Close the graphical file manager, and in a terminal window, type "umount /dev/cdrom" in order to unmount the CD so you can remove it.
7. Type "cd /shr" to go to the IPF directory.
8. Type "gunzip ipf-3.27.16b.tar.gz" to unzip the file;
9. Type "tar -xvf ipf-3.27.16b.tar" to untar the file and its contents;
10.Type "cd /ipfupdates" and type "tar -xvf 1-Feb-2001.tar" to untar the update.
11.Type "cd 1-Feb-2001" to go into the /1-Feb-2001 subdirectory, and type "make" to recompile the IPF program with the latest update, and install the new executables in their correct location /bin.    "make" will do it all for you.
M:  UPGRADE IPF HOW-TO's AND MOTIF USER INTERFACE MENU:
The collection of "pop-up how-to's", are not all completed or revised, but the latest version is included on the CD.   They are part of a file called:     "ipf-disksH.tar.gz".  To update your system to the latest version, follow these steps:
1. Boot up system and login as "root"; type "startx" to start the Gnome Desktop GUI.
2. Type "cd /" to put yourself at the top of the directory tree.  Backup/save the existing /ipf-disks subdirectory by typing: "mv ./ipf-disks  ipf-disks-old"
OR  if you do not have anything there you want to keep, remove the existing /ipf-disks subdirectory  and its contents by typing,
"rm -fR ./ipf-disks"
3. Insert the CDROM with the newer version of /ipf-disks (included on the CD as the file: "ipf-disksH.tar.gz".  The CD should mount automatically.
4. Use the graphical file manager to copy the file "ipf-disksH.tar.gz" to the / directory.
5. Type "gunzip ipf-disksH.tar.gz" to unzip the file.
6. Type "tar -xvf  ipf-disksH.tar" to extract the entire set of files and subdirectories.
7. Type "cd /home/ipf327user" to get to the IPF-3.27 Motif user area.  Make a backup copy of your existing "fpanel" files, by typing:
"cp ./dot327.fpa  dot327.fpa.original" "cp ./.fpanelrc .fpanelrc.original"
8. Get a copy of the New-Updated version of dot327.fpa, by typing: "cp /ipf-disks/howto/dot327.fpa  ." Respond to the prompt, "yes" , if it asks you for permission to overwrite the old file.
9. Make a new version of the fpanel resource file, .fpanelrc, by typing: "cp ./dot327.fpa  .fpanelrc"    Logout, and login as "ipf327user"

## PC CONFIGURATION and GUIDELINE SPECIFICATION:
FOR RUNNING THE BPA POWERFLOW PROGRAM, IPF
Dual-Boot WinNT and Redhat Linux 6.2 INSTALLATION  11-April-2001 -------------------
HARDWARE CONFIGURATION:
Motherboard:  Intel Pentium CPU - 133-Mhz minimum, 266-Mhz or faster, preferred
RAM:  64-MB minimum, 128MB or more, recommended
1)  Primary Master IDE hard-drive: Loaded with MS-Windows95/98/NT/2000 ----2) Primary Slave IDE hard-drive: Loaded with MS-Windows95/98/NT/2000  OR  add a new drive and use it for Linux
3) Secondary Master IDE hard-drive: Loaded with MS-Windows95/98/NT/2000 OR  add a new drive and use it for Linux
4) Secondary Slave IDE hard-drive: Loaded with MS-Windows95/98/NT/2000 OR  add a new drive and use it for Linux ----If you already have all 4 of your IDE controllers connected to other Hard Drives, IDE CDROMs, etc, then you can add a Linux-supported SCSI adapter, and add one or more SCSI drives for the Linux installation.
The new drive added for LINUX should be at least 2.1-gig, at the very minimum.  10-gig or larger, highly recommended.
Floppy Drive:  3-1/2" standard
CDROM:  IDE or SCSI (for software loading)
Network Interface Card -  3COM 3C509x (ISA)min; 3C905, 3C905B, 3C905C (PCI) preferred
Video Card - ATI (Mach64) 2-MB VRAM or  more (preferred) or other Hi-Rez Video Card supported by Redhat Linux 6.2;   (1024x768, 256-colors, MINIMUM)

Mouse: 3-button PS/2, Logitech (preferred); 2-button mouse can emulate 3-button operaton
Optional CDR/RW: SCSI (most SCSI models supported by Redhat Linux 6.2)
--- NOTE:   IDE CDR/RW require special setup procedures with Linux, and are NOT recommended.
Optional SCSI Host Bus Adapter HBA -  Adaptec or Tekram, supported by Redhat Linux 6.2
Optional SCSI Tape Drive:  Wangtek 5525ES SCSI (DC6000 Series QIC tape drive)
Case & Power Supply; (250-watts, recommended)
Video Monitor:  15" 1024x768-minimum, 17" or larger recommended
Locally-Attached PRINTER:  Must have POSTSCRIPT capability in hardware, for example: HP-4M, HP-5MP, HP-6MP, HP Deskjet 1600CM, etc. connected to LPT1 port.
(NOTE:  there are a number of non-postscript printers (laser, inkjet, etc.) which will work with the Linux Ghostscript Postscript translator, but I have not seen any yet that plot the outside borders of an IPF Powerflow Plot correctly.   A printer with native postscript is highly recommended).
There are some ways of using Samba on Linux to print from Linux to a Windows NT LAN printer, and they use procedures related to editing the file, /etc/smp.conf, activating Samba SMBCLIENT, and using PrinterTool to set up a Samba/Windows95/NT Printer.  Detailed descriptions of this process will be part of a separate Howto Document.
One alternative is, if there is a LAN-Postscript-Capable printer in close physical proximity to the dual-boot LinuxWindows IPF PC, most printers such as HP-4M, also have the parallel and/or serial ports which will work and switch automatically with the LAN connection.  So, all you need to do is run a parallel printer cable from the Linux PC to the LAN printer.
OPERATING SYSTEM SOFTWARE:  Redhat Linux 5.2, 6.1, or 6.2, required
(Redhat 7.0 not recommended at this time, unless you want to install some additional patches from Redhat. Otherwise, get 6.2 for now, and wait for Redhat 7.1 or 7.2)
MOTIF SDK SOFTWARE:  Motif Window Manager/SDK:  Metrolink Motif Complete! , required.
Note:  This system is set up as "Dual-Boot", by installing the Linux on the added Hard Drive, and booting the Linux with a floppy.  This provides physical isolation between the Linux and MS-Windows partitions and boot files, to minimize any risks associated with corrupted partition tables or other things, as the equipment ages; or attacks by MS-Windows-associated boot viruses.

## Installation on Linux Redhat 5.1, 5.2, 6.1, 6.2
MetroLink Motif Complete for Running IPF ver. 1.04, 4/11/2001
1.  Logon as root, and then run "startx" to start the X-Windows environment and Gnome desktop.
2.  Insert the MetroLink Motif Complete CD.  The Gnome desktop will automatically mount the CD, in most cases. If so, you can skip Step-4.
3.  Open a terminal window, Xterm, and type "cd  /"
4.  Mount the CD by typing, "mount  -t  iso9660  /dev/cdrom  /mnt/cdrom
5.  Change directory to the CD by typing, "cd  /mnt/cdrom"
6.  Get a directory listing of what is there by typing, "ls  -l"
7.  There should be an install script, "install-motif", listed there.
8.  Run the install script by typing, "./install-motif"
9.  When the "Welcome to Metro Link Motif Complete!" window comes up, click "next".
10.  In the next window, read the license, and click the the "I accept ..." button, then click "next".
11.  In the next window, select "Install both Runtime and Development packages".
12.  In the next window, select Motif 1.2.4 as the Primary Motif Version, and click "next".
13.  In the next window, select both "mwm 2.0.1" and "mwm 1.2.4", and click "next".
14.  In the next window, select both "Motif 2.0.1" and "1.2.4" Runtime Libraries, and click "next".
15.  In the next window, select "Motif 2.0.1" as the Secondary Development Environment, and click "next".
16.  In the next window, select Install "libc5...", and click "next".
17.  In the next window, select "RPM" package format, and click "next".
18.  In the next window, click "next" to proceed with installation.
19.  When it is all done with the installation, type "cd  /" to get back off the CD to your hard drive, and then unmount the CD by typing, "umount  /dev/cdrom", and then remove the CD.

## Motif Window Manager for Running IPF Set Up User Login Accounts for Motif
1.  It is helpful to have more than one user account, each running a different Window Manager.  For example, I set up two accounts for myself, one running the default window manager for the particular Linux Installation, and the other one to run Motif, so that I can run IPF. .....  User Login Accounts:  jack and jackm    .... the one with the "m" is set up to use Motif Window Manager.
2.  When you set up a new account to run Motif Window Manager, DO NOT run "startx" in that account until after you have set up the proper start-up files in that account directory.  When you first run "startx", a new .Xauthority file is created, along with other setup files, which are then used by the chosen window manager for that account.
3.  First, login to the new Motif user directory, (e.g. login as "jackm").  Type "ls  -al" to get a listing of all the files, including the "hidden"files.  In Linux and Unix, the hidden files always start with a "."   You will find a file called ".bash_profile".... make a backup of the original version of the file by copying it to a new name:  type "cp  .bash_profile  .bash_profile_original".
4.  There are some Motif user setup files you will need to use.  They can be found on the CD in the file "ipf-disksH.tar.gz".    As part of the preliminary installation, you would have copied this file to the / directory of the hard drive, unzipped and untarred it.  The files you need are named and located in these locations:
/ipf-disks/newdotfiles/dot327.bas /ipf-disks/newdotfiles/dot.mwm /ipf-disks/newdotfiles/dot.xin /ipf-disks/newdotfiles/dot327.fpa /ipf-disks/XGUI
5.  Copy these five (5) files to your "Motif User Account" directory.
6.  You will need to replace the .bash_profile with the new one containing IPF-specific path information.  Do this by typing, "cp  ./dot.bas  .bash_profile".  If it asks if you want to overwrite the existing file, respond with a "y" for yes.
7.  Make the proper versions of the other files by typing,  "cp  ./dot.mwm  .mwmrc",  "cp  ./dot.xin  .xinitrc", and "cp  ./dot327.fpa  .fpanelrc".
7a.  Assuming you have already installed IPF, bring a copy of the IPF XGUI resource file into the Motif user directory by typing, "cp /ipf-disks/XGUI  ."   This can also be done at a later time, if you have not yet installed IPF. Without the XGUI file in your home directory, you will not get the proper colors on the screen when running the IPF GUI graphic user interface.
8.  Log out by typing, "exit".
9.  Log back in as "jackm", type the command, "startx", and you should have a Motif Window Manager desktop in front of you with an mxterm, terminal window open.  You can open another terminal window by clicking on the "tools" icon in the fpanel control panel bar at the bottom of the screen.
10.  If you have IPF-3.27 already installed, you can run it by going to an open mxterm window and typing, the following command:  "gui  &"

## IPF Coordinate Files and Powerflow Plot Title Blocks -- "HowTo"
Ver. 1.00  3/9/2001
1) IPF Coordinate Files:
2) For the .COR files to work properly with IPF on Linux, the .COR file data cannot contain Carriage Returns at the ends of the lines.  .COR files transferred for a DOS or MS-Windows environment, will have had Carriage Returns inserted automatically, and these must be removed.
In order to remove the carriage returns, use the IPF utility, "dos-linux.bash"
Examples of Use:
dos-linux.bash  EWEB.COR <cr> dos-linux.bash  ewebppl.cor <cr> dos-linux.bash  \*.COR <cr>
This utility also renames the files with "UPPER CASE" filenames, to "lower case" filenames.
3) Powerflow Plot Title Blocks/User Comments:
There are some specific steps necessary to properly setup Title Blocks and User Comments on the IPF Plots. Described here is the step-by-step procedure to be followed:
After you have loaded a study case, and loaded either an existing plot coordinate file or created a new one from either the Landscape (L85X11.COR) or Portrait (P85X11.COR), you will want to insert Title Block User Comments and Descriptive Information:
a.  Click "File,Plot Options, User Comments"
b.  Click with the mouse in the white area for Additional User Comments
c.  Do two (2) Carriage Returns <cr> <cr> in order to provide space to not overwrite comments inserted by IPF itself.
d.  Type your comments, using as many lines as you need, each followed by a carriage return <cr>, up to the limits of space available.
c.  Click "OK"
d.  Look at the Title Block on the screen.  You will see that the comments do not yet appear.
e.  Click "File,PlotOptions,PageOptions" and click the "OK" button.
f.  Now you will see your Additional User Comments appear in the title block.  This process is necessary to update the Additional User Comments information, each time you make a change.
g.  Now, click "File,Plot Options, User Comments,ViewPlot"  or  "File, Print Plot" to view the Postscript IPF Plot with the Ghostview Postscript file viewer.
h.  From Ghostview, you can view the plot up close, and print copies as needed.
R.  Problems with Linux finding all of a machine's RAM
Question:
My machine has 128 MB of RAM, however Linux only sees 64 MB of it. What is going on, and how can I fix it?
Answer:
On most systems, the reason is that the BIOS has a limit of how much memory it will tell the OS is present in the machine, even though the board can have more. Common limits seen with this problem are 16M, 32M, 64M, and 128M. To get around this, we need to explicitly specify the amount of memory to the kernel at boot time via the mem=< actual memory goes here > flag.
In the following example, we have a 128M machine but only 64M are being seen by Linux. At the LILO prompt, we type
                LILO: linux mem=128M
After the machine boots, we use the free command to see if the larger amount of memory was recognized by the kernel. If so, we can add an append line to the /etc/lilo.conf file and rerun LILO to make it happen permanently. The example from above could look like the following:      boot=/dev/sda map=/boot/map install=/boot/boot.b prompt timeout=50 image=/boot/vmlinuz-2.2.12-20        label=linux        root=/dev/sda1        initrd=/boot/initrd-2.2.12-20.img        read-only        append="mem=128M"
Do not forget to run /sbin/lilo -v after editing the file.

# Initial Operation
SYSTEM LOGIN ACCOUNTS - (Redhat Linux 6.2 Operating System):
System Administator:
Text Login Screen:  Login  as ( root ) , with password ( xxxxxx )
Start X-Windows graphical user interface (GUI):  by typing  ( startx )
IPF 3.17 User  (login account setup for running IPF and doing Powerflow Studies):
Text Login Screen:  Login as (ipf327user), with password ( yyyyyyyy )
X-Windows Motif Window Manager graphical user interface starts automatically.
Gnome Desktop User:
Text Login Screen:  Login as (gnome-user), with password ( zzzzzzz )
Start X-Windows graphical user interface (GUI):  by typing  ( startx )
System Administrator Access under a User Account:
If you are logged into a User Account, and need to have System Administrator access:
In X-Windows  GUI, open a terminal window:  Login as ( su ) with password ( xxxxxx ).


# Transient Stability Program, TSP-6.07:

## INSTALLATION/SETUP INSTRUCTIONS:

Simple instructions to build and execute BPA's Transient 
Stability Program (TSP) version 6.07.  TSP is dependent upon a successful 
build of IPF-3.27, at least of the batch powerflow program,"bpf".  For 
details, refer to the "readme" file found in the /shr/ipf-3.27 directory.

1. Be logged in as "root" or "su".  Copy the latest available version of the file 
   "tsp-6.07.xxSRC.tar.gz" to the directory /shr. This directory was created in the 
   IPF-3.27 install and build process.

2. Uncompress and untar the file, tsp-6.07.xxSRC.tar.gz.

   gunzip tsp-6.07.xxSRC.tar.gz
   tar -xvf tsp-6.07xxSRC.tar

3. Use "cd" to change to the tsp-6.07 subdirectory:  
   Type "make clean", and then type "make".
 
   This will complete the compile, build, and link process (see details below).

4. Copy the "tsp" executable to /bin or /usr/bin, so that all users can run "tsp"
   from the command line without having to type in the full path.  This is normally
   done by the "make" process, and need not be done manually.

   cd /shr/tsp-6.07/exe
   cp ./tsp /bin


5. Test the results with the small example included in the dat subdirectory.

   cd /shr/tsp-6.07/dat 
            
   bpf test_gov.pfc                   # executes bpf to create the sample
                                      # base case file, "test_gov.bse"

   tsp test_gov.fil                   # executes tsp using "test_gov.fil" as input.

6. In order to change the default TSP page creation size for TSP plots, install the proper
   version of the file "tspmaster.post", which resides in the directory /tsp-6.07/tsp.
   There is a path reference to this file required as part of the "test_gov.fil" or other 
   specific input file you are using.  Two versions reside in /tsp-6.07/tsp:  One version 
   for standard 8.5 x 11-inch Letter paper, and one version for Metric standard A4 paper.
   Copy the one you want to use to replace the existing version of "tspmaster.post". 

7. View and/or print out the resulting multi-page Postscript output/plot 
   file: test_gov.ps using Ghostview "gv" or "lp".

   gv test_gov.ps
   lp test_gov.ps
   
8. Convert the Postscript plot files to Acrobat PDF format, using the Ghostscript 6.50 
   or later conversion program, ps2pdf13.
   
   For Different Paper Sizes, use the following example:
   
   ps2pdf13 test_gov.ps                        [for default standard 8.5 x 11-inch size]
   ps2pdf13 -sPAPERSIZE=a4 test_gov.ps         [for metric standard A4 size]
   
   See the Ghostscript 6.50 or later, "man page" for details of other sizes (man gs).
   
          PAPERSIZE    X inches   Y inches   X cm      Y cm
       -----------------------------------------------------
       a0           33.0556    46.7778    83.9611   118.816
       a1           23.3889    33.0556    59.4078   83.9611
       a2           16.5278    23.3889    41.9806   59.4078
       a3           11.6944    16.5278    29.7039   41.9806
       a4           8.26389    11.6944    20.9903   29.7039
       a5           5.84722    8.26389    14.8519   20.9903
       a6           4.125      5.84722    10.4775   14.8519
       a7           2.91667    4.125      7.40833   10.4775
       a8           2.05556    2.91667    5.22111   7.40833
       a9           1.45833    2.05556    3.70417   5.22111
       a10          1.02778    1.45833    2.61056   3.70417
       b0           39.3889    55.6667    100.048   141.393
       b1           27.8333    39.3889    70.6967   100.048
       b2           19.6944    27.8333    50.0239   70.6967
       b3           13.9167    19.6944    35.3483   50.0239
       b4           9.84722    13.9167    25.0119   35.3483
       b5           6.95833    9.84722    17.6742   25.0119
       archA        9          12         22.86     30.48
       archB        12         18         30.48     45.72
       archC        18         24         45.72     60.96
       archD        24         36         60.96     91.44
       archE        36         48         91.44     121.92
       flsa         8.5        13         21.59     33.02
       flse         8.5        13         21.59     33.02
       halfletter   5.5        8.5        13.97     21.59
       note         7.5        10         19.05     25.4
       letter       8.5        11         21.59     27.94
       legal        8.5        14         21.59     35.56
       11x17        11         17         27.94     43.18
       ledger       17         11         43.18     27.94


COMPILE, BUILD, AND LINK PROCESS COMPLETED BY "make":

1. Build the library libtsp.a.

   cd /shr/tsp-6.07/tsp
   ./build_libtsp.bash

2. Build and link the tsp executable.  

   rm -f ipfinc
   ln -s /shr/ipf-3.27/ipf ipfinc        # sets a soft link to ipfinc
   rm -f tspinc
   ln -s /shr/tsp-6.07/tsp tspinc        # sets a soft link to tspinc
   make -f tsp.make                      # builds the tsp executable
