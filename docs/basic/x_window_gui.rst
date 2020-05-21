.. _x-window-graphical-interface:

****************************
X Window Graphical Interface
****************************
This section presents information about the X Window graphical user interface (GUI) for IPF.

IPFâ€™s GUI interface makes data entry and modification easy. It also simplifies the running of base case solutions and the printing of network diagrams. This guide shows how to use the major features of IPF. Users who need details about data input record formats or system models should consult the :ref:`record-formats` section.

IPFâ€™s GUI uses the X Window System and the OSF/Motif window manager interface. The graphical user interface features an interface that will be familiar to people who use an X Window System before, but it will feel a bit cumbersome compared to modern day applications.

Audience
========
You will be expected to already know the basics of power flow programs in general. You will find that small changes in bus and branch values are  easier to make in the GUI than hunting through ASCII text files. For example, users will get a feel for how changes to the input data affect the solution voltages in a network much more quickly with IPF than by using a batch style interaction.

User Interface
==============
The X Window System and the OSF/Motif window manager use certain descriptive terms for actions performed with the mouse. These terms are defined here. In addition, some keyboard actions are also defined. See the table below.

.. table:: User Interface Terms

  ============ ==================================
  Term         Meaning
  ============ ==================================
  Select       Perform a mouse (or keyboard) action that singles out a user interface object. Usually the object is highlighted in some fashion, such as reverse video for menu selections. Subsequent commands or operations are performed on the selected object
  Select Text  Click the left mouse button inside a text box. If the text is modifiable, the cursor will change to the text cursor (I-beam) cursor, which can be moved to edit or enter text in the box
  Press        Hold down without releasing a mouse button or key.
  Click        Press and release a mouse button in one quick action.
  Double-click Click the same mouse button (left, middle, or right) two times rapidly without moving the mouse between clicks.
  Drag         Press and hold a mouse button, move to a new location and release the button.
  Type         Type all the keys called for without pressing Return at the end.
  Enter        Type all the keys called for and press Return at the end.
  Scroll       Drag the bar at the right of the window up or down to move through a display. Some windows also have a bar at the bottom with which you can scroll right and left.
  =========== ==================================

Keyboard Conventions
--------------------
The following conventions are used for key strokes. Generally, a hyphen (-) connects key names that should be pressed and held, starting from left to right. For example, the key sequence Control-c Shift-E means to press and hold the Control key and then press c. Release this combination and press and hold the Shift key and then the E key. Then release these keys. A different example: Esc f means to press and release the Esc key followed by the f key.

In general, the mouse operation and keyboard operation follow the conventions of the Motif interaction style guidelines. These are found in the OSF/Motif Style Guide. Many aftermarket books about Motif cover these conventions also.

X Window System
---------------
This chapter provides a quick introduction to the X Window System and X window managers with emphasis on the OSF/Motif window manager. The treatment here is certainly not exhaustive or even complete. But it is intended to give you enough background to successfully use the Interactive Powerflow (IPF) program.

If you have not used an X Window based GUI before, be sure to go through this chapter for some pointers so that youâ€™ll be headed in the right direction. If you would like more information, refer to the following books about the X Window System and OSF/Motif.

  * Open Software Foundation. *OSF/Motif Style Guide Revision 1.1*. Prentice Hall, 1991. This provides the official description of OSF/Motif look, feel, and behavior for OSF/Motif software developers. Though not oriented toward OSF/Motif users, this book does give precise descriptions of all OSF/Motif components and behavior.
  * Quercia, Valerie and Tim Oâ€™Reilly. *X Window System Userâ€™s Guide OSF/Motif Edition*. Oâ€™Reilly & Associates, Inc., 1991. This is a good, general introduction to X and OSF/Motif.

Broadly speaking, the X Window System is designed to deliver mouse-driven menu/window user interface applications over a local area network.

The X Window System specifies that the "look and feel" of its user interface be "policy free."" Because of this, programmers are free to create their own look and feel within broad limits. Over the past few years, Sun Microsystems, AT&T, and the Open Software Foundation have all created GUIs for the X Window System with a distinct look and feel. The Open Software Foundation offers OSF/Motif.

Like most large software systems, X and its environment have a jargon of their own. Here are a few terms you should know:

Server

  The part of X resident in your local computer (or X terminal) memory. The server has three main tasks. First, the server takes care of communicating with the mouse and keyboard. Second, it takes care of managing X resources, such as fonts and colors. Third, it communicates with X applications.

Client 

  A stand-alone X program. Clients are the X programs that you use to accomplish your work, such as drawing graphs, preparing text, making power flow calculations, etc. Clients usually reside in a computer across the local area network, but they can also reside in the same memory as the server itself. Clients and servers communicate through a special language (â€œprotocolâ€) that is especially efficient for communication over a Local Area Network (LAN).

Resources 

  X components that are held and managed in common for X clients by the server. X resources reside in your local computer (or X terminal) memory just like the server. X resources are things like fonts, color â€œpixmaps,â€ font information, etc.

Window manager 

  A special X client that handles the window services of other X clients. There are a number of different X window managers, not just one, but you only use one window manager at a time.

X terminal 

  A special purpose computer having a built-in X server, and connection hardware and software for a LAN.

LAN 

  A Local Area Network usually based on the hardware and low-level software standard of Ethernet. For Unix computers, the software communication protocols are usually based on the TCP/IP standard. PC networks may use different hardware and software network communication standards.

The GUI portion of IPF is a server; the powerflow portion is a client. These two programs may reside on different computers, in which case the GUI will be running on the machine you are sitting in front of, and the powerflow will be running on a workstation somewhere else, perhaps a network server. Note that the X Window terminology for "client" and "server" is reversed from the network communication terminology. It is also possible for both client and server applications to be running on the same computer.

.. figure:: ../img/A_Server_Some_Clients_and_a_LAN.png

  A Server, Some Clients, and a LAN

The figure above shows you the basic architecture and communication model of the X Window System. Something you should note is that the client (application) program may be physically residing on a completely different computer from the one that your keyboard, mouse, and display are attached to. To access the remote application, you only have to know the name of the computer your client is on. (You also must have permission to use the other computer, of course, and it must be connected properly to the LAN.) The X Window System was designed from the ground up to run in a distributed computing environment.

When you are running a client, such as IPF, over the network, in contrast to running it in your own computer’s local memory, you will ordinarily notice very little performance degradation due to network traffic, though there may be some depending on how busy the network is. X is designed to minimize network communication.

The server side of X resides in your own computer’s (or X terminal’s) memory. There is one X server for each user’s keyboard, mouse, and display. The server is dedicated to you. However, your server may communicate simultaneously with many different X clients, not just IPF. And again, these other clients may be anywhere out on the LAN. Thus, in any one X session (between “login” and “logout”), you may run many X clients on many different computers. The server manages all this.

When your client requests a certain font, the server delivers it. When your client requests different colors for graphical objects, the server consults the color map for that client and delivers the correct colors. Fonts, colors, and certain other server-managed software components are termedresources. Some resources such as colors and fonts you can change in your own account’s IPF resources file, XGUI. See ?? if you would like to learn more about this.

A Summary of Motif Basics
=========================
IPF is designed to run and look best with the Motif window manager. This section introduces you to some Motif window manager basics. However, for detailed information, turn to the books cited at the beginning of this section.

Motif Windows
-------------
Motif windows are rectangular areas of the display. Various Motif components surround a central area where text and/or pictures appear. See example below. These components are controlled by the Motif window manager. However, the central area is controlled by an X client, which is a completely separate program from the Motif window manager. IPF ``gui`` is an X client, so it controls only the interior of Motif-managed windows. Because of this separation, you may notice that IPF’s windows can still be moved, iconized, etc., even though the IPF client may not be responding.

In the figure below, the Terminal Emulator client controls the central display (where the text is), the Menu Bar, and the Scroll Bar. The Motif window manager (client) controls all the rest of the window.

.. figure:: ../img/Motif_Window_Components.png

  Motif Window Components

Motif Resources
---------------
Like the X Window System, Motif has resources that you can change. Resources are system controlled components such as fonts, colors, initial size and position of windows, etc. Many X clients (application programs) have customizable resources. Since the Motif window manager is just another client, it has customizable resources, too.

Resources can be changed in two ways:

  * Change the dedicated client resource file with an ASCII text editor. On Unix systems, the Motif resource file is named ``.mwmrc`` and the X resources file is ``.Xdefaults``
  * Use a dedicated X application that provides a regular GUI interface for changing the resource file.

Since interpreting the meaning of the resource specifications is not always easy or straightforward, it is recommended that you look for a dedicated X client for changing resources. Your Motif system probably has this X client already available, so that all you have to do is choose the Motif resource editor as a command on a system menu. Possible names to look for are System Setup, Configuration, User Preferences, or something similar. The editing of the many resources may be broken out on your system menus as separate commands, such as Colors, Fonts, Sizes, etc.

As a second choice, use a Motif book to help you interpret the meaning of the resource specifiers in the ``.mwmrc`` file and edit them with an ASCII editor such as the vi ASCII editor. Nearly all Unix systems have vi.

Common Windows Tasks
--------------------
This section goes through a few common window and menu tasks to give you a feel for the Motif interface. Some tasks can be done through a menu command and through direct manipulation of a graphic component. And, in addition, many tasks can be accomplished through a sequence of keys without recourse to mouse movement or button clicks. The following brief descriptions concentrate on direct graphic component manipulation since this is usually the quickest way to get something done in Motif.

To move a window
^^^^^^^^^^^^^^^^
Motif windows have a title area at the top. The window shown above has "Terminal Emulator" in the title area. You move Motif windows by "grasping" the window in this area and dragging it to a new location.

  1. Move the mouse cursor to any point within the title area.
  2. Press and hold the left mouse button.
  3. Move the window to another location of the display. You will note that an outline box of the window shows you the dimensions of the window as you are moving.
  4. Release the button. The window appears at the new location.

To reduce a window to an icon
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Icons are small, rectangular graphic objects that represent the main windows of Motif applications. Icons have many, but not all of the same attributes as windows - for example, they can be moved like windows. Icons are used to organize the display and reduce clutter. You are free to put icons wherever you like on the display. Some Motif systems organize icons in a special window, which looks like a desktop.

  1. Click the Minimize button at the top right of a window. This shrinks the window to an icon and automatically places the icon at a predetermined place on the display. (Note that this place may be obscured by other windows!)
  2. Move the icon as you would a window by placing the cursor over the icon, pressing the left mouse button, moving, and releasing the button.

To change an icon into its window
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
An icon can be changed back into its windows by double clicking on it. The window will "remember" where its previous position was. Sometimes the timing between clicks is important, so make sure you do it fast enough.

  1. Find an icon (or create one) and move the mouse cursor over the icon. Double-click the left mouse button. Be sure to keep the mouse cursor stationary between clicks; otherwise, Motif may interpret your actions as a “move icon” operation.
  2. If you find that a menu pops up, select the Restore command by moving the cursor over the word and clicking once.

To resize a window
^^^^^^^^^^^^^^^^^^
Motif windows have a narrow border that acts as a “handle” for resizing operations. There are eight parts to the border — four corners and four sides. The corners are used to resize simultaneously both adjacent sides, and the sides are used to resize just one side at a time. The following procedure describes a common resizing operation.

  1. Move the mouse cursor over the lower right corner. Note that this corner is demarcated by two cross cuts on the border a short distance away from the corner. The cursor may change its appearance when it is in this area, indicating that it is in the right position.
  2. Press and hold the left mouse button on the lower right corner.
  3. Move the mouse to the inside or outside of the current window.
  4. Release the mouse button when you are satisfied with the size.

The opposite (upper left corner) remains stationary while you move the mouse around. A border line for two sides shows up to give you an idea of the size of the window as you move. Also, a pixel counter of the vertical and horizontal dimensions shows up to give you feedback if you need it.

If the corner or side the you want to grab is not visible on the screen, move the window until it is.

To enlarge a window quickly
^^^^^^^^^^^^^^^^^^^^^^^^^^^
The Maximize button in the upper right corner is a quick way to enlarge a window to the maximum size of your display. 

  1. Move the mouse cursor over the Maximize button in the upper right corner of a window.
  2. Click the button. Note that the window now covers the maximum area of the display. (All other windows should be covered.) You can resize the window using the directions above.

To pop up a window’s menu
^^^^^^^^^^^^^^^^^^^^^^^^^
All Motif windows have a minimum set of window management functions available in the upper left corner via the Window Menu button. Many of the functions described above are available through this menu. Some additional ones are there too, such as Close, Restore, and Lower.

  1. Move the mouse cursor over the Window Menu button in the upper left corner.
  2. Press and hold the left mouse button. Note that a menu pops up. Select one of the commands on the menu by moving the mouse cursor over the command and releasing. Only commands that are clearly visible are selectable. Those which are “grayed out” are not accessible or appropriate in the current context.

Closing a window causes the window to disappear from the display, so be careful because it can also quit the application. In the beginning, you may not know how to restart an application! (Look for a Motif window called the Session Manager. Your system operator has probably set up your account with a Session Manager. See if you can find the closed application on a menu in your Session Manager, so that you can re-open the application if you accidentally close it.)

Lowering a window means to cause it to go to the bottom of the windows “stack.” Think of Motif windows as pieces of rectangular paper on a desk. The ones that overlap have an order from top to bottom. The Lower command causes movement of windows in the stack from top to bottom. Experiment with the command to see how it works. You bring “buried” windows to the top by pushing visible ones down with the Lower command. Note, however, that if you see any piece of a Motif window, you can just click on or inside the border to bring it immediately to the top.

IPF as an X Client
==================
IPF is built on top of the X Window System and uses the facilities of an X window manager of your choice. (However, the recommended window manager is Motif.) In the figure below, you can see that both the X server and the GUI part of IPF reside on the user’s computer. Another client that is always present on the user’s computer is the user’s window manager. This is not shown in the figure below, however. In most configurations, the "engine" (``ipfsrv``) part of IPF resides on the user’s computer, but may, as shown below, reside on some other computer across the LAN.

When you start up IPF, the GUI initializes itself, initializes ``ipfsrv``, and then brings up IPF’s window interface.

.. figure:: ../img/Powerflow_GUI_Communication.png

  PowerFlow/GUI Communication

IPF X Window GUI Architecture
=============================
The figure below shows a high-level view of the IPF X Window GUI and its environment. IPF is divided into the GUI code, which is written in C, and the powerflow code, which is written in Fortran. When you run the X Window System and Motif, you are using the Motif and X libraries in addition to Unix operating system calls and IPC (Interprocess Communication) calls. The GUI part of IPF (``gui``) uses the Motif library and IPC calls. The powerflow part of IPF (``ipfsrv``) uses IPC calls.

Because the Motif library is used for IPF, the Motif window manager is recommended for running with IPF. You can use another window manager with IPF, but the program will not have a consistent look and feel. The functionality remains the same, but you will notice a visual style within the IPF client that is different from the window manager, which controls the display outside the IPF windows and dialog boxes.

.. figure:: ../img/Powerflow_GUI_and_System_Software.png

  Powerflow/GUI and System Software

History
=======
Some key developments took place in the late 1970s and 1980s that made possible the X Window System and the X window managers.

  * The idea of the graphical user interface (GUI) was created, implemented, and became popular.
  * Personal computers became powerful enough to handle the data and processing intensive GUI.
  * Efficient, fast, inter-computer communication - the local area network (LAN) — became widespread.
  * A general purpose, non-proprietary operating system - the UNIX operating system — achieved wide acceptance.

All of these developments contributed materially to the possibility and, indeed, the eventual widespread acceptance of the X Window System and X window managers.

In the late 1970s, medium-sized computers began to get powerful enough to handle information organized graphically rather than in a character-based (textual) way. They also began to get cheap enough to be dedicated to one person. It was on one of these computers that the Xerox Corporation installed the very first GUI using a mouse, menus, and windows. This GUI borrowed heavily from the Smalltalk-80 user interface, which was also invented at Xerox. (Smalltalk-80 is an object-oriented programming environment and system.)

GUI - graphical user interface - simply means using the graphics capability of a computer as the primary mode of interacting with users. A few GUIs do not, in fact, use a mouse, menus, and windows. However, GUIs using this combination of elements became common early because of their inherent ease of use.

Meanwhile, truly affordable personal computing was taking off in the late 1970s and early 1980s with the Apple and then IBM PC computers. These computer architectures were essentially graphical in nature, especially the Apple, though their interfaces were still character-based.

In 1984, Apple introduced the first personal computer with a thorough-going GUI. This was the Macintosh. This computer introduced wide numbers of people to a very easy to use graphical interface. It showed many computer manufacturers that they needed to design with GUIs in mind. A few years later, Microsoft Corporation retrofitted a windowing system onto MS DOS IBM PC-compatible computers.

Also in the late 1970s and 1980s, Sun Microsystems, Hewlett-Packard, DEC, and other companies saw the need for powerful computing "workstations" that business, research laboratories, and government could use. These workstations became common where personal computers didn’t have enough power. However, they generally lacked GUIs, which made them harder to use and less versatile than they might have been.

Another element in the computing picture was also developing in the 1970s and 1980s. This was widespread inter-computer communication. The idea of the local area networks (LAN), which was a room-to-room and building-to-building communication network, was created and implemented. A particularly popular LAN was, and is today, the Xerox-created Ethernet. This LAN is simply a cable connecting computers, whereby the computers can request and send just about any kind of data, often organized as files.

Another computing environment element was the development and distribution of a general purpose operating system that was platform independent, the UNIX system developed in the 1970s and 1980s at AT&T's Bell Labs. This operating system was distributed freely to many college campuses, and the University of California at Berkeley developed many extensions to UNIX, among them sockets which provide efficient communication over LANs. Today, the UNIX operating system is offered commercially by AT&T as UNIX System V.

To complete the computer environment picture of the 1980s and 1990s, engineers at the Massachusetts Institute of Technology created the X Window System, often called just "X." This GUI and underlying software was designed for powerful workstations with graphics-oriented hardware architecture. These capabilities are now available on high-end PCs as well. It is also designed for computers that make heavy use of a LAN. Though not limited to the UNIX operating system, the X Window System was first developed on UNIX computers.

Working with the GUI
====================
This section describes how to accomplish basic tasks in the GUI version of IPF. After reading through this chapter, you should be able to use the Concepts and Commands section to figure out and work with the rest of IPF's features.

The material in this section is not written as a complete, start-to-finish tutorial. Rather, the material is organized by individual task. However, the various topics are organized so that they reflect the common order of tasks in a typical session. So, you can either read the topics and do the steps within the topics in the order presented, or you can skip around and try out specific topics as they interest you.

.. note::

  Most of the task procedures in this chapter involve using the mouse and its buttons. When you are asked to use a mouse button, the left-hand button is meant unless you are explicitly directed otherwise.

The main menus and toolbox are the keys to IPF's main functions. The main menus contain commands that allow you to open files, save files, print network diagrams, solve cases, get help, edit bus and branch data, and so forth. The toolbox contains icon buttons that move you into and out of various display modes. They allow you to create new buses, move buses, bend branches, reduce or enlarge the display, modify bus or branch data, and so forth.

The Display Mode buttons determine whether the map shows the intersection of the currently loaded base case file and the currently loaded coordinate file, or everything in the coordinated file. The current files area tells you which files you currently have loaded. The branch color key indicates the base kV rating of branches shown on the display, or their overload status.

In a prototypical session, you would generally follow this scenario:

  1. Load a solved system and/or a network data file, and a coordinate file via the main menu Files - Open command.
  2. Make changes to the system and/or coordinate file data using the various toolbox icon buttons, which allow direct on-screen manipulation of bus icons and branches, or bring up dialog boxes for adding data to bus and branch records.
  3. Solve the new case with the main menu  Process - Solve command.
  4. Save your new case including its solution data with the main menu  Files - Save command.
  5. Print out a network diagram with the main menu  Files - Print Plot command.

You might also do the following:

  * Run a CFLOW program with the main menu Process - Auto CFLOW command.
  * Get some help along the way with the main menu Help command.

.. figure:: ../img/Main_Window.png

  Main Window

.. figure:: ../img/Toolbox_Icons.png

  Toolbox Icons

Starting IPF
============
IPF is an X Window System application and is started like any other X application. However, your underlying operating system and the window manager you are running offer some ways to simplify how you start up IPF. Essentially, there are three ways you can start up IPF:

  * You type the IPF command name ``gui`` in a terminal emulation window.
  * You select IPF on your window manager's "run applications" menu. Generally, this involves clicking a mouse button on the background to bring up the "run applications" menu.
  * You find that IPF is started automatically when you log in to your account. This means the system administrator has already set up your account to do this. You should see IPF as an icon or open window after the login process is complete.

To start IPF from an X terminal emulator window:

  1. Make sure the X Window System and your window manager are running. Consult with your system administrator if you don’t have X running.
  2. Find a terminal emulator window or open one from a window manager menu.
  3. In the terminal emulator window, enter gui. Within a minute or less, depending on the performance of your computer system, you should see the IPF main window appear.

Exiting IPF
===========
When you are through editing the currently loaded base case or coordinate files, running a solution, etc., and have saved your work, choose the Exit command from the File menu.

  1. Click the File menu and select the Exit command. You will see the Exit dialog box come up. Do not use the window menu button (upper left) to close the IPF window.
  2. Click OK if you are sure you want to exit IPF. Click Cancel if you have changed your mind and want to keep IPF running. If you select OK, IPF closes all its windows and removes them from the screen. You will still be in the X Window System, and you can proceed to any icon or open window to continue working with other X clients. If, after exiting IPF, you decide to run IPF again, see Starting IPF, above.

.. _opening-files:

Opening Files
=============
The Open dialog box shows you the five kinds of files you can open in IPF: the command, change, base case, network data, and coordinate files. See the figure below. For detailed information about these files, see :ref:`powerflow-command-language` and :ref:`record-formats`.

For most power flow studies, the base case (binary) coordinate files are used. However, network data files must be used to initially create a binary base case file. Change files are used to make changes to a base case file; this case is called a change case, and the results are typically saved as a new base case file. Command files are :ref:`powerflow-command-language`.

Unlike the command, change, network data, and coordinate files, which are ASCII text files, the base case file is a binary memory image file. The base case file contains only power system data, which is edited within IPF, whereas the command, change, and network data files may be editedoutside IPF using any ASCII text editor.

The coordinate file is a combination of bus position and branch bending point data in addition to plotting data. The coordinate file bus position and branch bending point locations can be altered by moving them in the GUI display, and a new coordinate file saved if desired. The plotting data can also be edited outside of IPF with an ASCII text editor.

Displaying a Network File
-------------------------
Normally, you will want to load a coordinate file in addition to the network data or base case file you intend to work on. If you do not, you will not be able to display the system graphically in the IPF main window. A system data file needs coordinate position information to display itself in IPF. It must get this information from a coordinate file since it does not have this information within itself. However, if all you want to do is edit a coordinate file, you can load just that file, and it will display properly in the main window.

If you load only a system data file, you can use only the textual editing and report capabilities of IPF to see your data, but you can still solve, make changes, save cases, etc. Also, you can generate a network diagram on the fly from which you can graphically navigate or display results. See :ref:`exploring-base-case-connectivity`.

To open a network ﬁle:

  1. Choose Open from the file menu in the IPF main window. You will see the Open dialog box as shown in Figure 3-3.
  2. Find Network Data File at the left of the dialog box. The button, information box, and file text boxes under the heading allow you to select a network data file. The file text box under the Select button holds a file selection string. The string displayed when you first open the Open dialog box comes from a default that you can set in your XGUI file. See Appendix A, Customizing IPF.
  3. Click the Select button. Notice that the file selection string is transferred to the Filter file text box at the right. Also, the Files list changed to reflect the Filter criteria.
  4. Scroll the Files list to find the file you want to load.
  5. Select the file by clicking it. This action puts the selected file in the Selection file text box below the Files list.
  6. Click the Apply button to put the file name you have selected in the Network Data File file text box at the left side of the Open dialog box.
  7. Click the Load Selections button at the bottom of the left side of the Open box. This final action actually loads the selected file into IPF’s memory.

Since step 7 causes the open files dialog to close, it is best to perform steps 1 to 6 for each of the different kinds of files you want to load, and then click Load Selections at the end to load all files at one time. Note that you cannot open a network and a base file. Only the last one you picked will be loaded.

To open a command, change, base case, or coordinate ﬁle:

To open these files, perform the steps above. All of the essentials are similar; only the file type is different.

Saving Files
============
You can save five kinds of files in the X Window GUI: change files, base case files, network files, coordinate, and stability interface files. Ordinarily, you save files after you are done with a work session, but you can save a file at any time. Change, network, and coordinate files are saved in ASCII text format. Base case files are saved in binary format. Stability interface files can be saved in either binary or ASCII format. In a binary file, a memory image of base case data is written out to a file. See :ref:`overview` for more information about these files.

To save a ﬁle:

  1. Choose Save from the File menu in the main IPF window. You will see the Save dialog box as shown in the figure below.
  2. Change the name of the file you are saving if you do not want to overwrite an existing file.
  3. Click the appropriate Save button to save the desired file.
  4. If the file you are trying to save has the same name as an existing file, you are presented with the Overwrite warning dialog box. You can choose Overwrite to complete the save or Cancel to cancel the save. Note only one base per file is allowed.

.. figure:: ../img/Saving_a_File.png

  Saving a File

Changing the Displayed Network Size
===================================
You can use the X Window scroll bars on the right and bottom edges of the network display window to see a different chunk of the network diagram. The X Window GUI also includes an enlarge (and reduce) displayed network feature. Two buttons in the toolbox control this. X Window GUI has three displayed network size options. When you first load a base case or coordinate file, the middle size is chosen by IPF. If you find you would like to see more detail in a network diagram, you choose the Enlarge button. If, on the other hand, you would like to see the overall picture, you choose the Reduce button. The Enlarge button doubles the displayed network size, and the Reduce button halves the image size.

All other toolbox operations work no matter what the network size is, so you are free to work with the size that best suits the task at hand.

.. figure:: ../img/Displayed_Network_Sizes.png

  Displayed Network Sizes

To alter the displayed network size:
  
  1. Make sure you have a coordinate file loaded. See Opening Files to find out how to do this.
  2. Click the Enlarge button in the toolbox in the main window. This magnifies the displayed network by two times. Note that you are now at the top magnification, 2.0.
  3. Click the Reduce button. The displayed network returns to its original size (1.0), which is the size created by an Open operation. Click the Reduce button again. The displayed network size decreases by half. This is the smallest size, 0.5.

Editing Base Case Data
======================
Editing a base case file is one of the primary activities in the X Window GUI. This involves a number of tasks: adding new buses or branches, modifying existing bus or branch values, and deleting existing buses or branches. These tasks are all accomplished in the main window toolbox and display area. 

Adding a Bus and Related Components
-----------------------------------
You can add a bus to a resident base case or to IPF itself with no base case loaded. The procedure is the same for both. Adding a bus means in effect to create a new bus record within IPF’s memory. You can add as many as you want up to IPF’s maximum limit. Bus-related components are such items as continuation, transformer, line, and Q records. The method of adding these components is very similar to adding a bus record.

To add a bus:
 
  1. Make sure you have a base case loaded if you want to add a bus to an existing base case file. See :ref:`opening-files` to find out how to do this.
  2. Click the New Bus icon in the toolbox in the main window. This puts you in the Add Bus mode and brings up the Bus Input Data dialog with a dummy bus name. Change this to whatever you want, and add data.
  3. Move into the display area where you will note that the cursor changes to a box to remind you that you are in New Bus mode. Click anywhere in the display area. Note that a bus icon with an the new name appears at the point where you clicked.

Modifying a Bus
---------------
Modifying a bus means to alter any of its associated values as they exist in the currently loaded base case data. This is done through the Input Data Edit Box. Refer to :ref:`bus-and-branch-editing` to find out more about the Input Data Edit Box.

To modify a bus:

  1. Make sure you have system data loaded. See :ref:`opening-files` to find out how to do this.
  2. Click the Input Data Edit button in the toolbox. You are now in Input Data Edit mode.
  3. Find a bus you want to modify, move the cursor over the bus, and click the left mouse button. The Input Data Edit Box appears with data text boxes filled with the values associated with the bus you clicked.
  4. Change any of the values to new ones or type new values in blank text boxes. See the :ref:`record-formats` for information on models and values. When you have values correctly typed (there is some data entry checking), click the Apply button at the bottom of the box to modify the bus data in the resident base case data.

Adding a Branch
---------------

.. _exploring-base-case-connectivity:

Exploring Base Case Connectivity
--------------------------------
Sometimes you may want to explode a portion of a large network to see how buses are locally interconnected. The idea is that you start with one bus on the display and find out what other buses are connected to it. Then, with each of these buses, you find out what buses are connected to these, and so on.

The Explode icon in the toolbox allows you to explore base case connectivity.

To explore a base case:
1. Load just a base case file to demonstrate this function. See Opening Files to find out how to do this.
2. Click the New Bus icon in the toolbox to enter Add Bus mode. The Input Data Edit box will come up; just close it without entering any data.
3. Select Alpha Search on the View menu. In the Alpha Search dialog box, type the first few letters of a bus name. The alphabetical list automatically scrolls to find the bus of interest in the list. See the Alpha Search command entry in Chapter 4 for more information.
4. Click the bus name of interest. It may already be highlighted, but you must click on it to make it the currently selected bus.
5. Move to the blank display area. (You should still be in Add Bus mode.) Click once to make the bus icon and name appear.
6. Move back to the toolbox and click the Explode icon. You previously established a bus that you can now explore bus connectivity with.
7. Click the bus. You will note that any buses that are connected to your bus of interest are now shown on the display with connecting lines representing branches. You can continue exploring the network by clicking any new buses that show up. You can reveal the entire network in this way if you like (although it will probably look like a mess!)
The buses and branches are positioned by an internal algorithm since you have not loaded any underlying coordinate data. You can click the Move icon and then move the buses around the screen if you want to clean things up.

Sectionalizing a Bus
--------------------
Sectionalizing a bus separates a bus into two buses and rearranges its branches between the two buses. You can also optionally create a tie line between the two buses. See figure below.

To sectionalize a bus:

  1. Make sure you have system data loaded. See Opening Files to find out how to do this.
  2. Click the Input Data Edit mode button in the toolbox. You are now in Input Data Edit mode.
  3. Select a bus by clicking it. This brings up the Input Data Edit Box. At the center bottom of the dialog box is the Sectionalize button. Click it to cause the Bus Sectionalize dialog box to appear. Note that the bus name of the currently selected bus appears in two places.
  4. Type a new bus name over the existing Bus 2 name to create a new bus.
  5. Click any branch, transformer, etc., record in the left-hand scrolling text box to transfer it to the right-hand scrolling text box. Note that you can go back and forth by clicking the appropriate records till branches, transformers, etc., are all associated with the bus you want.
  6. Click the OK button to send the sectionalized bus data to the resident base case data. If IPF detects any errors or inconsistencies, it puts up the Error dialog box. Examination of the message helps you figure out the problem.

.. note::

  If the name you type is not accepted, IPF has found it to be a duplicate name and rejects it. If, at any point, you would like to start from the beginning, just press Reset at any time. This returns all values to the state they were at the time the dialog box first opened.
  
  7. Sometimes you may want to connect the old and new buses making up the sectionalized bus. Click on the Bus Tie button to create a line with impedance of 0.0 + j0.00001. You may modify this line later, if you wish

.. _bus-and-branch-editing:

Bus and Branch Editing
======================