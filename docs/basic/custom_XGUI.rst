******************
Custom GUI (XGUI)
******************
X resources are X system components managed in common by the X server. Examples of resources
are colors, fonts and their characteristics, default size and position of windows, and default file
names for dialog boxes. The IPF ``gui`` allows you to change many of these resource values.

You customize IPF ``gui``, as you do other X clients, by changing the client's X resources file. This is the
XGUI file in your home (logon) directory. It is an ASCII text file, so you can alter it with any text
editor. It is recommended that only advanced users edit XGUI to any extent; when making changes
to X resource values it is very easy to cause problems that you won't know how to fix. For example,
if you accidentally changed some branch colors to the main window's background display color,
you will not be able see those branches, even though they are still there! Caution is urged when
you modify X resource values and specifications.

However, there are a few items that anybody can and will probably want to change. These are
covered below. 

.. note::  
  
  After you make changes to the XGUI file, you may not see changes in your client (IPF) until you exit the X Window System itself. Exiting and
  restarting IPF may or may not be sufficient.

Changing Open File Defaults
===========================
Although you can always find your way to the file you want to open by using the Files filter system,
it is much nicer to have IPF come up with the paths and/or names you most commonly use. Below
is the section of the XGUI file which you will want to alter in order to do this. Open XGUI with
your text editor and forward search for "DEFAULT MASKS"

.. code::

  ! These are the DEFAULT MASKS in the open file menu.
    
  XGUI*open_dia_command_dir_text.value: *.pcl
  XGUI*open_dia_change_dir_text.value: *.chg
  XGUI*open_dia_base_dir_text.value: *.bse
  XGUI*open_dia_network_dir_text.value: *.net
  XGUI*open_dia_coord_dir_text.value: *.cor
  
  ! These are the default filenames in the open file menu.
  ! Leave blank where you don’t want a default.
  ! Don’t set default filenames for both base and network!!!
  
  XGUI*file_select_dia_command_text.value:
  XGUI*file_select_dia_change_text.value:
  XGUI*file_select_dia_base_text.value:
  XGUI*file_select_dia_network_text.value:
  XGUI*file_select_dia_coord_text.value:
  ###########################################################################

When you first execute IPF and select the Open command from the main window menu, some
default file selections fill the file text boxes under the Select buttons. These specify the default
filters to be applied.

**To change a default filter mask**:
For example, the default coordinate file mask in the XGUI file for the Open dialog box is:

.. code::

  XGUI*open_dia_coord_dir_text.value: *.cor

This assumes that you are executing ``gui`` from the directory where the data files reside, and that you
will be selecting the coordinate file you want to load from among files in this directory that end
with ``.cor``. (The asterisk (*) is a Unix wildcard character meaning "any arbitrary length string of
characters."

To make the filter default to a different directory, insert (for example) ``/archive/ipf/dat/``:

.. code::

  XGUI*open_dia_coord_dir_text.value: /archive/ipf/dat/*.cor

Now the coordinate files selected are in the ``/archive/ipf/dat`` directory. The pathname you
specify can be absolute or relative; just be sure that it specifies a valid directory on your system.

**To change a default filename mask**:

The default base case and coordinate file masks in the XGUI file for the Open dialog box are
specified with these two lines:

.. code::

  XGUI*file_select_dia_base_text.value:
  XGUI*file_select_dia_coord_text.value:

The filenames are currently blank, meaning that no files are loaded when ``gui`` starts up. If during
a study you are working continuously from a particular base file, say ``../base/97HS4.bse``, you
might want to edit XGUI to make this file, and its associated coordinate file, load automatically
every time you start ``gui``. You then can begin work immediately, without having to go through the
Open Files menu.