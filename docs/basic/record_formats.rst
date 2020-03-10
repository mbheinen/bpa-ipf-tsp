**************
Record Formats
**************

Overview
========
This chapter describes all data record formats recognized by BPF. Most, but not all of them are also available in the GUI interface. The record descriptions are organized alphabetically by record ID as you go through the chapter (except for the DC bus records which follow the AC bus records). The table below enables you to quickly locate a specific record format entry. The table also gives you a quick description of each record format.

Each entry has a card image that shows you where to put both identification and data information and where the implicit decimal point (if any) is located. Please note that all information must be in the correct columns; you will experience processing errors otherwise. Each card image has a legend showing whether data is required, optional, or ignored.

Each entry also has a table of column descriptions and data formats. This supplements the card image. Additional text accompanies any record formats that need extended discussion.

Some data are Fortran real numbers. For these fields, the implicit decimal point is shown as a black dot on the card image, just above the information entry row. Note that you can enter data with an explicit decimal point in any position within the field. A black dot that appears at the extreme right side of a field represents a decimal point at the right end (least significant digit) of a number. All other black dots appear above a line, indicating that the decimal point will fall between the digits on either side.

All IPF data record types are identified by the characters in columns 1 and 2. To fully specify a particular record, additional fields need to be filled in. These additional ID fields are usually the bus name and base kV. ID fields are called out in the column description tables.

==== ======================= =========== =================================
Link Record ID (columns 1-2) Field Width Description
==== ======================= =========== =================================
3-7  Period (``.``)          120         Comment (not printed)
3-8  ``+``                   80          Bus continuation
3-14 A                       80          Area interchange control
3-16 AO                      80          Area output sort
3-18 ``-``                   ``-``       General ac bus description
3-22 B                       80          AC bus — load bus
3-25 BC                      80          AC bus — voltage controlled by BG bus
3-28 BE                      80          AC bus — constant voltage
3-31 BF                      80          AC bus — special purpose bus for Newton-Raphson solution
3-32 BG                      80          AC bus — generator
3-35 BQ                      80          AC bus — constant voltage within Q limits
3-38 BS                      80          AC bus — system slack bus
3-41 BT                      80          AC bus — LTC transformer controlled ac bus
3-44 BV                      80          AC bus — constant Q within V limits
3-47 BX                      80          AC bus — attempts constant V using switched Q speciﬁed on X record
3-50 BD                      80          Two-terminal dc bus
3-52 BM                      80          Multi-terminal dc bus
3-55 DA                      80          Delete buses by area
3-56 DZ                      80          Delete buses by zones
3-57 E                       88          Equivalent branch (has extended ratings)
3-60 I                       80          Area intertie I record
3-62 L                       88          Transmission line (has extended ratings)
3-66 LD                      80          Two-terminal dc line
3-69 LM                      80          Multi-terminal dc line
3-72 PO PZ PN PA PB PC PD    80          Factor changes
3-78 QN QP QX                120         Reactive capability curve
3-82 R RV RQ RP RN RM        80          Regulating transformer
3-86 RZ                      80          VAR compensator model
3-88 T                       92          Transformer (has extended ratings)
3-88 TP                      92          Phase shifter (has extended ratings)
3-93 X                       80          Switched reactance (BX record)
3-96 Z                       80          Zone rename
==== ======================= =========== =================================

System Changes
==============
After a base case has been established, it may be changed with the use of change records. The change records are identified as system data records which immediately follow the network-solution qualifier ``/CHANGES`` statement. See ?? for details.

The change records are system data records with a change code in column 3. Each of the input data forms describe the permissible types of changes under the caption CHANGE CODES. In all, there are five types of changes.

 * *Additions*: Change code = blank

   The data record identification must be unique to the system. The contents of the record must be complete as if it were being submitted to build a base case.
 
 * *Deletions*: Change code = ``D``

   Only existing data may be deleted, and only the identification fields are needed. Numerical data in any other field is ignored. Special conditions are given:
 
   * Deleting a bus automatically deletes all continuation bus data, switched reactance data and all branch data associated with that bus. Deleting all branches connected to a bus will result in an error. It is better to delete the bus. A bus should not be deleted and added back in the same case with the same name in a single change file.

   * Deleting all transformer banks between two buses will automatically delete any regulating transformer data.

   * A line composed of sections may be deleted in its entirety by deleting section 0 (zero) or blank. The alternative is to delete each section with a separate change record. If a section is deleted the line is reconnected without that section. Transformer sections cannot be deleted.
   
   * A branch composed of parallel lines between two buses may be deleted in its entirety by entering a * in place of CKT ID. This provides a means of disconnecting two directly connected buses from each other. The branches may also be deleted individually.
   
   * A blank branch ID is legitimate identification.
   
   * Changing a bus from type X to any other type will delete all switched-reactance data automatically. No separate X delete record should be included.

 * *Elimination*: Change code = ``E``

   This causes the elimination of all existing A (area interchange) records to make room for possible new A records. The user should note that this change code works for A records only.

 * *Modifications*: Change code = ``M``
 
   All currently existing system data may be modified, which means changing the value of some quantity of system data. Data in the identification field cannot be changed.
 
   Only the data to be changed is entered on the change record. Nonblank fields constitute data modification, while blank fields indicate that the quantity is not to be changed. A blank and a zero quantity on the change record are distinguishable. Often, it is necessary to change a quantity into a blank. Examples are the bus zone name and the bus subtype. To change these into blank quantities, a 00 or 0 must be entered in the appropriate respective column fields. Special recognition is conferred on these change quantities.

 * *Restorations*: Change code = ``R``

   Previously outaged system data may be reactivated with the use of restore change records. This type change permits data to be restored to the system with change records using the identification fields only. Any data fields on the restore record will be ignored, and the reactivated data assumes the same quantities it had prior to deletion. Restoring data is an option. Data could be re-entered with additional type change records as well. The operations and rules are similar to those for deletions. However, some important differences must be explained.

   * Restore changes are permissible only with data deleted in a previous change case but within the same base case. You cannot restore data outaged in a previous OLD_BASE case.
   * Area interchange records may not be restored.
   * Restoring a bus restores only branches which connect to a viable system base. In some cases, not all of the outaged branches can be restored.
   * Restoring all transformer banks between two buses will not automatically restore any regulating transformer data. If this is intended, the regulating ``R`` transformer data must be restored separately.
   * A restored branch record must match the branch code as well as the identification fields. For example, if the branch is type ``L``, the restore record must match the type.

   The change records are read, interpreted and stored for further processing. As they are read the following are processed immediately:
    
    * Area Interchange (``A``-blank)
    * Zone Renames (``Z``-blank)
    * Area Deletes (``DA``)

   Processing of changes then continues in the following manner:
    
    * Changes Percents (``P``)
    * Deletes
    * Adds/Restores
    * Modifications

.. note:: 
  
  If more than one modification for the same data item occurs, the changes will be made in the order encountered. In IPF, no message will be given the user.

If fatal data errors are encountered in batch mode, switches are set and the processing continues only to discover additional errors, list all changes and exit. For changes to existing data, component identification must be specified exactly as in the base case file.
If fatal errors are encountered in interactive mode, appropriate diagnostics are issued so that the user can remedy the faulty command or data.

Comment
=======
This comment text is used to annotate the program control file, network data file, or change file. Its contents are not added to the output listings nor saved in the binary base case file.

Simply place a “.” (period character) in the first column and the comment in the rest of the columns. This is also handy to temporarily deactivate a command or data line.

.. image:: ../img/Comment_Input_Format.png

====== ======== ====== =============================
Column ID Field Format Content
====== ======== ====== ==============================
1      yes      A1     .(period)
2-120  no       A119   Text string for record comment
====== ======== ====== ==============================