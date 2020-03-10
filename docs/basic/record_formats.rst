**************
Record Formats
**************

Overview
========
This chapter describes all data record formats recognized by BPF. Most, but not all of them are also available in the GUI interface. The record descriptions are organized alphabetically by record ID as you go through the chapter (except for the DC bus records which follow the AC bus records). Table 3-1 enables you to quickly locate a specific record format entry. The table also gives you a quick description of each record format.

Each entry has a card image that shows you where to put both identification and data information and where the implicit decimal point (if any) is located. Please note that all information must be in the correct columns; you will experience processing errors otherwise. Each card image has a legend showing whether data is required, optional, or ignored.

Each entry also has a table of column descriptions and data formats. This supplements the card image. Additional text accompanies any record formats that need extended discussion.

Some data are Fortran real numbers. For these fields, the implicit decimal point is shown as a black dot on the card image, just above the information entry row. Note that you can enter data with an explicit decimal point in any position within the field. A black dot that appears at the extreme right side of a field represents a decimal point at the right end (least significant digit) of a number. All other black dots appear above a line, indicating that the decimal point will fall between the digits on either side.

All IPF data record types are identified by the characters in columns 1 and 2. To fully specify a particular record, additional fields need to be filled in. These additional ID fields are usually the bus name and base kV. ID fields are called out in the column description tables.