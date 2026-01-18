## Resubmission

Version 0.1.5:
This is a re-submission. In this version I have:

* Revised the description and parameter names for the function 'create_range_plot' for easier
implementation for either vertical or horizontal range segment line orientation.

* Added points, specify colors, labeling to the range segment lines of 'create_range_plot'.

Version 0.1.4:
This is a resubmission. In this version I have:

* In tests/testthat folder for test-create_table.R, changed 'rowname_col' from
'ID' to 'col_0'.

Version 0.1.3:
This is a resubmission. In this version I have:

* removed version number from methods package in DESCRIPTION

* separated namespace tests in all tests in the testthat folder.

* 'create_range_plot' function received an "orientation" parameter to better control 
either a horizontal or vertical range for points. Axis scaling is modified in
accordance with the selection.

* 1 Note from devtools::check_win_devl() -- corrected in DESCRIPTION file
