## Resubmission

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
