## Resubmission

This is a resubmission. In this version I have:

* removed version number from methods package in DESCRIPTION

* separated namespace tests in all tests in the testthat folder.

* 'create_range_plot' function received an "orientation" parameter to better control 
either a horizontal or vertical range for points. Axis scaling is modified in
accordance with the selection.

* 1 Note from devtools::check_win_devl() -- corrected in DESCRIPTION file
