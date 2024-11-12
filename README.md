## RplotterPkg

This R computer language package contains a number of plotting routines that functionalize many of the geom's contained in the ggplot2 package.  Demonstrations are provided in showing how the functions can assist in statistical plotting for everyday use. 

To install *RplotterPkg* enter the following command lines from an R console:

```R
install.packages("devtools")
```

```R
devtools::install_github("deandevl/RplotterPkg")
```

In your R application file include the package:

```R
library(RplotterPkg)
```

The following table list the functions from the package:

| Function                  | Description                                                  |
| ------------------------- | :----------------------------------------------------------- |
| create_bar_plot           | Function creates a ggplot2 based bar plot with optional scaling and ordering. |
| create_box_plot           | Function creates a ggplot2 based box plot with optional scaling, ordering and outlier viewing. |
| create_density_plot       | Function creates a ggplot2 based density plot with options for scaling, shading probability areas, and plotting observation locations. |
| create_density_ridge_plot | Function creates ggplot2 based density plots stacked vertically (also known as ridge or raincloud plots). |
| create_heatmap            | Function wraps ggplot2 geom_tile to produce a heatmap that shows magnitude as an array of cells in two dimensions. |
| create_histogram_plot     | Function plots a ggplot2 based histogram with options for scaling and viewing observation locations. The function offers one of four ways of setting the number of bins. |
| create_range_plot         | Function returns a ggplot2 plot object displaying the individual spread or vertical interval/range for a collection of x/y pairs of points. |
| create_scatter_plot       | Function returns a ggplot2 plot object of x/y scatter points/lines. Options are provided for axis scaling and variable/non-variable dependent aesthetics. |
| create_stick_plot         | Function returns a plot object showing vertical/horizontal lines that run from a base value to a measurement value. Options are provided for scaling. |
| create_table              | Function incorporates the [gt](https://cran.r-project.org/web/packages/gt/index.html) package to create tables primarily for R Markdown/Quarto documents.  The function offers a quick alternative if just limited styling and formatting are required. |
| create_table_graphic      | Function creates a simple, non-scrollable static table to be laid out with other ggplot2 graphics.  The function is based on `grid` along with the `gtable` package. |
| get_grob_component        | Function retrieves a grob component from a ggplot2 plot object. |
| multi_panel_grid          | Function creates a figure with title and arranges multiple plots/grobs across a given number of rows and columns. The function depends on the `grid`, `gtable`, and `ggplotify` packages. |
| percentile_table          | Function displays a table with lower/upper locations, values, average and spread between lower/upper from median to one hundred and twenty-eighth percentiles. The function returns a list with a viewable gt table along with the data.table containing the table's values. |
| spread_level_plot         | The spread is defined as the difference between the 75th and 25th quartiles. Function returns a named list with a data.table of each level's median, quartile values, and a ggplot2 scatter plot with medians along the x axis and spreads along the y axis. |
| stem_leaf_display         | Function accepts a named list of numeric vectors from which stem and leaf displays are provided. |
| symmetry_plot             | Function orders the numeric vector from low to high. It then divides the data into two groups: values above the median(u group) and values below the median(v group). The distances from the median for values in each group are computed. A scatter plot is created where distances in the v group are plotted along the x axis and distances from the v group are plotted along the y axis. If the upper and lower distance data forms a straight line then the original data is perfectly symmetrical. |

