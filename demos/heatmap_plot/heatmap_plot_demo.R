library(data.table)
library(rlang)
library(ggplot2)
library(here)
library(readr)
library(forcats)
library(lubridate)
library(dplyr)
library(RplotterPkg)
library(RpalettePkg)

file_path <- here::here("demos/data/RESSALES-mf.csv")
df <- readr::read_csv(file = file_path, skip = 740)

dates_df <- data.frame(per_idx = 1:687, date=seq.Date(from = as.Date("1963-01-01"), by = "1 month", length.out = 687))

data_df <- dplyr::left_join(df, dates_df, by = "per_idx")


# get housiing sales
sales_df <- data_df %>%
  filter(
    cat_idx == 1, # NSA home sales
    dt_idx == 1,  # all houses
    geo_idx == 1  # just USA
  ) %>%
  select(val, date)

sales_df <- sales_df %>%
  mutate(
    month_factor = forcats::fct_reorder(as.character(lubridate::month(date), format = "%b"), lubridate::month(date)),
    year_factor = forcats::fct_reorder(as.factor(lubridate::year(date)), -lubridate::year(date))
  ) %>%
  filter(
    lubridate::year(date) > 1999
  ) %>%
  rename(Sales = val)

# get housing supply
supply_df <- data_df %>%
  filter(
    is_adj == 1, # seasonally adjusted
    cat_idx == 3, # supply
    dt_idx == 1, # all houses
    geo_idx == 1 # just usa
  ) %>%
  select(val, date)

supply_df <- supply_df %>%
  mutate(
    month_factor = forcats::fct_reorder(as.character(lubridate::month(date), format = "%b"), lubridate::month(date)),
    year_factor = forcats::fct_reorder(as.factor(lubridate::year(date)), -lubridate::year(date))
  ) %>%
  filter(
    lubridate::year(date) > 1999
  ) %>%
  rename(Supply = val)

# create a palette of colors to use as a graduated set of fill colors for the heatmap tiles
# Load the RpalettePkg datasets pal_colors.rda and palettes.rda
colors_file_path <- here::here("demos/data/heatmap_colors.rda")
palette_file_path <- here::here("demos/data/heatmap_palettes.rda")

ret_message <- RpalettePkg::initialize_colors(colors_file_path)
ret_message <- RpalettePkg::initialize_palettes(palette_file_path)


# create the palette
ret_message <- RpalettePkg::crud_palette(
  color_path = colors_file_path,
  palette_path = palette_file_path,
  palette_name = "cool_to_hot",
  action = "create",
  palette_colors = c(
    "ch_lightblue_1" = "#a9a3f6",
    "ch_lightblue_2" = "#7a71f7",
    "ch_dark_blue" = "#1e0ef6",
    "ch_lightgreen" = "#9eeb8c",
    "ch_darkgreen" = "#2fb80c",
    "ch_yellow" = "#f1f41a",
    "ch_gold" = "#f3da20",
    "ch_orange" = "#efb80e",
    "ch_red" = "#e73706"
  )
)

# plot the sales using the above palette as a graduated color scale of the sales values "val"
RplotterPkg::create_heatmap(
  df = sales_df,
  aes_x = "month_factor",
  aes_y = "year_factor",
  aes_fill = "Sales",
  aes_label = "Sales",
  title = "U.S. New Home Sales by Month",
  subtitle = "1000's of homes not seasonally adjusted",
  center_titles = TRUE,
  x_title = "Month",
  y_title = "Year",
  rot_y_tic_label = TRUE
) + RpalettePkg::get_fill_scale(
  colors_path = colors_file_path,
  palette_path = palette_file_path,
  palette_name = "cool_to_hot",
  scale_title = "New Home Sales (1000s)",
  discrete = FALSE)

# plot the supply using the above palette
# Note: the fill scale colors are reversed
RplotterPkg::create_heatmap(
  df = supply_df,
  aes_x = "month_factor",
  aes_y = "year_factor",
  aes_fill = "Supply",
  aes_label = "Supply",
  title = "U.S. Months' Supply of New Homes",
  subtitle = "(seasonally adjusted)",
  center_titles = TRUE,
  x_title = "Month",
  y_title = "Year",
  rot_y_tic_label = TRUE
) + RpalettePkg::get_fill_scale(
  colors_path = colors_file_path,
  palette_path = palette_file_path,
  palette_name = "cool_to_hot",
  scale_title = "New Home Supply",
  discrete = FALSE,
  reverse_colors = TRUE)
