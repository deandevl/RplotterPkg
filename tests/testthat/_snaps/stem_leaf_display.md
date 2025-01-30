# stem_leaf_display()

    Code
      marathon_times_lst <- list(age_20 = RplotterPkg::boston_marathon[age == 20, ]$
        time, age_30 = RplotterPkg::boston_marathon[age == 30, ]$time, age_40 = RplotterPkg::boston_marathon[
        age == 40, ]$time, age_50 = RplotterPkg::boston_marathon[age == 50, ]$time,
      age_60 = RplotterPkg::boston_marathon[age == 60, ]$time)
      a_plot <- RplotterPkg::stem_leaf_display(x = marathon_times_lst, title = "Women times(min) in Boston marathon",
        heading_color = "#FF5500")

