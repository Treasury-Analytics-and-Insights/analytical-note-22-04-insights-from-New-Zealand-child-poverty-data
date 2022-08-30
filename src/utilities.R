###########################
# Functions
###########################
summarise_populations <- function(this_data, exclude_group){
  
  this_data[, Population := as.numeric(Population)]
  this_data[is.na(Population), Population := 0]
  
  # totals 
  totals_data <- this_data[Descriptor_1 == "All" & Group != "All"]
  totals_data[, Population_total := sum(Population)]
  totals_data[, PopulationOfInterest_total := sum(Population*(!(Group %in% c("All", exclude_group))))]
  totals_data[, Group_total := Population, by = Group]
  totals_output <- totals_data
  totals_data <- totals_data[Group != "All"]

  totals_data[, .(Group, Population_total, PopulationOfInterest_total, Group_total)]
  
  this_data <- merge(this_data[Descriptor_1 != "All"], 
                     totals_data[, .(Group, Population_total, PopulationOfInterest_total, Group_total)], 
                     by = "Group")
  
  this_data[, Population_LessGroup := Population_total - Group_total]
  this_data[, PopulationOfInterest_LessGroup := PopulationOfInterest_total - Group_total]
  
  this_data[, Characteristic_total := sum(Population, na.rm=T), by = .(Descriptor_1, Descriptor_2)]
  this_data[, Characteristic_PopulationOfInterest_total := sum(Population*(Group!=exclude_group), na.rm=T), 
          by = .(Descriptor_1, Descriptor_2)]
  this_data[, Characteristic_total_LessGroup := Characteristic_total - Population]
  this_data[, Characteristic_totalOfInterest_LessGroup := Characteristic_PopulationOfInterest_total - Population]
  
  this_data[, Item_Prob := Population/Group_total]
  this_data[, ItemAllGroups_Prob := Characteristic_total/Population_total]
  this_data[, ItemProb_LessThisGroup := Characteristic_total_LessGroup/Population_LessGroup]
  this_data[, ItemPovGroups_Prob := Characteristic_totalOfInterest_LessGroup/PopulationOfInterest_LessGroup]
  
  this_data[, Relative_Risk_all := Item_Prob/ItemProb_LessThisGroup]
  this_data[, Relative_Risk_PopOfInterest := Item_Prob/ItemPovGroups_Prob]
  
  characteristics <- this_data[, .(Group,
                                 Descriptor_1,Descriptor_2,
                                 Population = Population,
                                 Item_Prob = Item_Prob, 
                                 Relative_Risk_all = round(Relative_Risk_all,2), 
                                 Relative_Risk_PopOfInterest = round(Relative_Risk_PopOfInterest,2))]
  
  characteristics[Item_Prob == 0, Item_Prob := NA]
  characteristics[Relative_Risk_all == 0, Relative_Risk_all := NA]
  characteristics[Relative_Risk_PopOfInterest == 0, Relative_Risk_PopOfInterest := NA]
  
  return(list(characteristics, totals_output))
  
}

characteristics <- function(filename, grouping, name_map, exclude_group){
  results <-  fread(filename)
  results <- results[Table == grouping]
  results[, Group := factor(name_map[as.character(Group)], ordered=TRUE)]
  results <- results[!is.na(Group)]

  all_characteristics <- summarise_populations(results, exclude_group)
  characteristics <- all_characteristics[[1]] 
  totals_output <- all_characteristics[[2]]

  output_characteristics <- melt(characteristics, id.vars = c("Group", "Descriptor_1", "Descriptor_2"))
  output_characteristics <- dcast(output_characteristics, Descriptor_1 + Descriptor_2 ~ variable + Group)
  
  return(list(characteristics, output_characteristics, totals_output))
  
}

make_table <- function(characteristic_data, 
                       characteristic_names, 
                       type, 
                       exclude_group  = NA){
  
  this_table <- characteristic_data[Descriptor_1 %in% c("All", names(characteristic_names))]
  this_table[, Name := factor(characteristic_names[Descriptor_1], order=TRUE)]
  
  this_table <- merge(this_table, ordering, by = "Descriptor_2")
  this_table[, Descriptor_1 := NULL]
  this_table[, Descriptor_2 := NULL]

  keep_cols <- c("Name", "Value", "Order", names(this_table)[names(this_table) %like% type])

  this_table <- this_table[, .SD, .SDcols = keep_cols]

  setnames(this_table, gsub(paste0(type, "_"), "", names(this_table))) 
  this_table <- this_table[, .SD, 
                           .SDcols = 
                             c("Name", "Value", "Order",
                               name_map)]
  if(type == "Relative_Risk_PopOfInterest"){
    this_table <- this_table[, c(exclude_group) := NULL]
  }

  this_table <- this_table[order(Name, Order)] 
  this_table[, Order := NULL]

  format_cols <- names(this_table)[3:ncol(this_table)]

  options(knitr.kable.NA = '')
  
  out_table <-
    this_table %>%
    gt(groupname_col = "Name") %>%
    fmt_missing(columns = everything(),
                missing_text = "-") %>%
    #Apply new style to all column headers
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(sides = "top", weight = px(3)),
        cell_borders(sides = "bottom", weight = px(3)),
        cell_text(weight = "bold")
      ),
      locations = cells_row_groups(groups = everything())) %>%
    tab_options( table.font.size = "small")
  
  if (type == "Item_Prob"){
    out_table <-
      out_table  %>%
      tab_spanner(label = "Item probability",
                  columns = all_of(format_cols)
      ) %>%
      data_color(columns = all_of(format_cols),
                 colors = palette_percent, 
                 alpha=0.75)  %>%
      fmt_percent(columns = all_of(format_cols), decimals = 0) %>%
      cols_width( !matches("Value") ~ px(80))
  }
  
  if (type == "Population"){
    out_table <-
      out_table  %>%
      tab_spanner(label = "Number of children",
                  columns = all_of(format_cols)
      )  %>%
      fmt_integer(columns = all_of(format_cols)) %>%
      cols_width( !matches("Value") ~ px(80))
  }

  if (type %like% "Relative"){

    test_fun <- function(x){cells_body(columns = !!sym(x), rows = !!sym(x) > 3)}

    out_table <-
      out_table    %>%
      tab_spanner(label = "Relative risk",
                  columns = all_of(format_cols)) %>%
      data_color(columns = all_of(format_cols),
                 colors = palette_risk, alpha=0.75)  %>%
      tab_style(style = list(cell_fill(color = palette_risk(3), 
                                       alpha=0.75),
                             cell_text(color = "black")),
                locations = lapply(format_cols,
                                   test_fun)) %>%
      cols_width( !matches("Value") ~ px(80))

  }
  
  return(out_table)
}
