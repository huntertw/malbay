create_data <- function(data, groups, facet_by_scale = TRUE, by_subtask = FALSE){
 # Task or Subtask
  if(by_subtask){
    # Rethink this. Groups should always include Task or not?
    # Can I just show subtask in some other way?
    more_groups <- "Subtask"
    data <- data %>% 
      filter(!is.na(.data[[more_groups]]))
  } else {
    more_groups <- "Task"
  }
  
  # Scalability
  if(facet_by_scale == TRUE){
    more_groups <- c(more_groups, "Scalability")
  } 
  
  # Set up vectors
  ordering_by <- rlang::data_syms(groups)
  groups <- c(more_groups, groups)
  
  dataset <- data %>% 
    group_by(pick(all_of(groups))) %>%
    summarize(Hours = sum(.data$Total.Hours), .groups = "drop") %>% 
    # reTask is fine because it isn't used for a name anywhere
    mutate(reTask = reorder_within(.data[[groups[1]]], .data$Hours, 
                                   list(!!!ordering_by)))
  
  dataset
}

create_plot <- function(data, groups, period = "week"){
  x_var <- 'Hours'
  y_var <- 'reTask'  
  xlab <- period
  ylab <- "Task"
  row_vars <- rlang::data_syms(groups)
  
  cols <- NULL
  names_in_data <- names(data)
  
  if("Scalability" %in% names_in_data) {cols <- vars(.data$Scalability)} # cols
  if("Subtask" %in% names_in_data) {ylab <- "Subtask" } # Reset ylab
  if("Position.Specialty" %in% names_in_data) {fill_col <- "Position.Specialty"}
  if("Operational.Function" %in% names_in_data) {fill_col <- "Operational.Function"}
  if(!'fill_col' %in% ls()) {stop('huh?')}
  
  plot <- data %>% 
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_col(aes(fill = .data[[fill_col]]))  + # If this changes, I need to re-define how colors are created...
    geom_text(aes(label = .data[[x_var]]), hjust = 0, size = 2.5) +
    facet_nested(rows = vars(!!!row_vars),
                 cols = cols,
                 space = "free_y", scale = "free",
                 labeller = label_wrap_gen(width = 20, multi_line = TRUE)) +
    labs(title = NULL,
         y = ylab, 
         x = paste0("Hours per ", xlab)) +
    theme(legend.position = "none",
          strip.text.y.right = element_text(angle=0)) +
    scale_y_reordered() +
    scale_fill_manual(values = my_colors) # TODO: Create a function to generate colors
  
  plot
}

create_data_and_plot <- function(data, groups = vars, facet_by_scale = TRUE, 
                       by_subtask = FALSE, period = "week"){
  data_for_plt <- create_data(data, groups = groups, 
                               facet_by_scale = facet_by_scale,
                               by_subtask = by_subtask)
  a_plot <- create_plot(data_for_plt, groups = groups, period = period)
  
  a_plot
}

# a <- create_data_and_plot(main_dat, groups = c("Position.Specialty"))
# b <- create_data_and_plot(main_dat, groups = c("Position.Specialty", "Current.Position"))
# c <- create_data_and_plot(main_dat, groups = c("Operational.Function"))
# d <- create_data_and_plot(main_dat, groups = c("Operational.Function", "Current.Position"))
