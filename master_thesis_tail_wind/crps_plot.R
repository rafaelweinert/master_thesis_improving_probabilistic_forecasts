#%% load libraries ----

# remove all objects from the workspace
rm(list = ls())

# read the score_data RData file 
library(dplyr)
library(crch)
library(TailCalibration)
library(rlang)
library(ggplot2)
library(scoringRules)
library(plotly)
library(cowplot)



#%% individual plots for each lead time

crps_scores <- read.csv("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/scores/crps_scores_new.csv")
crps_scores <- crps_scores %>% filter(method %in% toupper(c("ens", "emos_sea", "mbm_sea", "idr", "emos_bst", "qrf_loc", "bqn", "drn", "hen")))

# for each lead time, group by method and plot twcrps on the y axis and t on the x axis
for (l in unique(crps_scores$lead_time)) {
  crps_scores_lead_l <- crps_scores %>% filter(lead_time == l)
  print(
    crps_scores_lead_l %>%
      ggplot(aes(x = t, y = owcrps, color = method)) +
      geom_point() +
      geom_line() +
      labs(title = paste0("CRPS for lead time ", l),
           x = "Threshold",
           y = "CRPS") +
      theme_minimal() +
      theme(legend.position = "bottom")
  )
}

crps_scores_lead_1 <- crps_scores %>% filter(lead_time == 1)
crps_scores_lead_1 %>%
  ggplot(aes(x = t, y = twcrps, color = method)) +
  geom_point() +
  geom_line() +
  labs(title = paste0("CRPS for leat time ", l),
       x = "Threshold",
       y = "CRPS") +
  theme_minimal() +
  theme(legend.position = "bottom")




#%% overall crps (threshold = 0)

crps_scores_0 <- crps_scores %>% filter(t == 0)
p <- crps_scores_0 %>%
  ggplot(aes(x = lead_time, y = twcrps, color = method)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  labs(title = "CRPS for all lead times",
       x = "Lead Time (h)",
       y = "CRPS") +
  theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.title.x = element_text(margin = margin(t = 10)),
      legend.text = element_text(size = 12),
      panel.background = element_blank(),
      plot.background = element_blank(),
    )
  # theme(#legend.position = "bottom",
  #       plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  #       axis.title = element_text(size = 14),
  #       axis.text = element_text(size = 12),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       plot.background = element_blank())

# Save the plot
cowplot::save_plot(
  filename = "plots/crps_plots/overall_crps.pdf",
  plot = p,
  base_width = 12,
  base_height = 6,
  dpi = 300
)







#%% individual plots for each threshold
# for each threshold, group by method and plot twcrps on the y axis and lead time on the x axis

# Create a directory for the combined plots if it doesn't exist
# dir.create("plots/crps_plots/combined", showWarnings = FALSE, recursive = TRUE)

for (t_value in unique(crps_scores$t)) {
  crps_scores_t <- crps_scores %>% filter(t == t_value)
  
  # Create both plots
  p1 <- crps_scores_t %>%
    ggplot(aes(x = lead_time, y = twcrps, color = method)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1) +
    labs(title = "twCRPS",
         x = "Lead Time (h)",
         y = "twCRPS") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())
  
  p2 <- crps_scores_t %>%
    ggplot(aes(x = lead_time, y = owcrps, color = method)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1) +
    labs(title = "owCRPS",
         x = "Lead Time (h)",
         y = "owCRPS") +
    theme_minimal() +
    theme(#legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())
  
  # Extract the legend from one of the plots
  legend_plot <- crps_scores_t %>%
    ggplot(aes(x = lead_time, y = twcrps, color = method)) +
    geom_point(size = 2) +
    geom_line(linewidth = 1) +
    labs(color = "Method") +
    theme_minimal() +
    theme(legend.position = "bottom")

  
  legend <- cowplot::get_legend(legend_plot)
  
  # Combine plots with cowplot
  combined_plot <- cowplot::plot_grid(
    p1, p2, 
    nrow = 1
  )
  
  # Add the legend to the combined plot
  final_plot <- cowplot::plot_grid(
    combined_plot, 
    legend, 
    ncol = 1, 
    rel_heights = c(1, 0.2)
  )
  
  # Add a title for the entire figure
  title <- cowplot::ggdraw() + 
    cowplot::draw_label(
      paste0("twCRPS and owCRPS for threshold = ", t_value, " m/s"),
      fontface = 'bold',
      size = 16,
      x = 0.5
    )
  
  final_plot_with_title <- cowplot::plot_grid(
    title, final_plot,
    ncol = 1,
    rel_heights = c(0.1, 1)
  )
  
  # Print the plot
  print(final_plot_with_title)
  
  # Save the plot
  cowplot::save_plot(
    filename = paste0("plots/crps_plots/wcrps_t_", t_value, ".pdf"),
    plot = final_plot_with_title,
    base_width = 12,
    base_height = 6,
    dpi = 300
  )
  
  # Also save as PNG if needed
  cowplot::save_plot(
    filename = paste0("plots/crps_plots/wcrps_t_", t_value, ".png"),
    plot = final_plot_with_title,
    base_width = 12,
    base_height = 6,
    dpi = 300
  )
}


#%%

calibration_abs_dist <- read.csv("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/scores/calibration_abs_dist.csv")

for (l in unique(calibration_abs_dist$lead_time)){
  calibration_abs_dist_lead_l <- calibration_abs_dist %>% filter(lead_time == l)
  print(
    calibration_abs_dist_lead_l %>%
      ggplot(aes(x = t, y = com, color = method)) +
      geom_point() +
      geom_line() +
      labs(title = paste0("Calibration Absolute Distance for lead time ", l),
           x = "Threshold",
           y = "Absolute Distance") +
      theme_minimal() +
      theme(legend.position = "bottom")
  )

}



#%% plot theshold-crps with plotly

# crps_scores <- read.csv("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/scores/crps_scores_new.csv")


# # Unique lead times and methods
# lead_times <- unique(crps_scores$lead_time)
# methods <- unique(crps_scores$method)
# thresholds <- unique(crps_scores$t)

# # Assign consistent colors to methods
# method_colors <- setNames(colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(methods)), methods)


# # Create an initial plot
# p <- plot_ly()

# # Add traces for each method within each lead time and metric
# y_vars <- c("owcrps", "twcrps")
# trace_index <- 1
# trace_mapping <- list()

# for (y_var in y_vars) {
#   for (l in lead_times) {
#     crps_scores_lead_l <- crps_scores %>% filter(lead_time == l)
#     calibration_abs_dist_lead_l <- calibration_abs_dist %>% filter(lead_time == l)

#     for (m in methods) {
#       crps_scores_method_m <- crps_scores_lead_l %>% filter(method == m)
#       calibration_abs_dist_method_m <- calibration_abs_dist_lead_l %>% filter(method == m)
      
#       trace_name <- paste(m, "(Lead Time", l, ")")
#       trace_mapping[[trace_name]] <- trace_index

#       # Add CRPS trace (left y-axis)
#       p <- p %>% add_trace(
#         data = crps_scores_method_m,
#         x = ~t,
#         y = as.formula(paste("~", y_var)),
#         type = "scatter",
#         mode = "lines+markers",
#         name = m,
#         legendgroup = m,  # Link both traces for the method
#         visible = ifelse(y_var == "owcrps" & l == lead_times[1], TRUE, FALSE),
#         line = list(color = method_colors[m]),
#         marker = list(color = method_colors[m])
#       ) 

#       # Add COM trace (right y-axis)
#       p <- p %>% add_trace(
#         data = calibration_abs_dist_method_m,
#         x = ~t,
#         y = ~com,
#         type = "scatter",
#         mode = "lines+markers",
#         name = m,  # Use same legend name
#         legendgroup = m,  # Ensures both traces toggle together
#         showlegend = FALSE,  # Hide duplicate legend entry
#         yaxis = "y2",
#         visible = ifelse(y_var == "owcrps" & l == lead_times[1], TRUE, FALSE),
#         line = list(color = method_colors[m], dash = "dash"),
#         marker = list(color = method_colors[m])
#       )

#       trace_index <- trace_index + 1
#     }
#   }
# }

# # Create steps for slider
# steps <- lapply(1:length(lead_times), function(i) {
#   visibility_vector <- rep(FALSE, 2 * length(y_vars) * length(lead_times) * length(methods))
#   start_index <- (i - 1) * 2 * length(methods) + 1
#   end_index <- start_index + 2 * length(methods) - 1
#   visibility_vector[start_index:end_index] <- TRUE
  
#   list(
#     method = "update",
#     args = list(
#       list(visible = visibility_vector),
#       list(title = paste("CRPS for Lead Time", lead_times[i]))
#     ),
#     label = paste(lead_times[i])
#   )
# })

# # Create buttons for metric selection
# buttons <- lapply(1:length(y_vars), function(i) {
#   visibility_vector <- rep(FALSE, 2 * length(y_vars) * length(lead_times) * length(methods))
#   for (j in 1:length(lead_times)) {
#     start_index <- ((i - 1) * length(lead_times) + (j - 1)) * 2 * length(methods) + 1
#     end_index <- start_index + 2 * length(methods) - 1
#     visibility_vector[start_index:end_index] <- TRUE
#   }
  
#   list(
#     method = "update",
#     args = list(
#       list(visible = visibility_vector),
#       list(title = paste("CRPS -", y_vars[i]))
#     ),
#     label = y_vars[i]
#   )
# })

# # Apply layout and add slider & dropdown menu
# p <- p %>% layout(
#   title = paste("CRPS for Lead Time", lead_times[1]),
#   xaxis = list(title = "Threshold"),
#   yaxis = list(title = "CRPS", range = c(0, 1.2), side = "left"),
#   yaxis2 = list(
#     title = "Combined Ratio",
#     overlaying = "y",
#     side = "right",
#     showgrid = FALSE,
#     range = c(0, max(calibration_abs_dist$com, na.rm = TRUE))  # Ensure correct scaling
#   ),
#   sliders = list(
#     list(
#       active = 0,
#       currentvalue = list(prefix = "Lead Time: "),
#       steps = steps
#     )
#   ),
#   updatemenus = list(
#     list(
#       type = "dropdown",
#       buttons = buttons,
#       direction = "down",
#       x = 0.8,
#       y = 1.2
#     )
#   )
# )

# # Render plot
# p



# # store the plot
# htmlwidgets::saveWidget(p, "C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/plots/crps_plots_plotly/crps_threshold_plot.html", selfcontained = TRUE)



#%% plot lead time - crps 

crps_scores <- read.csv("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/scores/crps_scores_new.csv")
calibration_abs_dist <- read.csv("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/scores/calibration_abs_dist.csv")

# Unique lead times and methods
lead_times <- unique(crps_scores$lead_time)
methods <- unique(crps_scores$method)
thresholds <- unique(crps_scores$t)
method_colors <- setNames(colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(methods)), methods)


p <- plot_ly()

# Add traces for each method within each lead time and metric
y_vars <- c("owcrps", "twcrps")
trace_index <- 1
trace_mapping <- list()

for (y_var in y_vars) {
  for (t in thresholds) {
    crps_scores_t <- crps_scores %>% filter(t == !!t)
    calibration_abs_dist_t <- calibration_abs_dist %>% filter(t == !!t)
    for (m in methods) {
      crps_scores_method_m <- crps_scores_t %>% filter(method == m)
      calibration_abs_dist_method_m <- calibration_abs_dist_t %>% filter(method == m)
      trace_name <- paste(m, "(Threshold", t, ")")
      trace_mapping[[trace_name]] <- trace_index
      p <- p %>% add_trace(
        data = crps_scores_method_m,
        x = ~lead_time,
        y = as.formula(paste("~", y_var)),
        type = "scatter",
        mode = "lines+markers",
        name = m,
        legendgroup = m,
        visible = ifelse(y_var == "owcrps" & t == thresholds[1], TRUE, FALSE),
        line = list(color = method_colors[m]),
        marker = list(color = method_colors[m])
      ) %>% add_trace(
        data = calibration_abs_dist_method_m,
        x = ~lead_time,
        y = ~com,
        type = "scatter",
        mode = "lines+markers",
        name = m,  # Same legend entry
        legendgroup = m,  # Ensures both traces toggle together
        showlegend = FALSE,  # Hide duplicate legend entry
        yaxis = "y2",
        visible = ifelse(y_var == "owcrps" & t == thresholds[1], TRUE, FALSE),
        line = list(color = method_colors[m], dash = "dash"),  # Differentiate visually
        marker = list(color = method_colors[m])
      )
        
      trace_index <- trace_index + 1
    }
  }
}

# Create steps for slider
steps <- lapply(1:length(thresholds), function(i) {
  visibility_vector <- rep(FALSE, 2 * length(y_vars) * length(thresholds) * length(methods))
  start_index <- (i - 1) * 2 * length(methods) + 1
  end_index <- start_index + 2 * length(methods) - 1
  visibility_vector[start_index:end_index] <- TRUE
  
  list(
    method = "update",
    args = list(
      list(visible = visibility_vector),
      list(title = paste("CRPS for Threshold", thresholds[i]))
    ),
    label = paste(thresholds[i])
  )
})

# Create buttons for metric selection
buttons <- lapply(1:length(y_vars), function(i) {
  visibility_vector <- rep(FALSE, length(y_vars) * length(thresholds) * length(methods))
  for (j in 1:length(thresholds)) {
    start_index <- ((i - 1) * length(thresholds) + (j - 1)) * length(methods) + 1
    end_index <- start_index + length(methods) - 1
    visibility_vector[start_index:end_index] <- TRUE
  }
  
  list(
    method = "update",
    args = list(
      list(visible = visibility_vector),
      list(title = paste("CRPS -", y_vars[i]))
    ),
    label = y_vars[i]
  )
})

# Apply layout and add slider & dropdown menu
p <- p %>% layout(
  title = paste("CRPS for Threshold", thresholds[1]),
  xaxis = list(title = "Lead Time"),
  yaxis = list(title = "CRPS", range = c(0, 1.6), side = "left"),
  yaxis2 = list(
    title = "Combined Ratio",
    overlaying = "y",
    side = "right",
    showgrid = FALSE,
    range = c(0, max(calibration_abs_dist$com, na.rm = TRUE))  # Ensure correct scaling
  ),
  sliders = list(
    list(
      active = 0,
      currentvalue = list(prefix = "Threshold: "),
      steps = steps
    )
  ),
  updatemenus = list(
    list(
      type = "dropdown",
      buttons = buttons,
      direction = "down",
      x = 0.8,
      y = 1.2
    )
  )
)

# Render plot
p


# store the plot
htmlwidgets::saveWidget(p, "C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/plots/crps_plots_plotly/crps_leadtime_plot.html", selfcontained = TRUE)


# ->> if you look at a very low threshold, the crps is more or less equal for all lead times per respective method, whereas for a medium high threshold (e.g. 5), the crps is highest for lead times around 13-15. 
# ->> for a high threshold, (e.g. 11), the crps is more or less equal for all lead times per respective method.
