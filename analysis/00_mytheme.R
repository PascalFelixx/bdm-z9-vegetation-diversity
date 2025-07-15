# ------------------------------------------------------------------------------
# Script Name: 00_mytheme
# Purpose:     Define custom ggplot2 theme for BDM Z9 thesis visualizations.
#              Ensures consistent styling across all plots and figures.
# Author:      Pascal Felix
# Date:        2025-05-30
# ------------------------------------------------------------------------------


# ==============================================================================
# Custom ggplot2 Theme for BDM Thesis Plots
# ==============================================================================

library(ggplot2)

mytheme <- theme_classic() + 
  theme(
    axis.line = element_line(linewidth = 0.1, color = 'black'),
    axis.text = element_text(size = 11, color = 'black'),
    axis.title = element_text(size = rel(1.1), color = 'black'),
    axis.ticks = element_line(linewidth = 0.6, color = 'black'),
    axis.ticks.length = unit(0.2, "cm"),
    panel.border = element_rect(colour = 'black', fill = NA, linewidth = 1),
    axis.title.y = element_text(vjust = 4),
    axis.title.x = element_text(vjust = 2.5, margin = margin(t = 8)),
    plot.title = element_text(hjust = 0.5, size = 13),
    plot.margin = margin(t = 10, r = 14, b = 6, l = 14, unit = "pt"),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

