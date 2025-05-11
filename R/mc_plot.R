#' @import ggplot2
#' @import grDevices


# Discrete palettes
all_colours <- c("#738f2D", "#576cc6", "#d14c4c", "#e38f10", "#b3c87f", "#a3aedd", "#dd9d9d", "#eadeab", "#898989", "#ced1c7")

main_colours <- c("#738f2D", "#576cc6", "#d14c4c", "#e38f10")

light_colours <- c("#b3c87f", "#a3aedd", "#dd9d9d", "#eadeab")

# Green to Blue gradient
green_blue_gradient <- grDevices::colorRampPalette(c("#738f2D", "#576cc6"))

# Orange to Red gradient
vainilla_red_gradient <- grDevices::colorRampPalette(c("#eadeab", "#d14c4c"))

# Usage example:
# green_blue_gradient(5) # Creates a gradient with 5 colours
# vainilla_red_gradient(5) # Creates a gradient with 5 colours
