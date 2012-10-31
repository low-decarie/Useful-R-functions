theme_minimal <- function (base_size = 12, base_family = "") 
{
  require(grid)
theme_grey(base_size = base_size, base_family = base_family) %+replace%
theme(
  axis.line = element_blank(),
  axis.text.x = element_text(family = base_family, size = base_size * 0.7, lineheight = 0.9, vjust = 0), 
  axis.text.y = element_text(family = base_family, size = base_size * 0.7, lineheight = 0.9, hjust = 1), 
  axis.ticks = element_line(colour = "black", size = 0.2),
  axis.title.x = element_text(family = base_family, size = base_size, vjust = 0), 
  axis.title.y = element_text(family = base_family, size = base_size, angle = 90, vjust = 0.5),
  axis.ticks.length = unit(0.3, "lines"), axis.ticks.margin = unit(0.5, "lines"),
  legend.background = element_rect(colour = NA), 
  legend.margin = unit(0.2, "cm"), legend.key = element_rect(colour = NA), 
  legend.key.size = unit(1.2, "lines"), legend.key.height = NULL,
  legend.key.width = NULL, legend.text = element_text(family = base_family, size = base_size * 0.8), 
  legend.text.align = NULL,
  legend.title = element_text(family = base_family, size = base_size * 0.8, face = "bold", hjust = 0), 
  legend.title.align = NULL,
  legend.position = "none", 
  legend.direction = "vertical", 
  legend.justification = "center",
  legend.box = NULL, 
  panel.background = element_rect(fill = "white", colour = NA), 
  panel.border = element_rect(fill = NA, colour = "grey90"), 
  panel.grid.major = element_line(colour = "grey90", size = 0.2), 
  panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
  panel.margin = unit(0.25, "lines"),
  strip.background = element_rect(fill = NA, colour = NA), 
  strip.text.x = element_text(family = base_family, size = base_size * 0.7),
  strip.text.y = element_text(family = base_family, size = base_size * 0.7, angle = -90),
  plot.background = element_rect(colour = NA),
  plot.title = element_text(family = base_family, size = base_size * 1.2),
  plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")
) 
}