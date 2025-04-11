library(ggplot2)
library(hexSticker)
library(data.table)

mccratio <- fread("./pkgdown/mccratio.csv")


light <- "#d9d9d9"
dark <- "#909090"

mccratioplot <- ggplot(mccratio, aes(x = time, y = ratio)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl),
              alpha = .25, color = "transparent", fill = "#c93312") +
  geom_line(linewidth = 1, color = "#c93312") +
  geom_rect(xmin = 1.1, xmax = 2.3, ymin = log10(1.8), ymax = log10(50), fill = dark) +
  scale_y_log10(breaks = c(.25,.5,1,2)) +
  coord_cartesian(ylim = c(.3,2), xlim = c(0,5)) +
  labs(y = "", x = "") +
  theme_void() +
  theme(axis.line = element_line(color = light),
        axis.ticks = element_line(color = light),
        axis.ticks.length = unit(2,"pt"),
        panel.grid.major = element_line(color = light, linewidth = .05))

# customise your hex sticker
hexSticker::sticker(
  filename = "inst/figures/logo.png",
  white_around_sticker = TRUE,
  # subplot aesthetics
  subplot = mccratioplot,
  s_width = 2, s_height = 1,
  s_x=1.2, s_y=.9,
  # package name aesthetics
  package = "MCC",
  p_size = 24,
  p_color = light,
  # hexagon aesthetics
  h_size = 1,
  h_fill = dark,
  h_color = light#,
  # url aesthetics
  # url = "psyteachr.github.io/demopkg",
  # u_size = 5.4,
  # u_color = "white"
) |> plot() # preview with plot()
