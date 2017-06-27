
my_gg_theme <- function(theme.size = 14) {
  theme(strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour='#D0D0D0',size=.2),
        text = element_text(size = theme.size, colour="black"),
        axis.line = element_line(colour='#A0A0A0',size=.5),
        axis.text = element_text(size = theme.size, colour="black")
        )
}

