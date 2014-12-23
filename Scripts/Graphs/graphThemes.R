baseSize <- 12

theme1 <- ggplot2::theme_bw(base_size=baseSize) +
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize+1)) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size=baseSize-2)) +
  ggplot2::theme(axis.title=ggplot2::element_text(colour="gray40")) +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  ggplot2::theme(axis.ticks.length = grid::unit(0, "cm")) +
  ggplot2::theme(text = element_text(size =baseSize+7)) 


barFitTheme <- theme_bw() +
  theme(axis.text = element_text(colour="gray40", size=15)) +
  theme(axis.text.x = element_text(angle=90, vjust = .5)) +
  theme(axis.title = element_text(colour="gray40")) +
  theme(panel.border = element_rect(colour="gray80")) +
  theme(panel.grid.major.x = element_blank()) +
  # theme(axis.ticks = element_line(colour="gray80")) +
  theme(axis.ticks.length = grid::unit(0, "cm")) +
  theme(legend.position=c(.85,.8), legend.justification=c(0,0)) +
  # theme(legend.background = element_rect(fill = '#99999933')) +
  theme(legend.background = element_rect(fill = NA)) +
  theme(legend.text = element_text(colour = 'gray40'))


barFitTheme2 <- theme_bw() +
  theme(axis.text = element_text(colour="gray40", size=15)) +
  theme(axis.text.x = element_text(angle=0, vjust = .5)) +
  theme(axis.title = element_text(colour="gray40")) +
  theme(panel.border = element_rect(colour="gray80")) +
  theme(panel.grid.major.x = element_blank()) +
  # theme(axis.ticks = element_line(colour="gray80")) +
  theme(axis.ticks.length = grid::unit(0, "cm")) +
  theme(legend.position=c(.85,.8), legend.justification=c(0,0)) +
  # theme(legend.background = element_rect(fill = '#99999933')) +
  theme(legend.background = element_rect(fill = NA)) +
  theme(legend.text = element_text(colour = 'gray40'))
