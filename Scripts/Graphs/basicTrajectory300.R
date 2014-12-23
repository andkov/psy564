# p <- ggplot2::ggplot(dsM,aes(x=time,y=attend, group=id))
# p <- p + geom_line() 
# p <- p + geom_point(pch=20, size=2.5)
# p <- p + plotTheme
# p <- p + scale_x_continuous(limits=c(0,11),
#                             breaks=c(0:11))
# p <- p + scale_y_continuous(limits=c(1,8), 
#                             breaks=seq(1,8, by=1))
# p <- p + labs(list(
#   title="How often did you attend worship last year?",
#   x="Time scale: years since 2000", y="Church attendance"))


# dsM <- dplyr::filter(dsL, id <=300, 
#                      raceF != "Mixed (Non-H)") %>% 
#   dplyr::select(id,sexF,raceF,year,attend,attendF) %>%
#   dplyr::mutate(yearc = year - 2000)
#  
p <- ggplot2::ggplot(dsM,aes(x=time,y=attend))
p <- p + geom_line(aes(group=id), color='firebrick',
                   alpha=.2,
                   position=position_jitter(w=0.3, h=0.3))
p <- p + geom_point(shape=1, color="black", fill=NA,                 
                    alpha=.4, size=2, 
                    position=position_jitter(w=0.3, h=0.3))
p <- p + plotTheme
p <- p + scale_x_continuous(limits=c(0,11),
                            breaks=c(0:11))
p <- p + scale_y_continuous(limits=c(0,8), 
                            breaks=seq(1,8, by=1))
p <- p + labs(list(
  title="How often did you attend worship last year?",
  x="Time scale: years since 2000", y="Church attendance"))
p
