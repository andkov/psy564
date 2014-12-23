p <- ggplot2::ggplot(dsM,aes(x=time,y=attend, group=id))
p <- p + geom_line() 
p <- p + geom_point(pch=20, size=2.5)
p <- p + plotTheme
p <- p + scale_x_continuous(limits=c(0,11),
                            breaks=c(0:11))
p <- p + scale_y_continuous(limits=c(0,8), 
                            breaks=seq(1,8, by=1))
p <- p + labs(list(
  title="How often did you attend worship last year?",
  x="Time scale: years since 2000", y="Church attendance"))
