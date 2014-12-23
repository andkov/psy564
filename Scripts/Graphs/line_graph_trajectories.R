# rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

require(ggplot2)
require(dplyr)
require(reshape2)

BuildLine <- function( ) {

baseSize <- 12
### <b>
plotTheme <- ggplot2::theme_bw() +
  ### </b>
  ggplot2::theme_bw(base_size=baseSize)+
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize + 3)) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size= baseSize - 2))+
  ggplot2::theme(axis.title.x=ggplot2::element_text(colour="gray40", size = baseSize + 2, vjust=-.3))+
  ggplot2::theme(axis.title.y=ggplot2::element_text(colour="gray40", size = baseSize + 2, vjust=1.3))+
  ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80"))+
  ggplot2::theme(axis.ticks.length = grid::unit(0, "cm"))#+
    # ggplot2::theme(panel.background=element_rect(fill=bgColour,colour=NA)) +
    # ggplot2::theme(legend.position=c(.95,.90),legend.direction="vertical") +
    # ggplot2::theme(legend.background = element_rect(fill=NA)) +
    # ggplot2::theme(legend.text = element_text(size = 15),legend.title.align =(-3.3))# +
    # ggplot2::theme(panel.grid = element_line(linetype = 1,size=rel(3)))
dsM<- dsL %>% dplyr::filter(id %in% c(1,23))
  # ds<- dsp
  # head(ds)
p <- ggplot2::ggplot(dsM,aes(x=year,y=attend, group=id))
p <- p + geom_line() + geom_point() + plotTheme
p <- p + scale_x_continuous(limits=c(2000,2011),
                            breaks=c(2000:2011))
p <- p + scale_y_continuous(limits=c(1,8), 
                            breaks=seq(1,8, by=1))
p <- p + labs(list(
  title="How often did you attend worship last year?",
  x="Year of observation", y="Church attendance"))
p 
    return( p )

}

gLine <- BuildLine(
  p <- p + abline(a=0,b=1))
print(gLine)


# BuildLine("m7F")
# BuildLine("m7R1")
# BuildLine("m7R2")
