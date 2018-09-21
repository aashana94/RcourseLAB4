ggplot2::ggplot(faithful, aes(x=eruptions, y=waiting))+
  geom_point()+
  theme_liu1()


theme_liu1 <- function(){

  theme_bw()+
  theme(panel.background = element_rect(fill = "#FFFFFF", colour = "#666666"),
        panel.grid.major = element_line(colour = "#54d8e0", size=0.3),
        panel.grid.minor = element_line(colour = "#54d8e0", size=0.3),
        axis.line = element_line(size =0.3, colour = "#54d8e0"),
        axis.text = element_text(size = rel(0.8)),
        axis.ticks = element_line(colour = "#54d8e0"),
        legend.key = element_rect(colour = "#54d8e0"),
        panel.border = element_rect(fill = NA, colour = "#54d8e0"),
        strip.background = element_rect(fill = "grey80", colour = "#54d8e0"))
}


ggplot2::ggplot(iris, aes(x=Sepal.Length, y=Petal.Length))+
  geom_point()+
  theme_liu2()

theme_liu2 <- function(){
  theme_bw()+
    theme(panel.background = element_rect(fill = "#54d8e0", colour = "#666666"),
          panel.grid.major = element_line(colour = "#FFFFFF", size=0.3),
          panel.grid.minor = element_line(colour = "#FFFFFF", size=0.3),
          axis.line = element_line(size =0.3, colour = "#54d8e0"),
          axis.text = element_text(size = rel(0.8)),
          axis.ticks = element_line(colour = "#54d8e0"),
          legend.key = element_rect(colour = "#54d8e0"),
          panel.border = element_rect(fill = NA, colour = "#54d8e0"),
          strip.background = element_rect(fill = "grey80", colour = "#54d8e0"))+
    annotation_custom(g_pic, xmin=5, xmax=7, ymin=30, ymax=45)
}
