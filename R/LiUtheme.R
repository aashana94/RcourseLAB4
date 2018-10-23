#' Linkoping University theme
#' @name theme_liu1
#' @importFrom ggplot2 theme_bw theme element_line element_rect element_blank element_text
#'
#' @return Two ggplot themes
#' @export
theme_liu1 <- function(){
  ggplot2::theme_bw()+
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = "#666666"),
        panel.grid.major = ggplot2::element_line(colour = "#54d8e0", size=0.3),
        panel.grid.minor = ggplot2::element_line(colour = "#54d8e0", size=0.3),
        axis.line = ggplot2::element_line(size =0.3, colour = "#54d8e0"),
        axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
        axis.ticks = ggplot2::element_line(colour = "#54d8e0"),
        legend.key = ggplot2::element_rect(colour = "#54d8e0"),
        panel.border = ggplot2::element_rect(fill = NA, colour = "#54d8e0"),
        strip.background = ggplot2::element_rect(fill = "grey80", colour = "#54d8e0"))
}
ggplot2::ggplot(faithful, ggplot2::aes(x=eruptions, y=waiting))+
  ggplot2::geom_point()+
  theme_liu1()

#' Linkoping University theme with blue background
#' @name theme_liu2
#' @importFrom ggplot2 theme_bw theme element_line element_rect element_blank element_text
#'
#' @return Two ggplot themes
#' @export
theme_liu2 <- function(){
  ggplot2::theme_bw()+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#54d8e0", colour = "#666666"),
          panel.grid.major = ggplot2::element_line(colour = "#FFFFFF", size=0.3),
          panel.grid.minor = ggplot2::element_line(colour = "#FFFFFF", size=0.3),
          axis.line = ggplot2::element_line(size =0.3, colour = "#54d8e0"),
          axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
          axis.ticks = ggplot2::element_line(colour = "#54d8e0"),
          legend.key = ggplot2::element_rect(colour = "#54d8e0"),
          panel.border = ggplot2::element_rect(fill = NA, colour = "#54d8e0"),
          strip.background = ggplot2::element_rect(fill = "grey80", colour = "#54d8e0"))
     #annotation_custom(picc, xmin=5, xmax=7, ymin=30, ymax=45)
}
ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length))+
  ggplot2::geom_point()+
  theme_liu2()

# imgg <- png::readPNG("/Users/aashana/RcourseLAB4/images/liuLogo.png")
# picc <- as.raster(imgg, 10, 10, 10, 10, angle = 0, interpolate = TRUE)
