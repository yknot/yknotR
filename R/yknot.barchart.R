#' A Barchart function with ggplot2
#'
#' Creates barchart and saves it as png
#' @param df dataframe
#'        x x data
#'        y ydata
#' @keywords plot
#' @export
#' @examples
#' yknot.barchart(iris, "Sepal.Length", "Sepal.Width", "Species", "Sepal Comparison", "Sepal Length", "Sepal Width", "Test")


yknot.barchart <- function(df, x, y, fill, title, xlab, ylab, filename) {
  ggplot(df, aes_string(x = x, y = y, fill = fill)) +
    # bar chart
    geom_bar(stat='identity') +
    # numbers on chart
    #geom_text(aes_string(label=y), position=position_dodge(width=0.9), size=10, hjust=1.5, vjust= .4) +
    # make it vertical
    coord_flip() +
    # black and white
    theme_bw() +
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill="#F0F0F0")) +
    theme(plot.background=element_rect(fill="#F0F0F0")) +
    theme(panel.border=element_rect(colour="#F0F0F0")) +
    # Format the grid
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    # Fix legend
    theme(legend.key.size = unit(2.5, "cm")) +
    theme(legend.text = element_text(size=20)) +
    theme(legend.title = element_text(size=20)) +
    theme(legend.background=element_rect(fill="#F0F0F0")) +
    # Set title and axis labels, and format these and tick marks
    ggtitle(title) +
    theme(plot.title=element_text(face="bold",colour="#3C3C3C",size=30)) +
    ylab(ylab) +
    xlab(xlab) +
    theme(axis.text.x=element_blank()) +
    theme(axis.text.y = element_text(size=20, hjust=.75)) +
    theme(axis.title.y=element_text(size=26,colour="#535353",face="bold",vjust=1.5)) +
    theme(axis.title.x=element_text(size=26,colour="#535353",face="bold",vjust=-.5)) +
    # Plot margins and finally lin  e annotations
    theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) +
    # color palette for fills
    scale_fill_brewer(palette="Paired")

  ggsave(filename=paste(filename, ".png", sep=""), height=60, width=40, units="cm", dpi=600)
}
