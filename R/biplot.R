#' Biplot from a principal components analysis (PCA)
#'
#' @params PC  a prcomp object obtained by conducting a principal components
#'     analysis.
#' @params d a data frame of metadata containing the sample identifiers and
#'     categorical classification of the samples.
#' @params colors a character vector of same length as the number of levels in
#'     the classfication.
#' @params legend_t the name of the column in d that gives the sample
#'     classification.
#' @params varnames a character vector of names for the variables in the biplot.
#' @params labels a logical indicating whether samples should be labelled.
#' @params title a title for the biplot.
#' @params x principal component to be plotted along the horizontal (x) axis or
#'     abcissa.
#' @params y principal component to be plotted along the vertical (y) axis or
#'     ordinate.
#' @return A [ggplot2 object]
#' @rdname PCbiplot
#' @export
PCbiplot <- function(PC
            , d
            , colors
            , legend_t
            , varnames
            , labels 
            , title
            , x="PC1"
            , y="PC2") {
    require(ggplot2)
    require(scales)
##### PC being a prcomp object
##### data <- data.frame(obsnames=row.names(PC$x), PC$x)
    data <- data.frame(obsnames = d$ID, PC$x, State = factor(d[,legend_t]))
    plot <- ggplot(data, aes_string(x=x, y=y))
    
#####plot <- plot + geom_text(alpha=.7, size=3, aes(label=obsnames, color = Disease.State))
    plot <- plot + geom_point(size=3, aes(color = State), alpha = 0.7)
    if(labels){
        plot <- plot + geom_text(aes(label=obsnames),
                                 show_guide  = F,
                                 hjust=1,
                                 vjust=1.25,
                                 size = 2)
    }
    plot <- plot + guides(color = guide_legend(paste(legend_t)))
    plot <- plot + scale_color_manual(values = colors)
    plot <- plot + geom_hline(aes(yintercept = 0), size=.2) + geom_vline(aes(xintercept = 0), size=.2)
    plot <- plot + scale_x_continuous(breaks=pretty_breaks(12))
    plot <- plot + scale_y_continuous(breaks=pretty_breaks(10))
                                        #datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
    datapc <- data.frame(varnames=varnames, PC$rotation)
    mult <- min(
    (max(data[,y]) - min(data[,y]))/(max(datapc[,y])-min(datapc[,y])),
    (max(data[,x]) - min(data[,x]))/(max(datapc[,x])-min(datapc[,x]))
    )
    datapc <- transform(datapc,
                        v1 = mult*(get(x)),
                        v2 = mult*(get(y))
                        )
#####print(head(datapc))
    plot <- plot + coord_equal()
    q1 <- subset(datapc, PC1 > 0 & PC2 > 0)
    q2 <- subset(datapc, PC1 < 0 & PC2 > 0)
    q3 <- subset(datapc, PC1 < 0 & PC2 < 0)
    q4 <- subset(datapc, PC1 > 0 & PC2 < 0)
    
    if(dim(q1)[1] > 0){
        plot <- plot + geom_text(data=q1, aes(x=v1, y=v2, label=varnames, fontface = "bold"), size = 3, vjust=-1, color="black")
    }
    if(dim(q2)[1] > 0){
        plot <- plot + geom_text(data=q2, aes(x=v1, y=v2, label=varnames, fontface = "bold"), size = 3, vjust=-1, color="black")
    }
    if(dim(q3)[1] > 0){
        plot <- plot + geom_text(data=q3, aes(x=v1, y=v2, label=varnames, fontface = "bold"), size = 3, vjust=-1, color="black")
    }
    if(dim(q4)[1] > 0){
        plot <- plot + geom_text(data=q4, aes(x=v1, y=v2, label=varnames, fontface = "bold"), size = 3, vjust=-1, color="black")
    }

    plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2),
                                size = 1,
                                arrow=arrow(length=unit(0.5,"cm")),
                                alpha=0.75,
                                color="black")
    
    plot <- plot + ggtitle(title)
    plot
}
