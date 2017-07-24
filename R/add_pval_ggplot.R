
get_in_parenthesis <- function(str){
  if (grepl(')',str)){
    str = regmatches(str, gregexpr("(?<=\\().*?(?=\\))", str, perl=T))[[1]]
  }
  str
}

## Convert a vector of p-values to characters, p<0.1 ., p<0.05 **, p<0.01 ***, the others are empty string
pvars2star <- function(pvars){
  pvars <- ifelse(pvars<0.001, '***',
                  ifelse(pvars<0.01,'**',
                         ifelse(pvars<0.05,'*',
                                ifelse(pvars<0.1,'',''))))
  return(pvars)
}

fdr2star <- function(fdrs, alpha=0.1){
  fdrs <- ifelse(fdrs<alpha, "*", "")
  fdrs
}


format_pval <- function(pval){
  if (is.character(pval)){
    # pval contains fold change
    pval <- strsplit(pval, ' ')[[1]]
    fc <- pval[2]
    pval <- as.numeric(pval[1])
  }else{
    fc <- ""
  }
  pval <- format.pval(pval, digits = 2)
  if (grepl("<", pval)){
    pval <- gsub("< ?", "", pval)
    pval <- bquote(italic(P) < .(paste(pval, fc)))
  }else{
    pval <- bquote(italic(P) == .(paste(pval, fc)))
  }
  pval
}

# Function to infer response variable
infer_response <- function(ggplot_obj){
  get_y <- function(mapping){
    get_in_parenthesis(strsplit(as.character(mapping)[2],'->')$y)
  }
  dt <- data.table(ggplot_obj$data)
  # Find the layer with raw data
  mapping <- ggplot_obj$mapping
  response <- get_y(mapping)
  # Check for other layers
  maps <- lapply(ggplot_obj$layers, function(i) i$mapping)
  maps <- maps[!sapply(maps, is.null)]
  y <- sapply(maps, get_y)
  if (length(y) == 1 & (y[1] %in% names(dt))){
    if (is.numeric(dt[, get(y)]) & (length(table(dt[, get(y)])) > length(table(dt[, get(response)])))){
      response <- y
      #warning(paste("The inference of raw data column for statistical testing being", response, "might not correct."))
    }
  }else{
    if (length(y) > 1){
      #warning(paste("The inference of raw data column for statistical testing being", response, "might not correct."))
    }
  }
  response
}

#' Add p-values to ggplot objects.
#'
#' @param ggplot_obj ggplot object
#' @param pairs a list pairs of comparison. Groups indicated by integer numbers counted from left to right. e.g. list(c(1, 2), c(2, 3))
#'  will compare first group with second, second group with third
#' @param test character of statistical testing method. e.g. t.test, wilcox.test. Default wilcox.test
#' @param heights integer or vector of integers. The heights of the p-value/annotation. Default maximum value from the data
#' @param barheight tip bar height of the annotation. Default calculated by range_y / 20
#' @param textsize p-value/annotation text size
#' @param pval_text_adj distance of p-value/annotation from annotation bar. Default barheight/2
#' @param annotation text to annotate. If specified, statistical test will not be done
#' @param log whether y axis is log transformed. Default FALSE
#' @param pval_star whether transform pval numbers to stars
#' @param fold_change whether also compute and show fold changes. Default FALSE.
#' @param parse_text whether parse the annotation text (NULL, TRUE, FALSE). If NULL, p-values will be parsed,
#'  text annotations will not. Default NULL.
#' @param response the column that contains the data for statistical testing. Default infer from ggplot object.
#' @param ... additional arguments for statistical testing function (e.g. alternative = "less").
#'
#' @import data.table
#' @import stats
#' @importFrom ggplot2 layer_scales
#'
#' @examples
#' library(ggplot2)
#' library(ggpval)
#' data("PlantGrowth")
#' plt <- ggplot(PlantGrowth, aes(group, weight)) +
#'   geom_boxplot()
#' add_pval(plt, pairs = list(c(1, 3)), test='wilcox.test')
#'
#'
#' @export

add_pval <- function(ggplot_obj,
                     pairs=list(c(1,2),c(1,3)),
                     test="wilcox.test",
                     heights=NULL,
                     barheight=NULL,
                     textsize=5,
                     pval_text_adj=NULL,
                     annotation=NULL,
                     log=FALSE,
                     pval_star=FALSE,
                     fold_change=FALSE,
                     parse_text=NULL,
                     response="infer",
                     ...){
  if (is.null(parse_text)){
    if (is.null(annotation)){
      parse_text <- TRUE
    }else{
      parse_text <- FALSE
    }
  }
  facet <- NULL
  if (class(ggplot_obj$facet)[1] != 'null'){
    if (class(ggplot_obj$facet)[1] == "FacetGrid"){
      facet <- ggplot_obj$facet$params$cols[[1]]
    }else{
      facet <- ggplot_obj$facet$params$facets[[1]]
    }
  }
  if (!is.null(heights)){
    if (length(pairs) != length(heights)){
      pairs <- rep_len(heights, length(pairs))
    }
  }
  ggplot_obj$data <- data.table(ggplot_obj$data)
  ggplot_obj$data$group <- ggplot_obj$data[ ,get(get_in_parenthesis(strsplit(as.character(ggplot_obj$mapping)[1],'->')$x))]
  ggplot_obj$data$group <- factor(ggplot_obj$data$group)
  if (response == "infer"){
    response_ <- infer_response(ggplot_obj)
  }else{
    response_ <- response
  }
  ggplot_obj$data$response <- ggplot_obj$data[ ,get(response_)]
  y_range <- layer_scales(ggplot_obj)$y$range$range
  n_facet <- length(unique(ggplot_obj$data[, eval(facet)]))
  # infer barheight of annotation,
  if (is.null(barheight)){
    barheight <- (y_range[2] - y_range[1]) / 20
  }
  # infer heights to put bar
  if (is.null(heights)){
    heights <- y_range[2] + barheight
    heights <- rep(heights, length=length(pairs))
  }
  if (length(barheight) != length(pairs)){
    barheight <- rep(barheight, length=length(pairs))
  }
  # infer distance pval text above annotation bar
  if (is.null(pval_text_adj)){
    pval_text_adj <- barheight * 0.5
  }
  if (length(pval_text_adj) != length(pairs)){
    pval_text_adj <- rep(pval_text_adj, length=length(pairs))
  }
  if (!is.null(annotation)){
    # check annotation input, if provided
    if ((length(annotation) != length(pairs)) && length(annotation) != n_facet){
      annotation <- rep(annotation, length = length(pairs))
    }
    if (is.list(annotation)){
      if (length(annotation[[1]]) != length(pairs)){
        annotation <- lapply(annotation, function(a) rep(a, length = length(pairs)))
      }
    }
    annotation <- data.frame(annotation) # each row annotate each pair
  }
  # Scale barheight and pval_text_adj log
  if (log){
    barheight <- exp(log(heights) + barheight) - heights
    pval_text_adj <- exp(log(heights) + pval_text_adj) - heights
  }
  V1 <- aes <- annotate <-  geom_line <-  group <-  response <- NULL
  # for each pair, build a data.frame with pathess
  for (i in seq(length(pairs))){
    if (length(unique(pairs[[1]])) != 2){
      stop('Each vector in pairs must have two different groups to compare, e.g. c(1,2) to compare first and second box.')
    }
    test_groups <- levels(ggplot_obj$data$group)[pairs[[i]]]
    # subset the data to calculate p-value
    data_2_test <- ggplot_obj$data[ggplot_obj$data$group %in% test_groups,]
    # statistical test
    if (!is.null(facet)){
      if (class(ggplot_obj$data[, eval(facet)]) != 'factor'){
        facet_level <-levels(as.factor(ggplot_obj$data[, eval(facet)]))
      }else{
        facet_level <- levels(ggplot_obj$data[, eval(facet)])
      }
      pval <- data_2_test[, lapply(.SD, function(i) get(test)(response ~ as.character(group), ...)$p.value),
                          by=facet,
                          .SDcols=c('response','group')]
      pval <- pval[,facet := factor(facet, levels = facet_level)][order(facet), group]
    }else{
      pval <- get(test)(data=data_2_test, response ~ group, ...)$p.value
      if (fold_change){
        fc <- data_2_test[, median(response), by = group][order(group)][, .SD[1] / .SD[2], .SDcols='V1'][ ,V1]
        fc <- paste0('FC=', round(fc, digits = 2))
        pval <- paste(pval, fc)
      }
    }
    # convert pval to stars if needed
    if (pval_star){
      pval <- pvars2star(pval)
      annotation <- pval
    }
    # make data from of label path, the annotation path for each facet is the same
    height <- heights[i]
    df_path <- data.frame(group=rep(pairs[[i]], each=2),
                          response=c(height, height+barheight[i], height+barheight[i], height))
    ggplot_obj <- ggplot_obj + geom_line(data=df_path, aes(x=group, y=response), inherit.aes = F)
    # start annotation
    if (is.null(annotation)){ # assume annotate with p-value
      labels <- sapply(pval, function(i) deparse(format_pval(i)))
    }else{
      labels <- unlist(annotation[i,])
    }
    ggplot_obj <- ggplot_obj + annotate("text",
                                        x = (pairs[[i]][1]+pairs[[i]][2])/2,
                                        y = height+barheight[i]+pval_text_adj[i],
                                        label = labels,
                                        size = textsize,
                                        parse=parse_text,
                                        vjust="bottom")
  }
  ggplot_obj
}
