
utils::globalVariables("study")
utils::globalVariables("tau.hat")
utils::globalVariables("row_number")
utils::globalVariables("rowname")
utils::globalVariables("aes")
utils::globalVariables("row_number")
utils::globalVariables("lowerinterval")
utils::globalVariables("upperinterval")

#' vizTauHat

#' Takes a dataset augmented with individual CATE estimates and visualizes overall tau.hat distribution (CATE + CI) .
#'
#'
#' @export
#' @param cfdf; dataframe (study data augmented with cate and ci)
#' @return ggplot object; figure with individual CATE estimate visualizations
#'
vizTauHat <- function(cfdf, combine){


  ##### Overall tau.hat plot

  if(combine==F){

  dfOrd <- dplyr::group_by(cfdf, study)
  } else{dfOrd <- cfdf}

  dfOrd <- dplyr::arrange(dfOrd, tau.hat)
  dfOrd <- dplyr::mutate(dfOrd, rowname = dplyr::row_number() - 1)
  dfOrd <- dplyr::ungroup(dfOrd)


  ####### Plot

  dtaus <- ggplot2::ggplot(dfOrd,
                           ggplot2::aes(x = rowname,
             y=tau.hat)) +
    ggplot2::geom_hline(yintercept = 0, color="darkred") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lowerinterval, ymax = upperinterval) ,
      alpha=0.5, color="gray",
    ) +
    ggplot2::geom_point(position="identity", size=0.85, shape=21, alpha=0.65,
               color="darkgray") +
    ggplot2::ylab("Tau.hat + CI*") +
    ggplot2::xlab("") +
    ggplot2::ggtitle("Effect Estimate with Confidence Intervals (All Studies)") +
    ggplot2::theme_bw()

  if(combine==F){
    dtaus <- dtaus + ggplot2::facet_grid(~ study, scale = "free_x")
  }

  return(dtaus)
}
