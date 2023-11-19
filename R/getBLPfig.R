utils::globalVariables("study")
utils::globalVariables("estimate")
utils::globalVariables("term")
utils::globalVariables("aes")
utils::globalVariables("conf.low")
utils::globalVariables("conf.high")

theme_MH <- ggplot2::theme(plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.15), color="lightcyan4",
                                               hjust = 0, vjust = 2),
                  plot.background = ggplot2::element_rect(fill = "white"),
                  panel.background = ggplot2::element_rect(colour = "grey80", fill = 'white'),
                  panel.grid.major = ggplot2::element_line(colour = "grey90", size=0.20),
                  axis.text = ggplot2::element_text(size=12, colour = "grey50", vjust = 0),
                  axis.text.x = ggplot2::element_text(angle=90),
                  axis.ticks.x = ggplot2::element_line(colour = "grey50"),
                  axis.ticks.y = ggplot2::element_line(colour = "grey50"),
                  axis.ticks.length = ggplot2::unit(0.25, "cm"),
                  axis.title.y = ggplot2::element_text( angle = 90,
                                               vjust = 0),
                  axis.title.x = ggplot2::element_text(angle = 0))

## Function: getBLPfig()

#' Takes the best linear projection* of the CATE as a function
#'  of of user-provided covariates and visually displays resulting coefficients.
#' * Let tau(Xi) = E(Y(1) - Y(0) | X = Xi) be the CATE, and Ai be a vector of user-provided covariates.
#' This function provides a (doubly robust) fit to the linear model tau(Xi) ~ beta_0 + Ai * beta."
#'
#' @export
#' @param res list;
#' @param combine logical; combine data from multiple studies
#' @return ggplot object; figure with model results
#'
getBLPfig <- function(res, combine) {

   if(combine == F){
      blpList <- lapply(res, function(x) {x$BLP})

    # 2. Plot all models together:
    model.names <- unlist(lapply(c(1:length(blpList)), function(i) {paste0("Study ", i)}))
    blps <- jtools::plot_summs(blpList,
                             point.shape = FALSE,
                             model.names=model.names)

    dfg <- blps$data
    dfg$study <- as.factor(dfg$model)
    dfg$study <- factor(dfg$study , levels=c(model.names))

    ### Visualize:

    ggBLP <- ggplot2::ggplot(dfg,
                             ggplot2::aes(x=study, y=estimate,
                         color=term, group=term),
    ) +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = 0, linetype="dashed", color ="black", size=0.65)+
      ggplot2::geom_pointrange(position = ggplot2::position_dodge(width =0.75),
                               ggplot2::aes(ymin=conf.low, ymax=conf.high),
                      alpha=0.85) +

      ggplot2::ggtitle("CATE best linear projection by Study and Covariate") +
      theme_MH +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::facet_wrap(~ study, scales = "free") +
      ggplot2::xlab("") +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
      ) }else{

        blpList <- res$BLP

        blps <- jtools::plot_summs(blpList,
                                   point.shape = FALSE)
        dfg <- blps$data
        dfg$study <- dfg$model

        ### Visualize:

        ggBLP <- ggplot2::ggplot(dfg,
                                 ggplot2::aes(x=study, y=estimate,
                            color=term, group=term),
        ) +
          ggplot2::coord_flip() +
          ggplot2::geom_hline(yintercept = 0, linetype="dashed", color ="black", size=0.65)+
          ggplot2::geom_pointrange(position = ggplot2::position_dodge(width =0.75),
                                   ggplot2::aes(ymin=conf.low, ymax=conf.high),
                          alpha=0.85) +

          ggplot2::ggtitle("CATE best linear projection by Covariate") +
          theme_MH +
          ggplot2::xlab("") +
          ggplot2::ylab("") +
          ggplot2::theme(
            axis.text.y = ggplot2::element_blank(),
          )

  }
    return(ggBLP)
}



