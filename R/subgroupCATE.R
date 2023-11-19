utils::globalVariables("study")
utils::globalVariables("tau.hat")
utils::globalVariables("value")
utils::globalVariables("variable")
utils::globalVariables("theme_bw")
utils::globalVariables("aes")
utils::globalVariables("..density..")
utils::globalVariables("lowerinterval")
utils::globalVariables("upperinterval")

#' @title
#' subgroupCATE
#'
#' @description
#' Takes a dataset augmented with individual CATE estimates and visualizes tau.hat distribution (CATE + CI)
#' grouped by specified subgroups to assess HTE.

#' @export
#' @param cfdf; dataframe (study data augmented with cate and ci)
#' @param outCol character; name of outcome column
#' @param txCol character; name of treatment status column
#' @param catCols vector; column names of categorical covariates to subgroup
#' @param numCols vector; column names numerical covariates to subgroup
#' @param combine logical; combine data from multiple studies
#' @return list; list of ggplot objects; figure with individual CATE estimate visualizations by subgroup
#'
subgroupCATE <- function(cfdf, outCol, txCol, covList, combine
                         ){

  cfdf <- as.data.frame(sjlabelled::unlabel(cfdf, verbose = FALSE))

  for (v in colnames(cfdf)){
    if(length(unique(cfdf[,v])) < 6){
      cfdf[,v] <- as.factor(cfdf[,v])
    }
  }

  sfdf <- dplyr::select(cfdf, c(covList))

  dfm <- reshape2::melt(cfdf, id.vars=c("study", "tau.hat", "lowerinterval", "upperinterval"))
  dfm <- dplyr::filter(dfm , !variable %in%  c("X", "X.Intercept.", outCol, txCol))

  catCols <- colnames(dplyr::select_if(sfdf, is.factor))
  catCols <- c(catCols, colnames(dplyr::select_if(sfdf, is.character)))
  numCols <- colnames(dplyr::select_if(sfdf, is.numeric))

  dfCat <- dplyr::filter(dfm , variable %in% c("tau.hat", catCols))
  dfNum <- dplyr::filter(dfm , variable %in% c(numCols))

# Distribution by categorical variable
  if(combine==F){
  densCat <- ggplot2::ggplot(dfCat, ggplot2::aes(x = tau.hat,
                             fill = value,
                             group = value)) +

    ggplot2::geom_density(ggplot2::aes(y=ggplot2::after_stat(density),
                                       fill=value),
                 alpha=0.5) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title=ggplot2::element_blank()) +
    ggplot2::ggtitle("Effect Estimate Distribution by Subgroup and Study: Categorical Covariates") +
    ggplot2::facet_wrap(~ variable + study,
                        ncol=length(unique(cfdf$study)))

  # Distribution by continuous variable


  tauNum <- ggplot2::ggplot(dfNum,
                            ggplot2::aes(x = value,
                       y=tau.hat )) +
    ggplot2::geom_hline(yintercept = 0, color="red") +
    ggplot2::geom_point(position="identity",
               fill="#515A71", size=0.5,
               alpha=0.5,
               shape=21) +

    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lowerinterval, ymax = upperinterval),
      color="#615A71", alpha=0.5
    ) +
    ggplot2::geom_smooth(alpha=0.5, color="blue") +
    ggplot2::ylab("Tau.hat") +
    ggplot2::ggtitle("Effect Estimate Distribution by Subgroup and Study: Numeric Covariates") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous() +
    ggplot2::facet_wrap(~ variable + study, ncol=length(unique(cfdf$study)))} else{

      densCat <- ggplot2::ggplot(dfCat, ggplot2::aes(x = tau.hat,
                                                     fill = value,
                                                     group = value)) +

        ggplot2::geom_density(ggplot2::aes(y=ggplot2::after_stat(density),
                                           fill=value),
                              alpha=0.5) +
        ggplot2::ggtitle("Effect Estimate Distribution by Subgroup, Combined Studies: Categorical Covariates") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title=ggplot2::element_blank()) +
        ggplot2::facet_wrap(~ variable)

      # Distribution by continuous variable


      tauNum <- ggplot2::ggplot(dfNum,
                                ggplot2::aes(x = value,
                                             y=tau.hat )) +
        ggplot2::geom_hline(yintercept = 0, color="red") +
        ggplot2::geom_point(position="identity",
                            fill="#515A71", size=0.5,
                            alpha=0.5,
                            shape=21) +

        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = lowerinterval, ymax = upperinterval),
          color="#615A71", alpha=0.5
        ) +
        ggplot2::geom_smooth(alpha=0.5, color="blue") +
        ggplot2::ylab("Tau.hat") +
        ggplot2::ggtitle("Effect Estimate Distribution by Subgroup, Combined Studies: Numeric Covariates") +
        ggplot2::theme_bw() +
        ggplot2::scale_y_continuous() +
        ggplot2::facet_grid(~ variable, scales="free")
    }

return(list(densCat, tauNum))
}


