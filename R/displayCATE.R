utils::globalVariables("study")
utils::globalVariables("dfList")

#' @title
#' displayCATE

#' @description
#' Estimates CATE using causal forests with data from multiple trials
#' and displays multiple treatment effect heterogeneity diagnostics by subgroup.
#'
#' @export
#' @param dfList list; a list of dataframes, one per trial
#' @param outCol character; name of outcome column
#' @param txCol character; name of treatment status column
#' @param covList vector; column names of covariates to estimate forest
#' @param blpredList vector; number of units to be initially randomly selected
#' @param combine logical; combine data from multiple studies
#' @param ci numeric; confidence interval %
#' @param nTrees numeric; column names of unit level variables  to sample units on
#' @param seedN numeric; seed number to be used for sampling. If NA, calls set.seed(); default = NA
#' @return list with:
#' 1) full output from getCATE(), 2) tauHat figures, 3) sub-group CATE figures, 4) best linear projection figures

displayCATE <- function(dfList,
                      outCol,
                      txCol,
                      covList,
                      blpredList,
                      combine,
                      verbose,
                      ci,
                      nTrees,
                      seedN){

  if(class(dfList) != "list"){

    dfList <- as.list(dfList)
    }

  # 1. Run main function for each study:

  if(combine==T){

    df <- dfList[[1]]

    df$study <- as.factor(paste0("Study ", 1))

    for(i in 2:length(dfList)){
      dfi <- dfList[[i]]
      dfi$study <- as.factor(paste0("Study", i))
      df <- plyr::rbind.fill(df, dfi)
    }

    res <- getCATE(df,
                 outCol,
                 txCol,
                 covList,
                 blpredList,
                 combine,
                 verbose,
                 ci,
                 nTrees,
                 seedN)

    dfviz <- res$cateDF
    dfviz$study <- df$study

    # 2. Produce CATE estimates figures, all studies:
    vizTauHat <- vizTauHat(dfviz, combine=T)
    vizSubgroupCATE <- subgroupCATE(dfviz, outCol, txCol, covList, combine=T)

    # 3. Produce figure for best linear projection  results, all studies:
    vizblp <- vizBLP(res, combine=T)

  } else{

    res <- lapply(dfList, function(x) {getCATE(x, outCol,
                                             txCol,
                                             covList,
                                             blpredList,
                                             combine,
                                             verbose,
                                             ci,
                                             nTrees,
                                             seedN)})

    df <- res[[1]]$cateDF
    df$study <- as.factor(paste0("Study ", 1))

    for(i in 2:length(dfList)){
      dfi <- res[[i]]$cateDF
      dfi$study <- as.factor(paste0("Study ", i))
      df <- plyr::rbind.fill(df, dfi)
    }

    df[,outCol] <- df$Y
    df[,txCol] <- df$tx

    # 2. Produce CATE estimates figures, all studies:
    vizTauHat <- vizTauHat(df, combine=F)
    vizSubgroupCATE <- subgroupCATE(df, outCol, txCol, covList, combine=F)

    # 3. Produce figure for best linear projection  results, all studies:
    vizBLP <- vizBLP(res, combine=F)

  }

  return(list(cateOutput=res,
              vizTauHat=vizTauHat,
              vizSubgroupCATE=vizSubgroupCATE,
              vizBLP=vizBLP
              )
  )
}




