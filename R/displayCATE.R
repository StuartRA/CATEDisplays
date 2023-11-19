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
#' @return list with: 1) , 2),  3)

displayCATE <- function(dfList,
                      outCol,
                      txCol,
                      covList,
                      blpredList=NULL,
                      combine=FALSE,
                      ci=0.95, nTrees=10000, seedN=7203){

  if(class(dfList) != "list"){
    dfList <- as.list(dfList) }

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
                 ci, nTrees,
                 seedN)

    dfviz <- res$CTN
    dfviz$study <- df$study

    # 2. Produce CATE estimates figures, all studies:
    tauHat <- vizTauHat(dfviz, combine=T)
    subgroupCATE <- subgroupCATE(dfviz, outCol, txCol, covList, combine=T)

    # 3. Produce figure for best linear projection  results, all studies:
    blpF <- vizBLP(res, combine=T)

  } else{

    res <- lapply(dfList, function(x) {getCATE(x, outCol,
                                             txCol,
                                             covList,
                                             blpredList,
                                             combine,
                                             ci, nTrees,
                                             seedN)})

    df <- res[[1]]$CTN
    df$study <- as.factor(paste0("Study ", 1))

    for(i in 2:length(dfList)){
      dfi <- res[[i]]$CTN
      dfi$study <- as.factor(paste0("Study ", i))
      df <- plyr::rbind.fill(df, dfi)
    }

    df[,outCol] <- df$Y
    df[,txCol] <- df$tx

    # 2. Produce CATE estimates figures, all studies:
    tauHat <- vizTauHat(df, combine=F)
    subgroupCATE <- subgroupCATE(df, outCol, txCol, covList, combine=F)

    # 3. Produce figure for best linear projection  results, all studies:
    blpF <- vizBLP(res, combine=F)

  }

  return(list(res, tauHat, subgroupCATE, blpF)
  )
}




