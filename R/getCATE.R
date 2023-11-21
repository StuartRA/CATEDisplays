utils::globalVariables("qnorm")

#' @title
#' getCATE
#'
#' @description
#'  Takes a dataset with data coming from one or more trials and: 1) Estimates the CATE */ using a causal forest model;
#'  2) Calculates overall ATE; 3) Carries out an agbostic test for the presence of treatment effect heterogeneity;
#'  4) Computes the best linear projection* of CATE as a function of user-provided covariates; 6)
#'  5) Visually displays resulting coefficients;
#'  6) Augments original datasets with CATE + CI.
#'
#' /* Let tau(Xi) = E(Y(1) - Y(0) | X = Xi) be the CATE, and Ai be a vector of user-provided covariates.
#' This then function provides a (doubly robust) fit to the linear model tau(Xi) ~ beta_0 + Ai * beta."

#' @export
#' @param stdf dataframe; trial data (single study)
#' @param outCol character; name of outcome column
#' @param txCol character; name of treatment status column
#' @param covList vector; column names of covariates to estimate forest
#' @param blpredList vector; column names of covariates to compute best linear projection; default=NULL (if NULL, same as covList)
#' @param combine logical; combine data from multiple studies; default=FALSE
#' @param verbose logical; print ATE and heterogeneity test results; default=TRUE
#' @param ci numeric; confidence interval %; default=0.95
#' @param nTrees numeric; number of trees in the causal forest; default=10000
#' @param seedN numeric; seed number to be used for reproducibility; default = NA
#' @return list with:
#' 1) causal forest model; 2) ATE + CI; 3) heterogeneity test; 4) trial dataset augmented with CATE+CI;
#'  5) best linear projection model
#'
getCATE <- function(stdf,
                  outCol,
                  txCol,
                  covList,
                  blpredList,
                  combine,
                  verbose=TRUE,
                  ci=0.95,
                  nTrees=10000,
                  seedN=NA, i=NA){

      if(! is.na(seedN)){
        set.seed(seedN)
      }

      stdf <- as.data.frame(stdf)
      stdf <- sjlabelled::unlabel(stdf, verbose = FALSE)
      stdf$indID <- as.factor(c(1:nrow(stdf)))

      Y <- as.matrix(stdf[,outCol])
      colnames(Y) <- c('Y')

      W <- as.matrix(stdf[,txCol])
      colnames(W) <- c('tx')


      for(col in covList){
        c=class(stdf[,col])
        if(c != "numeric" | length(unique(stdf[, col])) < 6){
        stdf[, col] <- as.factor(stdf[, col])
        }
      }

      if(combine==T){
        covList <- c("study", covList)
        i = "All"
      }

      fm <- stats::as.formula(paste("~ 1 +", paste(covList, collapse= "+")))
      X <- stats::model.matrix(fm,  data=stdf)

      Y.forest = grf::regression_forest(X , Y)
      Y.hat = stats::predict (Y.forest)$predictions
      W.forest = grf::regression_forest (X , W)
      W.hat = stats::predict(W.forest )$predictions

      # causal forest:
      cf = grf::causal_forest(X=X,
                           Y=Y,
                           W=W,
                           num.trees = nTrees,
                           )

      # overall ATE:
      ATE = grf::average_treatment_effect(cf)
      # tx heterogeneity (agnostic) test:
      testHTE <- grf::test_calibration(cf)

      if(verbose==TRUE){
          print(paste("Study: ", as.character(i)))

          print(paste("-- 95% CI for the ATE:", round(ATE[1], 6),
                "+/-", round(qnorm(ci + (1-ci)/2) * ATE[2], 6)))

          print(paste(
                      "Estimate: ", round(testHTE[[2]],3), "||",
                      "Pr(>t): ", round(testHTE[[8]], 3)
          ))

          if(round(testHTE[[2]],3) > 0 & round(testHTE[[8]], 3) < 0.05){
          print("The `differential.forest.prediction` coefficient is significantly > 0: we can reject the null of no heterogeneity.")
          } else{
            print("The `differential.forest.prediction` coefficient is not significantly > 0: we cannot reject the null of no heterogeneity.")
          }
      }

      #COMPARISON TABLES (tau>0 vs. tau <=0, t.test)
      tau.hat = stats::predict(cf)$predictions

      prediction.result = stats::predict(cf, X, estimate.variance=TRUE)
      standard.error = sqrt(prediction.result$variance.estimates)

      lowerinterval = tau.hat-(1+ci)*standard.error
      upperinterval = tau.hat+(1+ci)*standard.error


      # data to do the subgroup descriptives (visuals):
      cateDF <- as.data.frame(cbind(tau.hat, lowerinterval,
                                      upperinterval, Y, W, X))
      cateDF$indID <- as.factor(c(1:nrow(cateDF)))
      covList2 <- unlist(lapply(covList, function(x) {if(! x %in% colnames(cateDF)){return(x)}}))
      cateDF <- dplyr::inner_join(dplyr::select(stdf, c("indID", covList2)),
                              cateDF, by = "indID")

      # Best linear projection: we need the causal forest and covariate set for each study,
      #computed by the above, code for each corresponding dataset.

      # compute best linear predictor (and save the output):
      if(is.null(blpredList)){
        Xblp=X } else{
          fm <- stats::as.formula(paste("~ 1 +", paste(blpredList, collapse= "+")))
          Xblp <- stats::model.matrix(fm,  data=stdf)
          }
        blp <- grf::best_linear_projection(cf, Xblp)

      return(list(cf=cf,
                  ATE=ATE,
                  testHTE = testHTE,
                  cateDF=cateDF,
                  BLP=blp)) # list with 5 elements
}




