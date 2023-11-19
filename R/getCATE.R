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
#' @param blpredList vector;
#' @param combine logical; combine data from multiple studies
#' @param ci numeric; confidence interval %
#' @param nTrees numeric; column names of unit level variables  to sample units on
#' @param seedN numeric; seed number to be used; default = NA
#' @return list with: 1) , 2),  3)
#'
getCATE <- function(stdf,
                  outCol,
                  txCol,
                  covList,
                  blpredList,
                  combine,
                  ci, nTrees,
                  seedN){

      set.seed(seedN)

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
      }

      fm <- stats::as.formula(paste("~ 1 +", paste(covList, collapse= "+")))
      X <- stats::model.matrix(fm,  data=stdf)

      Y.forest = grf::regression_forest(X , Y)
      Y.hat = stats::predict (Y.forest)$predictions
      W.forest = grf::regression_forest (X , W)
      W.hat = stats::predict(W.forest )$ predictions

      # causal forest:
      cf = grf::causal_forest(X=X,
                           Y=Y,
                           W=W,
                           num.trees = nTrees,
                           )

      ATE = grf::average_treatment_effect(cf)
      print(paste("-- 95% CI for the ATE:", round(ATE[1], 6),
            "+/-", round(qnorm(ci + (1-ci)/2) * ATE[2], 6)))


      # tx heterogeneity (agnostic) test:
      testHTE <- grf::test_calibration(cf)

      print(paste(
                  "Estimate: ", round(testHTE[[2]],3), "||",
                  "Pr(>t): ", round(testHTE[[8]], 3)
      ))

      if(round(testHTE[[2]],3) > 0 & round(testHTE[[8]], 3) < 0.05){
      print("The `differential.forest.prediction` coefficient is significantly greater than 0: we can reject the null of no heterogeneity.")
      } else{
        print("The `differential.forest.prediction` coefficient is not significantly greater than 0: we cannot reject the null of no heterogeneity.")
      }

      #COMPARISON TABLES (tau>0 vs. tau <=0, t.test)
      tau.hat = stats::predict(cf)$predictions

      prediction.result = stats::predict(cf, X, estimate.variance=TRUE)
      standard.error = sqrt(prediction.result$variance.estimates)

      lowerinterval = tau.hat-(1+ci)*standard.error
      upperinterval = tau.hat+(1+ci)*standard.error


      # data to do the subgroup descriptives (visuals):
      CTN_output <- as.data.frame(cbind(tau.hat, lowerinterval,
                                      upperinterval, Y, W, X))
      CTN_output$indID <- as.factor(c(1:nrow(CTN_output)))
      covList2 <- unlist(lapply(covList, function(x) {if(! x %in% colnames(CTN_output)){return(x)}}))
      CTN_output <- dplyr::inner_join(dplyr::select(stdf, c("indID", covList2)),
                              CTN_output, by = "indID")

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
                  CTN=CTN_output,
                  BLP=blp)) # list with 5 elements
}




