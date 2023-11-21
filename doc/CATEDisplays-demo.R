## ----include = FALSE----------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE, fig.width=10, fig.height=5) 
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
if(!require(devtools)){
    install.packages("devtools", repos = "http://cran.us.r-project.org")
}

if(!require(CATEDisplays)){
devtools::install_github("StuartRA/CATEDisplays")
}


## -----------------------------------------------------------------------------
library(CATEDisplays)

## ----results='asis'-----------------------------------------------------------

df1 <- data.frame(matrix(NA, nrow = 250, ncol = 1))
df2 <- data.frame(matrix(NA, nrow = 50, ncol = 1))
df3 <- data.frame(matrix(NA, nrow = 450, ncol = 1))
df4 <- data.frame(matrix(NA, nrow = 350, ncol = 1))
df5 <- data.frame(matrix(NA, nrow = 150, ncol = 1))

## ----results='asis'-----------------------------------------------------------
df1$sex <- as.factor(sample(c('Male', 'Female'), 250, replace=TRUE))
df2$sex <- as.factor(sample(c('Male', 'Female'), 50, replace=TRUE))
df3$sex <- as.factor(sample(c('Male', 'Female'), 450, replace=TRUE))
df4$sex <- as.factor(sample(c('Male', 'Female'), 350, replace=TRUE))
df5$sex <- as.factor(sample(c('Male', 'Female'), 150, replace=TRUE))

## ----results='asis'-----------------------------------------------------------
df1$age <- unlist(lapply(rnorm(250, 40, 15), function (x) {max(sample(c(18:20)), round(x, 2))}))
df2$age <- unlist(lapply(rnorm(50, 30, 15), function (x) {max(sample(c(18:20)), round(x, 2))}))
df3$age <- unlist(lapply(rnorm(450, 25, 15), function (x) {max(sample(c(18:20)), round(x, 2))}))
df4$age <- unlist(lapply(rnorm(350, 20, 15), function (x) {max(sample(c(18:20)), round(x, 2))}))
df5$age <- unlist(lapply(rnorm(150, 40, 15), function (x) {max(sample(c(18:20)), round(x, 2))}))

## ----results='asis'-----------------------------------------------------------
df1$educ <- as.factor(sample(c('Less than HS', 'HS', 'College', 'Graduate'), 250, replace=TRUE))
df2$educ <- as.factor(sample(c('Less than HS', 'HS', 'College', 'Graduate'), 50, replace=TRUE))
df3$educ <- as.factor(sample(c('Less than HS', 'HS', 'College', 'Graduate'), 450, replace=TRUE))
df4$educ <- as.factor(sample(c('Less than HS', 'HS', 'College', 'Graduate'), 350, replace=TRUE))
df5$educ <- as.factor(sample(c('Less than HS', 'HS', 'College', 'Graduate'), 150, replace=TRUE))

## ----results='asis'-----------------------------------------------------------
df1$race <- as.factor(sample(c('White', 'Black', 'Asian', 'Hispanic', 'Other'), 250, replace=TRUE))
df2$race <- as.factor(sample(c('White', 'Black', 'Asian', 'Hispanic', 'Other'), 50, replace=TRUE))
df3$race <- as.factor(sample(c('White', 'Black', 'Asian', 'Hispanic', 'Other'), 450, replace=TRUE))
df4$race <- as.factor(sample(c('White', 'Black', 'Asian', 'Hispanic', 'Other'), 350, replace=TRUE))
df5$race <- as.factor(sample(c('White', 'Black', 'Asian', 'Hispanic', 'Other'), 150, replace=TRUE))

## ----results='asis'-----------------------------------------------------------
df1$ASIalc <- rnorm(250, 0, 0.55)
df2$ASIalc <- rnorm(50, 0, 0.25)
df3$ASIalc <- rnorm(450, 0, 0.15)
df4$ASIalc <- rnorm(350, 0, 0.25)
df5$ASIalc <- rnorm(150, 0, 0.35)

## ----results='asis'-----------------------------------------------------------
df1$outc <- rnorm(250, 0.6, 0.75)
df2$outc <- rnorm(50, 0.6, 0.75)
df3$outc <- rnorm(450, 0.6, 0.75)
df4$outc <- rnorm(350, 0.6, 0.75)
df5$outc <- rnorm(150, 0.6, 0.75)

## ----results='asis'-----------------------------------------------------------
df1$tx <- sample(c(1, 0), 250, replace=TRUE, prob = c(0.5, 0.5))
df2$tx <- sample(c(1, 0), 50, replace=TRUE, prob = c(0.25, 0.75))
df3$tx <- sample(c(1, 0), 450, replace=TRUE, prob =c(0.3, 0.7))
df4$tx <- sample(c(1, 0), 350, replace=TRUE, prob = c(0.5, 0.5))
df5$tx <- sample(c(1, 0), 150, replace=TRUE, prob = c(0.45, 0.55))

## ----results='asis'-----------------------------------------------------------
dfList <- list(df1, df2, df3, df4, df5)

## ----results='asis'-----------------------------------------------------------
catesDisplay <- CATEDisplays::displayCATE(
                           dfList=dfList,
                           outCol="outc",
                           txCol="tx",
                           covList= c("age", "race", "educ", "sex", "ASIalc"),
                           blpredList=NULL,
                           combine=F,
                           verbose=T,
                           ci=0.95,
                           nTrees=1000,
                           seedN=NA)

## ----message=FALSE, warning=FALSE---------------------------------------------
studyNum = 2
cateOutput <- catesDisplay$cateOutput[[studyNum]]

## ----message=FALSE, warning=FALSE---------------------------------------------
cateOutput$ATE

## ----message=FALSE, warning=FALSE---------------------------------------------
cateOutput$testHTE

## ----message=FALSE, warning=FALSE---------------------------------------------
head(cateOutput$cateDF, 5)

## ----message=FALSE, warning=FALSE---------------------------------------------
cateOutput$BLP

## ----results='asis',  message=FALSE, warning=FALSE----------------------------
catesDisplay$vizTauHat

## ----results='asis',  message=FALSE, warning=FALSE, fig.height=7, fig.width=10----
catesDisplay$vizSubgroupCATE

## ----results='asis',  message=FALSE, warning=FALSE----------------------------
catesDisplay$vizblp

## ----results='asis', message=FALSE, warning=FALSE-----------------------------
catesDisplayCombined <- CATEDisplays::displayCATE(
                                  dfList=dfList,
                                  outCol="outc",
                                  txCol="tx",
                                  covList= c("age", "race", "educ",
                                             "sex", "ASIalc"),
                                  blpredList=NULL,
                                  combine=T,
                                  ci=0.95,
                                  verbose=T,
                                  nTrees=10000,
                                  seedN=NA)

## ----message=FALSE, warning=FALSE---------------------------------------------
cateOutput <- catesDisplayCombined$cateOutput

## ----message=FALSE, warning=FALSE---------------------------------------------
cateOutput$ATE

## ----message=FALSE, warning=FALSE---------------------------------------------
cateOutput$testHTE

## ----message=FALSE, warning=FALSE---------------------------------------------
head(cateOutput$cateDF, 5)

## ----message=FALSE, warning=FALSE---------------------------------------------
cateOutput$BLP

## ----results='asis',  message=FALSE, warning=FALSE,  fig.height=6, fig.width=10----
catesDisplayCombined$vizTauHat

## ----results='asis',  message=FALSE, warning=FALSE,  fig.height=6, fig.width=10----
catesDisplayCombined$vizSubgroupCATE

## ----results='asis',  message=FALSE, warning=FALSE,  fig.height=12, fig.width=12----
vizBLP(cateOutput, combine=T)

