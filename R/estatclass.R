#' Classification statistics and table
#'
#' Produces a classification table and statistics given a binary response model.
#' @param model The regression model that was stored prior
#' @param dep.var The observed dependent variable (with data frame as prefix, "df$dep.var")
#' @param prob_cut cut-off point at which the predicted probabilities should be coded binary (0,1). Usually 0.5 is used to indicate >0.5 as 1 and <0.5 as 0
#' @return Different class-values and a list of them
#' @export

estat_class <- function(model, dep.var, prob_cut){

  ## Predicting yhat whilst dealing with MV
  estat_class_model <- update(model,na.action=na.exclude)
  yhat <- predict(estat_class_model, type = "response")

  ## Creating indicator variable for yhat at cut point
  predictions <- ifelse(yhat<prob_cut, 0, 1)

  ## Generating statistics

  # Classification table
  class_1 <- as.matrix(table(predictions, dep.var))
  rownames(class_1) <- c("Predic. 0", "Predic. 1")
  colnames(class_1) <- c("True 0", "True 1")

  # Sensitivity (true positives)
  class_2 <- (class_1[2,2]/(class_1[2,2]+class_1[1,2]))*100
  names(class_2) <- "Sensitivity or true positive rate (TPR) %"

  # Specificity (true negatives)
  class_3 <- (class_1[1,1]/(class_1[1,1]+class_1[2,1]))*100
  names(class_3) <- "Specificity or true negative rate (TNR) %"

  # False Positives // Einfacher 100 - Sensitivity
  class_4 <- (class_1[1,2]/(class_1[2,2]+class_1[1,2]))*100
  names(class_4) <- "Miss rate or false negative rate (FNR) %"

  # False Negatives // Einfacher 100 - Specificity
  class_5 <- (class_1[2,1]/(class_1[1,1]+class_1[2,1]))*100
  names(class_5) <- "Fall-out or false positive rate (FPR) %"

  # Precision or positive predictive value (PPV) // Einfacher 100 - Specificity
  class_6<- (class_1[2,2]/(class_1[2,2]+class_1[2,1]))*100
  names(class_6) <- "Precision or positive predictive value (PPV) %"

  # False Negatives // Einfacher 100 - Specificity
  class_7 <- 100 - class_6
  names(class_7) <- "False discovery rate (FDR) %"

  # R²-Count - Correctly Classified or accuracy (ACC)
  class_8 <- ((class_1[1,1]+class_1[2,2])/sum(class_1))*100
  names(class_8) <- "R²-Count or accuracy (ACC) %"

  # Adjusted R²-Count - Correctly Classified
  class_9 <- (((class_1[1,1]+class_1[2,2])-max(colSums(class_1)))/((sum(class_1))-max(colSums(class_1))))*100
  names(class_9) <- "Adj. R²-Count % (Long 1997: 108)"

  estat_classification <- list(class_1,class_2,class_3, class_4, class_5, class_6, class_7, class_8, class_9)
  estat_classification
}

#' Classification statistics and table
#'
#' Produces an extended classification table and statistics given a binary response model.
#' @param model The regression model that was stored prior
#' @param dep.var The observed dependent variable (with data frame as prefix, "df$dep.var")
#' @param prob_cut cut-off point at which the predicted probabilities should be coded binary (0,1). Usually 0.5 is used to indicate >0.5 as 1 and <0.5 as 0
#' @return Different class-values and a list of them
#' @export

extat_class <- function(model, dep.var, prob_cut){

  ## Predicting yhat whilst dealing with MV
  estat_class_model <- update(model,na.action=na.exclude)
  yhat <- predict(estat_class_model, type = "response")

  ## Creating indicator variable for yhat at cut point
  predictions <- ifelse(yhat<prob_cut, 0, 1)

  ## Generating statistics

  # Classification table
  class_1 <- as.matrix(table(predictions, dep.var))
  rownames(class_1) <- c("Predic. 0", "Predic. 1")
  colnames(class_1) <- c("True 0", "True 1")

  # Sensitivity (true positives)
  class_2 <- (class_1[2,2]/(class_1[2,2]+class_1[1,2]))*100
  names(class_2) <- "Sensitivity or true positive rate (TPR) %"

  # Specificity (true negatives)
  class_3 <- (class_1[1,1]/(class_1[1,1]+class_1[2,1]))*100
  names(class_3) <- "Specificity or true negative rate (TNR) %"

  # False Positives // Einfacher 100 - Sensitivity
  class_4 <- (class_1[1,2]/(class_1[2,2]+class_1[1,2]))*100
  names(class_4) <- "miss rate or false negative rate (FNR) %"

  # False Negatives // Einfacher 100 - Specificity
  class_5 <- (class_1[2,1]/(class_1[1,1]+class_1[2,1]))*100
  names(class_5) <- "fall-out or false positive rate (FPR) %"

  # Precision or positive predictive value (PPV)
  class_6<- (class_1[2,2]/(class_1[2,2]+class_1[2,1]))*100
  names(class_6) <- "Precision or positive predictive value (PPV) %"

  # False Negatives // Einfacher 100 - Specificity
  class_7 <- 100 - class_6
  names(class_7) <- "false discovery rate (FDR) %"

  # R²-Count - Correctly Classified or accuracy (ACC)
  class_8 <- ((class_1[1,1]+class_1[2,2])/sum(class_1))*100
  names(class_8) <- "R²-Count or accuracy (ACC) %"

  # Adjusted R²-Count - Correctly Classified
  class_9 <- (((class_1[1,1]+class_1[2,2])-max(colSums(class_1)))/((sum(class_1))-max(colSums(class_1))))*100
  names(class_9) <- "Adj. R²-Count % (Long 1997: 108)"

  # F1 score
  class_10 <- 2*((class_6*class_2)/(class_6+class_2))
  names(class_10) <- "F1 score"

  # balanced accuracy (BA) or balanced R²-Count %
  class_11 <- (class_2 + class_3)/2
  names(class_11) <- "balanced accuracy (BA) or balanced R²-Count %"

  # Matthews correlation coefficient (MCC)
  class_12 <- (as.numeric(class_1[1,1]*class_1[2,2])-as.numeric(class_1[1,2]*class_1[2,1]))/sqrt(as.numeric(class_1[1,1]+class_1[1,2])*as.numeric(class_1[1,1]+class_1[2,1])*as.numeric(class_1[2,2]+class_1[1,2])*as.numeric(class_1[2,2]+class_1[2,1]))
  names(class_12) <- "Matthews correlation coefficient (MCC)"

  # Fowlkes–Mallows index (FM)
  class_13 <- sqrt(class_6*class_2)
  names(class_13) <- "Fowlkes–Mallows index (FM)"

  # informedness or bookmaker informedness (BM)
  class_14 <- class_2 + class_3 - 100
  names(class_14) <- "informedness or bookmaker informedness (BM)"

  # markedness (MK) or deltaP
  class_15 <- class_6 + ((class_1[1,1]/(class_1[1,1]+class_1[1,2]))*100) - 100
  names(class_15) <- "markedness (MK) or deltaP"

  extat_classification <- list(class_1,class_2,class_3, class_4, class_5, class_6, class_7, class_8, class_9, class_10, class_11, class_12, class_13, class_14, class_15)
  extat_classification
}
