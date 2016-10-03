
# load packages
library(glmnet)
library(caret)

LogisticRegression <- function(input.data, positive.class, type.measure="class", sparsity=0.98){
  
  # Perform logistic regression with lasso and return the model, coefs, performance metrics, etc.
  # Response variable MUST be in column 1
  
  ### remove sparse terms
  doc.pct <- colSums(input.data[ , -1] >= 1, na.rm = T) / nrow(input.data) # pct of docs that contain >= 1 occurrence of each word
  non.sparse.terms <- names(doc.pct[doc.pct >= (1-sparsity)])
  input.data <- input.data[ , c(1, which(colnames(input.data) %in% non.sparse.terms))]
  
  ### center and scale the columns
  input.data[, -1] <- scale(input.data[, -1], center = TRUE, scale = TRUE)
  
  ### create train/test sets
  set.seed(1)
  ndx <- sample(nrow(input.data), floor(nrow(input.data) * 0.8))
  input.data.train <- input.data[ndx,]
  input.data.test <- input.data[-ndx,]
  
  ### perform logistic regression on the training set,
  ### with lasso, minimizing misclassification error
  model.logistic.train <- cv.glmnet(as.matrix(input.data.train[ , -1]),
                                    as.matrix(input.data.train[ , 1]),
                                    family="binomial", type.measure=type.measure,
                                    alpha=1)
  
  ### predict classes for the test set
  pred.logistic <- predict(model.logistic.train,
                           newx=as.matrix(input.data.test[ , -1]),
                           s="lambda.min", type="class")
  
  ### calculate the confusion matrix
  confusion.matrix <- confusionMatrix(data=pred.logistic,
                                      reference=as.matrix(input.data.test[ , 1]),
                                      dnn=c("Prediction", "True Value"),
                                      positive = positive.class)
  
  ### perform logistic regression on the full dataset, with the min lambda
  model.logistic <- glmnet(as.matrix(input.data[ , -1]),
                           as.matrix(input.data[ , 1]),
                           family="binomial",
                           lambda = model.logistic.train$lambda.min,
                           alpha=1)
  
  ### get coefficients
  coefs <- coef(model.logistic, s="lambda.min") %>%
    as.matrix() %>%
    as.data.frame()
  names(coefs) <- "coefficient"
  coefs$word <- row.names(coefs)
  row.names(coefs) <- NULL
  coefs <- coefs %>%
    arrange(-coefficient)
  
  return(list(model.logistic=model.logistic,
              coefs=coefs,
              model.logistic.train=model.logistic.train,
              pred.logistic=pred.logistic,
              confusion.matrix=confusion.matrix))
  
}
