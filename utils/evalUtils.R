# return observed VS expected results tibble ----------------
evalClassif <- function(model, cvSet, feats) {
  
  # compute predictions for muticlass classification and return
  tibble(predictions = as.vector(predict(model, as.matrix(cvSet[feats]), type = "class")),
         truth = cvSet$Class)
}