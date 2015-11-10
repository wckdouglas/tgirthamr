#' modeling
#'
#' This function is for modeling the distribution of errors 
#'
#' @param base: reference base at the alignming position\code{inputParameter1}
#' @param df: dataframe that contains all the data \code{inputParameter2}
#' @param model: the model to be used \code{inputParameter2}
#'
#' @return y1: return fitted model or the only training class
#'
#' @keywords keywords
#'
#'
#' @examples
#' R code here showing how your function works

modeling <- function(base,df, model){
    suppressMessages(library(caret))
    fitControl <- trainControl(method = 'LOOCV',allowParallel = T)
    message('start modeling..')
    columns <- c('A','C','T','G','deletion','label')
    columns <- columns[!grepl(base, columns)]
    df <- df[df$ref==base,columns]
    #split data
    trainMat <- select(df, -label)
    trainClass <- factor(df$label)
    if (length(unique(trainClass)) > 1){
        message ('Start training ',model,' ',base)
        modelFit <- train(y = trainClass, x = trainMat, method = model, trControl = fitControl)
        message('Trained ',model, ' for ',base)
        return(modelFit)
    }else{
        message ('Skipped ',model,' ',base)
        return(unique(trainClass))
    }
}
