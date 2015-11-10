#'  fit and prediction
#'
#' This function is for modeling the distribution of errors 
#'
#' @param base: reference base at the alignming position\code{inputParameter1}
#' @param datatable: training dataset contains all the data \code{inputParameter2}
#' @param predictTable: new dataset to be predicted \code{inputParameter2}
#' @param model: the fitted model to be used \code{inputParameter2}
#'
#' @return df: return original data fram with a new column with the predicted class
#'
#' @keywords keywords
#'
#'
#' @examples
#' R code here showing how your function works

fitAndPredict <- function(base,dataTable,predictTable,model){
    modelFit <- modeling(base,dataTable, model)
    message('Tuned model')
    if(!is.character(modelFit)){
        return(predicting(base,modelFit,predictTable))
    }else{
        return (mutate(df,label = modelFit))
    }
}