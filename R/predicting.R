#' prediction
#'
#' This function is for modeling the distribution of errors 
#'
#' @param base: reference base at the alignming position\code{inputParameter1}
#' @param df: dataframe that contains all the data \code{inputParameter2}
#' @param model: the fitted model to be used \code{inputParameter2}
#'
#' @return df: return original data fram with a new column with the predicted class
#'
#' @keywords keywords
#'
#' @examples
#' R code here showing how your function works
predicting <- function(base,model,df){
    #subsetting data
    message ('start prediction')
    columns <- c('A','C','T','G','deletion')
    columns <- columns[!grepl(base,columns)]
    df <- subset(df,ref==base)
    df$label <- predict(model$finalModel,df[,columns],type = 'class')
    return (df)
}