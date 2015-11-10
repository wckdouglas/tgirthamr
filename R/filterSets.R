#' prediction
#'
#' This filters data frame wit hthe hypothesis by you choice
#'
#' @param df: dataframe that contains all the data \code{inputParameter1}
#' @param hyp: choice of hypothesis\code{inputParameter2}
#'
#' @return df: filtered data frame
#'
#' @keywords keywords
#'
#' @examples
#' R code here showing how your function works
filterSets <-function(df,hyp){
    if (hyp == 'hyp1') { df = subset(df,padj1==1)}
    else if (hyp == 'hyp2') { df = subset(df,padj2==1)}
    return(df)
}