#' prediction
#'
#' This function is for modeling the distribution of errors 
#'
#' @param predictTable: data table that store mismatch of the library\code{inputParameter1}
#' @param model: model of your choice  (knn/rf/..)\code{inputParameter2}
#' @param enzyme: what enzyme did you used for preparing the library?  \code{inputParameter3}
#' @param seqErr: theoretical sequencin/PCR error  \code{inputParameter4}
#' @param pCutOff: false discovery rate threshold  \code{inputParameter5}
#' @param hyp: hypothesis to use  \code{inputParameter6}
#' @param dbpath: the path storing the database  \code{inputParameter7}
#' @param devMode: 1 or 0 for outputing data that with extensive information or not \code{inputParameter8} 
#'
#' @return df: return original data fram with a new column with the predicted class
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' R code here showing how your function works

tgirthamr <- function(predictTable,model,enzyme,seqErr,
                 pCutOff,resultFile,hyp, dbpath,devMode) {
    library(dplyr)
    library(stringr)
    library(readr)
    dataTable <- str_c(dbpath,'/',enzyme,'Table.tsv') %>%
        read_tsv(col_type= 'cncnnnnnnncc') %>%
        transformDF(seqErr,pCutOff,binomTest) %>%
        filterSets(hyp) %>%
        rename(label=abbrev) %>%
        group_by(label) %>% 
        do(data.frame(count = nrow(.),
                      A = .$A, 
                      C = .$C, 
                      T=.$T,
                      G=.$G,
                      ref = .$ref,
                      deletion = .$deletion)) %>%
        filter(count > 2) %>%
        select(-count) %>%
        tbl_df
    
    predictTable <- predictTable %>%
        read_tsv %>%
        transformPredict(seqErr,pCutOff,binomTest) %>%
        filterSets(hyp) 
    message('Read Data!')
    
    bases = as.character(unique(predictTable$ref))
    tablename <- resultFile
    result <- lapply(bases,fitAndPredict,dataTable,predictTable,model)  %>%
        do.call(rbind,.) 
    
    if (length(result)<1){stop("No modification sites! \n")}
    else{
        if (devMode==1){
            result %>%
                select(chrom, start, end, ref, cov, strand, A, C, T, G, deletion, label) %>%
                write.table(tablename, sep='\t',quote=F,row.names=F,col.names=F)
        }else{
            result %>%
                select(chrom, start, end, ref, cov, strand, label) %>%
                write.table(tablename, sep='\t',quote=F,row.names=F,col.names=F)
        }
    }
}