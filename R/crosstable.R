#' A crosstable in LaTeX
#' @params data a dataframe
#' @params caption caption for the table produced by the function.
#' @params digits number of digits in the output.
#' 
#' The following parameters will be passed to xtabs and addmargins from stats
#'     package. 
#' @params formula a formula involving columns names of data. This formula will
#'     produce the crosstable and will be passed to xtabs function from stats
#'     package.
#' @params addmargins parameter for addmargins from stats package.
#' @return a latex crosstable with caption and margins
#' @export
ss_xtabs <- function(formula, data, caption = "", addmargins = NULL, digits){
    require(dplyr)
    cat("\\begin{table}[!h]\n")
    cat(paste0("\\caption{", caption,"}\n"))
    cat("\\begin{center}\n")
    data %>%
        xtabs(formula, data = .) %>%
        addmargins(addmargins) %>%
        ftable() %>%
        toLatex(digits = digits)
    cat("\\end{center}\n")
    cat("\\end{table}\n")
    
}

