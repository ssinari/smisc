#' A crosstable in LaTeX
#' @import magrittr
#' @param data a dataframe
#' @param caption caption for the table produced by the function.
#' @param digits number of digits in the output.
#' 
#' The following parameters will be passed to xtabs and addmargins from stats
#'     package. 
#' @param formula a formula involving columns names of data. This formula will
#'     produce the crosstable and will be passed to xtabs function from stats
#'     package.
#' @param addmargins parameter for addmargins from stats package.
#' @return a latex crosstable with caption and margins
#' @export
ss_xtabs <- function(formula, data, caption = "", addmargins = NULL, digits){
    requireNamespace(magrittr)
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

