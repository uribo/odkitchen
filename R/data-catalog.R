#' e-stat statistics catalog data
#' 
#' @section Dimensions: 
#' 
#' \itemize{
#'   \item `title`: statistic survey title.
#'   \item `start_date`, `end_date` data recored periods. 
#'   \item `information`: data summary. nested data.frame
#' }
#' 
#' @docType data
#' @name catalog
#' @usage catalog
#' @format A [tbl_df] with 1604 observations, unnested 1642 observations.
#' @examples 
#' catalog
#' 
#' tidyr::unnest(catalog)
NULL
