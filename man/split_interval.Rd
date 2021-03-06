% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_interval.R
\name{split_interval}
\alias{split_interval}
\title{Split date as character to starts and ends.}
\usage{
split_interval(label, ...)
}
\arguments{
\item{label}{target date character.}
}
\value{
tibble. Contains input dates sllit to starts and end days.
}
\description{
Split date as character to starts and ends.
}
\examples{
\dontrun{
split_interval("平成29年3月") # Determinate last day of month
split_interval("平成27年4月-5月")
split_interval("平成27年4~5月")
split_interval("平成27年4月から5月")
split_interval("平成27年4月より5月")
split_interval("平成1年11月から平成1年12月")
split_interval("平成20年~平成24年")
split_interval("国家公務員退職手当実態調査(平成28年度)")
split_interval("平成26年度")
split_interval("(平成28年1月~3月)")
split_interval("H20.12.31現在")
split_interval("平成17年(2005年)")
split_interval("平成7年")

require(magrittr); require(lubridate)
tibble::data_frame(
label = c("平成27年4月~5月",
         "平成29年3月",
         "平成1年11月から12月",
         "平成1年11月から平成1年12月")
         ) \%>\% 
         dplyr::mutate(out = purrr::pmap(., split_interval)) \%>\% 
         tidyr::unnest() \%>\% 
         dplyr::filter(start_date \%within\% lubridate::interval(ymd("2017-01-01"), 
                                           lubridate::ymd("2018-01-01")))
}
}
