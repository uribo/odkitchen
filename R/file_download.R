#' Download specific estat files using selenium driver
#' 
#' @param driver RSelenium remoteDriver object
#' @param url Stats data information url (character)
#' @import RSelenium
#' @examples 
#' \dontrun{
#' require("RSelenium")
#' 
#' crmDv <- wdman::chrome(port = 4445L)
#' crmDv$output()
#' remDr <- remoteDriver(port = 4445L,
#' browserName = "phantomjs")
#' 
#' download_estat_excel(remDr, url = "https://www.e-stat.go.jp/stat-search/files?\n
#' page=1&layout=datalist&toukei=00500226&tstat=000001015623\n
#' &cycle=7&tclass1=000001032194&tclass2=000001100735&stat_infid=000031571891")
#' 
#' eCaps <- list(chromeOptions = list(prefs = list(
#' "profile.default_content_settings.popups" = 0L,
#' "download.prompt_for_download" = FALSE,
#' "download.default_directory" = getwd())))
#' 
#' remDr <- remoteDriver(
#' port = 4445L,
#' browserName = "phantomjs",
#' extraCapabilities = eCaps)
#' 
#' download_estat_excel(remDr, url = "https://www.e-stat.go.jp/stat-search/files?\n
#' page=1&layout=datalist&toukei=00500226&tstat=000001015623&cycle=7\n
#' &tclass1=000001032194&tclass2=000001100735&stat_infid=000031571891")
#' }
download_estat_excel <- function(driver, url) {
  
  # ruuning at locac env
  # RSelenium::rsDriver(port = 4444L)
  
  driver$open(silent = TRUE)
  
  driver$navigate(url)
  # webElem <- driver$findElement("css selector", "div.stat-search_result-list.js-items > a")
  # dataset_url <- webElem$getElementAttribute(attrName = "href")[[1]]
  
  # driver$navigate(dataset_url)
  webElem <- driver$findElement("css selector", "div.stat-dl_icon.stat-xls_icon.js-dl")
  childElems <- webElem$findChildElements("css selector", "span")

  # # TODO: e-stat domain and parameter ?
  # driver$navigate(url)
  # 
  # Sys.sleep(2)
  # webElem <- driver$findElement("css selector", "div.stat-search_result-files")
  # childElems <- webElem$findChildElements("css selector", "div.stat-dl_icon.stat-xls_icon.js-dl > span")
  # 
  # # testthat::expect_equal(
  # #   childElems[[1]]$getElementText()[[1]],
  # #   "EXCEL"
  # # )
  
  Sys.sleep(2)
  childElems[[1]]$clickElement()
  Sys.sleep(10)
  driver$close()
}
