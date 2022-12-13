#' Create a list of supply events for a set of fixed income instruments.
#'
#' This method can be used to get all of the open dates and sizes for 
#' of spreads around historical bond auction dates. It also estimates the total PV01 / DV01 
#' of the supply. 
#'
#' @param ISINs A vector of 12 length character ISINs 
#' @return A data frame of supply events, with columns ISIN, issuer, open_date, maturity, notional, and est_PV01
#' @export
#' @examples
#' library(FIRVr)
#' supply_table(c("DE0001104800", "FR0012938116"))
#' supply_table(c("AT0000A1XML2"))
#' @importFrom magrittr "%>%"
supply_table <- function(ISINs){
  
  supply <- tibble::tibble()
  
  ISIN_list <- sapply(ISINs, function(x){paste0(x, " Govt")})
  
  for(i in 1:length(ISIN_list)){
    
    reopen_data <- FIRVr::bloomberg_query_dataset(security = ISIN_list[i],
                                                  field = "HISTORY_OF_REOP_TAPS_OF_THE_BD") 
    
    issue_data <- FIRVr::bloomberg_query_static(securities = ISIN_list[i],
                                                fields = c("ISSUE_DT",
                                                           "AMT_ISSUED", 
                                                           "ISSUE_PX",
                                                           "ISSUER",
                                                           "MATURITY"), 
                                                tidy_data = FALSE) %>%
      dplyr::rename(open_date = ISSUE_DT, 
                    notional = AMT_ISSUED,
                    price = ISSUE_PX,
                    issuer = ISSUER,
                    maturity = MATURITY) %>%
      dplyr::select(-Security)
    
    if(is.na(issue_data$price) | issue_data$price == 0){issue_data$price <- FIRVr::bloomberg_query(securities = ISIN_list[i], 
                                                                                                   fields = "PX_MID", 
                                                                                                   from_date = issue_data$open_date + 3, 
                                                                                                   to_date = issue_data$open_date + 5
                                                                                                  )$Value[[1]]
    } 
    
    if(nrow(reopen_data > 0)){ 
      
      reopen_data <- reopen_data %>%
        dplyr::rename(open_date = `Effective (Selling) Date`, 
                      notional = `Increase Amount`,
                      price = `Increase Price`) %>%
        dplyr::select(open_date, notional, price) %>%
        dplyr::mutate(issuer = issue_data$issuer,
                      maturity = issue_data$maturity)
      
      issue_data$notional <- issue_data$notional - (reopen_data %>% 
                                                      dplyr::summarise(notional = sum(notional)) %>% 
                                                      dplyr::pull(notional))
      
      if(min(reopen_data$price) <= 0){ 
        
        reopen_data <- reopen_data %>%
          dplyr::select(-price) %>%
          dplyr::left_join(FIRVr::bloomberg_query(securities = ISIN_list[i], 
                               fields = "PX_MID", 
                               from_date = min(reopen_data$open_date), 
                               to_date = max(reopen_data$open_date)) %>%
                             dplyr::select(Date, Value) %>%
                             dplyr::rename(open_date = Date,
                                            price = Value),
                           by = "open_date")
      
      }
    }
    
    issue_data <- reopen_data %>%
      rbind(issue_data) %>%
      dplyr::mutate(ISIN = ISINs[i]) %>%
      dplyr::mutate(est_PV01 = as.numeric((maturity - open_date) / 365) * notional * price * 0.000001) %>%
      dplyr::select(ISIN, issuer, open_date, maturity, notional, est_PV01)
    
    supply <- supply %>%
      rbind(issue_data)
    
  }
  
  supply %>%
    dplyr::arrange(open_date)

}
