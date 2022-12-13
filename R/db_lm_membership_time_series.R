#' Get a time series of list members
#'
#' @param list_names character vector of list names
#' @param from_date of class 'Date' (in the form YYYY-MM-DD)
#' @param to_date of class 'Date' (in the form YYYY-MM-DD)
#'
#' @return A data frame with columns: {ListName, AsAtDate, <any columns from the list management database that uniquely identify the member>}
#' @export
#'
#' @examples db_lm_membership_time_series(c("list1","list2"),from_date = Sys.Date()-10, to_date = Sys.Date())
db_lm_membership_time_series <- function(list_names,
                                         from_date = NULL,
                                         to_date = Sys.Date()) {
  # handle bad arguments
  is.Date <- function(x) {
    inherits(x, "Date")
  }
  if (is.null(list_names)) {
    stop("Must include at least one issuer to get. Set 'list_names = <character vector of your list of curves>'. Run FIRVrBOE::curve_codes() for options (hint: Germany is 'EUR_Govt_DE', US is 'USD_Govt_US'")
  }
  if (!is.null(from_date)) {
    if(is.Date(from_date) & is.Date(to_date)) {
      if (from_date > to_date) {
        stop("'from_date' cannot be after 'to_date'")
      }
    } else {
      stop("'from_date' and 'to_date' must be of class 'Date'")
    }

  }

  # Connect to the list management database
  database_filename <- rm_path("_Data/ListManagement/list_membership.db")
  conn <- db_lm_connect(database_filename)
  # Get the minimum valid date for a membership group to be complete and compute the date to construct the membership from, given the minimum membership date
  min_dates <- tbl(conn,sql("select List.ListName, min(Membership.Updated) from Membership
                                                           left Join List on List.ID_ListName = Membership.ID_ListName
                                                           group by List.ListName")) %>%
                     filter(ListName %in% list_names) %>%
                     collect() %>%
                     rename(MinDate = `min(Membership.Updated)`) %>%
                     mutate(MinDate = as.Date(str_extract(MinDate,"\\d{4}-\\d{2}-\\d{2}"))) %>%
                     mutate(FromDate = max(MinDate,from_date)) %>%
                     mutate(MinDate = NULL)
  # Get the list members
  conn <- db_RM_lm_connect()
  members <- db_lm_get_membership(conn,list_names) %>%
               unique()
  db_lm_disconnect(conn)
  members <- left_join(min_dates,members,by="ListName")

  # get list of weekdays between the two dates
  df <- do.call(rbind,lapply(min_dates$ListName,function(lst) {
    tmp <- min_dates %>% filter(ListName == lst)
    dates <- seq(tmp$FromDate,as.Date(to_date),by = 1)
    dates <- dates[!weekdays(dates,F) %in% c('Saturday','Sunday')]
    do.call(rbind,lapply(dates,function(dte){
      members %>%
        filter(ListName == lst,dte > StartDate & dte < EndDate) %>%
        select(-StartDate,-EndDate,-FromDate) %>%
        mutate(AsAtDate = dte)
    }))
  }))
  db_cols <- colnames(df)[colnames(df) != "ListName" & colnames(df) != "AsAtDate"]
  df <- df[,c("ListName","AsAtDate",db_cols)]
  return(df)
}


