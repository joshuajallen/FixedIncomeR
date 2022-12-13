#
# Functions for managing a simple on disk database of list memberships.
#

#' Connect to a list membership database.
#'
#' The database can be created using db_lm_create_list_membership_database()
#'
#' @param filename The path to the SQLite database file
#' @export
#' @return Database connection reference.
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_connect <- function(filename) {
  DBI::dbConnect(RSQLite::SQLite(), filename)
}

#' Disconnect from a list membership database.
#'
#' @param conn A database connection.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_disconnect <- function(conn) {
  DBI::dbDisconnect(conn)
}

#' Add a new membership list to the database.
#'
#' @param conn A database connection.
#' @param list_name The unique name of the list to be created.
#' @param list_desc A description of the list.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_create_list <- function(conn, list_name, list_desc) {

  tryCatch({

    rs <- DBI::dbSendStatement(
      conn,
      "insert into List (ListName, ListDesc, Updated, UpdatedBy)
      values (?, ?, ?, ?)"
    )

    params <- list(
      list_name, list_desc, as.character(Sys.time()), Sys.getenv("USERNAME")
    )

    DBI::dbBind(rs, param = params)
    DBI::dbClearResult(rs)

    "Created list"

  },
  error = function(e) {
    msg <- paste("ERROR:",
                 e,
                 DBI::dbGetStatement(rs),
                 paste(params, collapse = ", "))
    DBI::dbClearResult(rs)
    msg
  })

}


#' Modify a membership list definition
#'
#' @param conn A database connection.
#' @param list_name The unique name of the list to be modified
#' @param list_desc A description of the list.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_update_list <- function(conn, list_name, list_desc) {

  tryCatch({

    rs <- DBI::dbSendStatement(
      conn,
      "update List set ListDesc = ?, Updated = ?, UpdatedBy = ? where ListName = ?"
    )

    params <- list(
      list_desc, as.character(Sys.time()), Sys.getenv("USERNAME"), list_name
    )

    DBI::dbBind(rs, param = params)
    DBI::dbClearResult(rs)

    "Updated list"

  },
  error = function(e) {
    msg <- paste("ERROR:",
                 e,
                 DBI::dbGetStatement(rs),
                 paste(params, collapse = ", "))
    DBI::dbClearResult(rs)
    msg
  })

}


#' Add a new security to the database.
#'
#' @param conn A database connection.
#' @param isin The unique identifier for the security.
#' @param security_des A description for the security e.g. Ticker Coupon Maturity
#' @param maturity_date The maturity date for the security.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_create_security <- function(conn, isin, security_des, maturity_date) {

  tryCatch({

    maturity_date <- as.character(as.Date(maturity_date))

    rs <- DBI::dbSendStatement(
      conn,
      "insert into Security (ISIN, SecurityDes, MaturityDate, Updated, UpdatedBy)
      values (?, ?, ?, ?, ?)"
    )

    params <- list(
      isin, security_des, maturity_date,
      as.character(Sys.time()), Sys.getenv("USERNAME")
    )

    DBI::dbBind(rs, param = params)
    DBI::dbClearResult(rs)

    "Created security"

  },
  error = function(e) {
    msg <- paste("ERROR:",
                 e,
                 DBI::dbGetStatement(rs),
                 paste(params, collapse = ", "))
    DBI::dbClearResult(rs)
    msg
  })

}


#' Update an existing entry in the Security table.
#'
#' @param conn A database connection.
#' @param isin The unique identifier for the security.
#' @param security_des A description for the security e.g. Ticker Coupon Maturity
#' @param maturity_date The maturity date for the security.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_update_security <- function(conn, isin, security_des, maturity_date) {

  tryCatch({

    maturity_date <- as.character(as.Date(maturity_date))

    rs <- DBI::dbSendStatement(
      conn,
      "update Security set
      SecurityDes = ?, MaturityDate = ?, Updated = ?, UpdatedBy = ?
      where ISIN = ?"
    )

    params <- list(
      security_des, maturity_date, as.character(Sys.time()), Sys.getenv("USERNAME"), isin
    )

    DBI::dbBind(rs, param = params)
    DBI::dbClearResult(rs)

    "Updated security"

  },
  error = function(e) {
    msg <- paste("ERROR:",
                 e,
                 DBI::dbGetStatement(rs),
                 paste(params, collapse = ", "))
    DBI::dbClearResult(rs)
    msg
  })

}


#' Create a database entry to represent a list membership.
#'
#' Adds a link between a security and a list for a specified range of dates.
#' List name, ISIN and start date form a primary key so that only
#' one entry may exist for each combination of these fields.
#'
#' @param conn A database connection.
#' @param list_name The name of the list to be modified.
#' @param isin The unique security identifier.
#' @param start_date The start date of the membership period.
#' @param end_date End date for the list membership period.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_create_membership <- function(conn, list_name, isin, start_date, end_date) {

  start_date <- as.character(as.Date(start_date))
  end_date = as.character(as.Date(end_date))

  # Get list name ID

  rs <- DBI::dbSendQuery(conn, "select ID_ListName from List where ListName = ?")
  DBI::dbBind(rs, param = list_name)
  id_list_name <- DBI::dbFetch(rs)$ID_ListName
  DBI::dbClearResult(rs)
  if ( length(id_list_name) != 1 ) {
    return(paste0("ERROR: No entry for '", list_name, "' in List table."))
  }

  # Get security ID

  rs <- DBI::dbSendQuery(
    conn,
    "select ID_ISIN from Security where ISIN = ?"
  )
  DBI::dbBind(rs, param = isin)
  id_isin <- DBI::dbFetch(rs)$ID_ISIN
  DBI::dbClearResult(rs)
  if ( length(id_isin) != 1 ) {
    return(paste0("ERROR: No entry for '", isin, "' in Security table."))
  }

  # Insert the membership record (depends on list and
  # security being found in the above queries)

  tryCatch({

    rs <- DBI::dbSendStatement(
      conn,
      "insert into Membership
      (ID_ListName, ID_ISIN, StartDate, EndDate, Updated, UpdatedBy)
      values (?, ?, ?, ?, ?, ?)"
    )

    params <- list(
      id_list_name, id_isin, start_date, end_date,
      as.character(Sys.time()), Sys.getenv("USERNAME")
    )

    DBI::dbBind(rs, param = params)
    DBI::dbClearResult(rs)

    "Created membership"

  },
  error = function(e) {
    msg <- paste("ERROR:",
                 e,
                 DBI::dbGetStatement(rs),
                 paste(params, collapse = ", "))
    DBI::dbClearResult(rs)
    msg
  })

}


#' Update an existing list membership.
#'
#' @param conn A database connection.
#' @param list_name The name of the list to be modified.
#' @param isin The unique security identifier.
#' @param start_date The start date of the membership period.
#' @param end_date End date for the list membership period.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_update_membership <- function(conn, list_name, isin, start_date, end_date) {

  start_date <- as.character(as.Date(start_date))
  end_date = as.character(as.Date(end_date))

  # Get list name ID

  rs <- DBI::dbSendQuery(conn, "select ID_ListName from List where ListName = ?")
  DBI::dbBind(rs, param = list_name)
  id_list_name <- DBI::dbFetch(rs)$ID_ListName
  DBI::dbClearResult(rs)
  if ( length(id_list_name) != 1 ) {
    return(paste0("ERROR: No entry for '", list_name, "' in List table."))
  }

  # Get security ID

  rs <- DBI::dbSendQuery(
    conn,
    "select ID_ISIN from Security where ISIN = ?"
  )
  DBI::dbBind(rs, param = isin)
  id_isin <- DBI::dbFetch(rs)$ID_ISIN
  DBI::dbClearResult(rs)
  if ( length(id_isin) != 1 ) {
    return(paste0("ERROR: No entry for '", isin, "' in Security table."))
  }

  # Insert the membership record (depends on list and
  # security being found in the above queries)

  tryCatch({

    rs <- DBI::dbSendStatement(
      conn,
      "update Membership set EndDate = ?, Updated = ?, UpdatedBy = ?
      where ID_ListName = ? and ID_ISIN = ? and StartDate = ?"
    )

    params <- list(
      end_date, as.character(Sys.time()), Sys.getenv("USERNAME"),
      id_list_name, id_isin, start_date
    )

    DBI::dbBind(rs, param = params)
    DBI::dbClearResult(rs)

    "Updated membership"

  },
  error = function(e) {
    msg <- paste("ERROR:",
                 e,
                 DBI::dbGetStatement(rs),
                 paste(params, collapse = ", "))
    DBI::dbClearResult(rs)
    msg
  })

}


#' Get the identifiers of all membership lists in the database.
#'
#' @param conn A connection to a list membership database.
#' @return A data frame with columns ListName and ListDesc.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_get_lists <- function(conn) {

  DBI::dbGetQuery(
    conn,
    "select ListName, ListDesc from List order by ListName"
  ) %>% tibble::as_tibble()

}


#' Get all securities in the database.
#'
#' @param conn A connection to a list membership database.
#' @return A data frame with columns ISIN, SecurityDes and MaturityDate.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_get_securities <- function(conn) {

  df <- DBI::dbGetQuery(
    conn,
    "select ISIN, SecurityDes, MaturityDate from Security order by SecurityDes"
  ) %>% tibble::as_tibble()

  if ( nrow(df) > 0 ) {
    df <- df %>%
      dplyr::mutate(
        MaturityDate = as.Date(MaturityDate)
      )
  }

  df

}


#' Get a list of securities that belong to a list for a specific date.
#'
#' @param conn A database connection.
#' @param list_name The name of the list.
#' @param date The date for which membership details are requested.
#' @return A data frame of results.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_get_membership <- function(conn, list_name, date = NULL) {

  if ( is.null(date) ) {
    date_clause <- ""
    params = list(list_name)
  } else {
    date_clause <- "and StartDate <= ? and (EndDate >= ? or EndDate is NULL)"
    date <- as.character(as.Date(date))
    params = list(list_name, date, date)
  }

  df <- DBI::dbGetQuery(
    conn,
    paste("select
          ListName, Security.ISIN, SecurityDes, MaturityDate, StartDate, EndDate
          from Membership, Security, List
          where ListName = ? and
          Membership.ID_ListName = List.ID_ListName and
          Membership.ID_ISIN = Security.ID_ISIN ",
          date_clause,
          "order by MaturityDate"),
    params = params
  ) %>% tibble::as_tibble()

  # Convert date types as dates come back from DB as strings
  if ( nrow(df) > 0 ) {
    df <- df %>%
      dplyr::mutate(
        MaturityDate = as.Date(MaturityDate),
        StartDate = as.Date(StartDate),
        EndDate = as.Date(as.character(EndDate))
      )
  }

  df

}


#' Create an instance of a list membership database from scratch.
#'
#' Creation can only proceed it the database (file) does not already exist.
#' @param filename The path the the file which will contain the new SQLite database.
#' @export
#' @examples See FIRVr/examples/ex_db_list_membership.R
db_lm_create_list_membership_database <- function(filename) {

  if (file.exists(filename)) {
    stop(paste0("Database ", filename, " already exists"))
  }

  db <- DBI::dbConnect(RSQLite::SQLite(), filename)

  DBI::dbExecute(
    db,
    "create table Security (
    ID_ISIN integer primary key autoincrement,
    ISIN text not null,
    SecurityDes text not null,
    MaturityDate date,
    Updated datetime not null,
    UpdatedBy text not null)"
  )
  DBI::dbExecute(db, "create unique index idx_security_isin on Security (ISIN)")

  DBI::dbExecute(
    db,
    "create table List (
    ID_ListName integer primary key autoincrement,
    ListName text not null,
    ListDesc text not null,
    Updated datetime not null,
    UpdatedBy text not null)"
  )
  DBI::dbExecute(db, "create unique index idx_list_listname on List (ListName)")

  DBI::dbExecute(
    db,
    "create table Membership (
    ID_ListName integer not null,
    ID_ISIN integer not null,
    StartDate date not null,
    EndDate date not null,
    Updated datetime not null,
    UpdatedBy text not null,
    foreign key(ID_ListName) references List(ID_ListName),
    foreign key(ID_ISIN) references Security(ID_ISIN))"
  )
  DBI::dbExecute(
    db,
    "create unique index idx_membership_comp_pk
    on Membership (ID_ListName, ID_ISIN, StartDate)"
  )

  DBI::dbDisconnect(db)

}


#
# Function to get benchmark bonds based on our list database
#

#' Get 'benchmark' bonds for ticker/tenor combinations.
#'
#' @param conn Connection to the list management database
#' @param ticker_groups A list of arrays of tickers for which benchmarks
#'        should be returned
#' @param list_names Consider only securities in these lists
#' @param benchmark_years Maturities for which to get benchmarks
#' @param min_history_days Minimum history for benchmarks in days
#' @param benchmark_window_days Size of window around target maturity
#' @param target_date Date to which benchmark maturities are calculated
#'
#' @return Tibble of benchmark data with columns TickerGroup,
#'         Bmk (tenor in years), SecurityDes, ISIN, MaturityDate
#' @export
#'
#' @examples
#' db_lm_get_benchmarks(
#'   conn,
#'   ticker_groups = list(c("BKO", "OBL", "DBR"),
#'                        c("FRTR")),
#'   list_names = c("EUR_Govt_DE", "EUR_Govt_FR"),
#'   benchmark_years = c(2, 5, 10),
#'   min_history_days = 90,
#'   benchmark_window_days = 365,
#'   target_date = Sys.Date(),
#'   return_all_candidates = FALSE
#' )
#'
db_lm_get_benchmarks <- function(
  conn,
  ticker_groups = list(c("BKO", "OBL", "DBR"),
                       c("FRTR")),
  list_names = NULL,
  currency = "EUR",
  benchmark_years = c(2, 5, 10),
  min_history_days = 90,
  benchmark_window_days = 365,
  target_date = Sys.Date(),
  return_all_candidates = FALSE) {

  # First, get a list of all bonds that might match our requirements
  benchmark_centres = target_date + (benchmark_years * 365.25)
  benchmark_mty_min = benchmark_centres - (benchmark_window_days/2)
  benchmark_mty_max = benchmark_centres + (benchmark_window_days/2)

  params = list(as.character(min(benchmark_mty_min)),
                as.character(max(benchmark_mty_max)))

  raw_df <- DBI::dbGetQuery(
    conn,
    paste("select
           ISIN, SecurityDes, MaturityDate, StartDate, ListName
           from Security, Membership, List
           where
           Membership.ID_ISIN = Security.ID_ISIN and
           Membership.ID_ListName = List.ID_ListName and
           MaturityDate >= ? and
           MaturityDate <= ?"),
    params = params
  ) %>%
    tibble::as_tibble() %>%
  # Keep only earliest membership as proof of issuance date
    dplyr::group_by(ISIN) %>%
    dplyr::filter(
      StartDate == min(StartDate),
      stringr::str_extract(ListName, "^[A-Z]{3}") == currency
    ) %>%
    dplyr::ungroup()

  # Apply list name filter if necessary. This helps us to
  # avoid e.g. inflation linked bonds that may share a ticker
  if ( ! is.null(list_names) ) {
    raw_df <- raw_df %>%
      filter(
        ListName %in% list_names
      )
  }

  # Build a tibble of ticker groups (tickers can be in
  # one group only, preferring the first occurrance)
  tickers_group_df <- list()
  for ( i in seq_len(length(ticker_groups)) ) {
    tickers_group_df[[i]] <- tibble::tibble(TickerGroup = i,
                                            Ticker = ticker_groups[[i]])
  }
  ticker_df <- dplyr::bind_rows(tickers_group_df) %>%
    dplyr::group_by(Ticker) %>%
    dplyr::filter(TickerGroup == min(TickerGroup))


  # Tibble of benchmark date ranges
  benchmark_ranges_df <- tibble::tibble(
    Bmk = benchmark_years,
    BmkStart = benchmark_mty_min,
    BmkEnd = benchmark_mty_max
  )

  # Now apply filters to select the benchmarks
  df <- raw_df %>%
    dplyr::mutate(
      Ticker = stringr::str_extract(SecurityDes, "^[A-Z]{1,7}"),
      DaysSinceIssued = difftime(as.Date(target_date),
                                 as.Date(StartDate),
                                 units = "days")
    ) %>%
    dplyr::left_join(ticker_df, by = "Ticker") %>%
    dplyr::filter(
      ! is.na(TickerGroup),
      ISIN != "",
      DaysSinceIssued >= min_history_days
    ) %>%
    dplyr::group_by(TickerGroup) %>%
    # Now add benchmark groups
    tidyr::crossing(benchmark_ranges_df) %>%
    dplyr::filter(
      MaturityDate >= BmkStart,
      MaturityDate <= BmkEnd
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(TickerGroup, Bmk, MaturityDate)

    fields <- c("TickerGroup",
                "Bmk",
                "SecurityDes",
                "ISIN",
                "MaturityDate")

    if ( return_all_candidates ) {
      return(df %>% select(fields))
    } else {
      df %>%
        dplyr::group_by(TickerGroup, Bmk) %>%
        dplyr::arrange(DaysSinceIssued) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(fields)
    }

}
