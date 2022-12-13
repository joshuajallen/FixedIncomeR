#
# Wrapper for Bloomberg API calls. Define a number of functions
# to simplify the requesting of common data structures.
#


#
# Flag to incidate whether Bloomberg is connected on this machine. This is
# necessary because we only want to try to connect once at start up due
# the the c. 20 second delay each time a connection is attempted on a
# non-Bloomberg machine.
#
# Three states are possible for BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE:
#   NULL  - no attempt to connect has been made yet
#   TRUE  - Bloomberg is available on this machine
#   FALSE - Bloomberg is not available on this machine
#

BLOOMBERG_INIT_CACHE <- new.env()
BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE <- NULL


#' bloomberg_init
#'
#' Create a Bloomberg connection if possible
#'
bloomberg_init <- function() {

  if ( ! "Rblpapi" %in% installed.packages() ) {
    if ( exists("flog.error") ) {
      flog.error("Rblpapi package is required for Bloomberg connectivity")
    } else {
      message("Rblpapi package is required for Bloomberg connectivity")
    }
    assign("BLOOMBERG_AVAILABLE", FALSE, envir = BLOOMBERG_INIT_CACHE)
  }

  tryCatch({
    Rblpapi::blpConnect()
    test_if_logged_on <- Rblpapi::bdp("DE0001102481 Govt", "CRNCY")$CRNCY
    assign("BLOOMBERG_AVAILABLE", TRUE, envir = BLOOMBERG_INIT_CACHE)
    if ( exists("flog.info") ) {
      flog.info("Connected to Bloomberg")
    } else {
      message("Connected to Bloomberg\n")
    }
  },
  error = function(e) {
    if ( exists("flog.warn") ) {
      flog.warn("Bloomberg not available/not logged on")
    } else {
      message("Bloomberg not available/not logged on\n")
    }
    assign("BLOOMBERG_AVAILABLE", FALSE, envir = BLOOMBERG_INIT_CACHE)
  })

}


#' bloomberg_disconnect
#'
#' Close a Bloomberg connection and reset the cache so that future
#' connection attempts are possible.
#'
bloomberg_disconnect <- function() {
  tryCatch({
    Rblpapi::blpDisconnect()
    assign("BLOOMBERG_AVAILABLE", NULL, envir = BLOOMBERG_INIT_CACHE)
    if ( exists("flog.info") ) {
      flog.info("Disconnected from Bloomberg")
    } else {
      message("Disconnected from Bloomberg")
    }
  },
  error = function(e) {
    if ( exists("flog.warn") ) {
      flog.warn("Could not disconnect from Bloomberg")
    } else {
      message("Could not disconnect from Bloomberg")
    }
  })

}


#' Query Bloomberg for timeseries data.
#'
#' @param securities A list of Bloomberg security IDs
#' @param fields Timeseries fields
#' @param from_date Start date
#' @param to_date End date
#' @param options A named list of options to pass to Bloomberg
#'
#' @return A data frame of tidy data with columns Security, Date, Field and Value
#' @export
#'
#' @examples
#' bloomberg_query("DE0001135176 Govt", "PX_LAST",
#'   from_date = "2019-05-08", to_date = "2019-05-10")
bloomberg_query <- function(securities,
                            fields,
                            from_date = Sys.Date() - 365,
                            to_date = Sys.Date(),
                            options = NULL) {

  if( is.null(BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE) ) {
    bloomberg_init()
  }

  if( ! BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE ) {
    return(tibble::tibble())
  }

  data_list <- Rblpapi::bdh(
    securities,
    fields,
    start.date = as.Date(from_date),
    end.date = as.Date(to_date),
    options = options
  )

  if ( is.data.frame(data_list) & length(securities) == 1 ) {
    # Single security was requested so we have a frame rather than a list
    data <- data_list %>%
      dplyr::mutate(Security = securities[1]) %>%
      tibble::as_tibble()
  } else if ( is.data.frame(data_list) & length(securities) > 1 ) {
    # Unexpected state
    stop("Unexpected result in bloomberg_query(). More than one security selected but only a single data frame returned.")
  } else {
    # List result (one frame per security), so use the list names to fill the ticker column
    data <- dplyr::bind_rows(data_list, .id = "Security") %>%
      tibble::as_tibble()
  }

  data %>%
    dplyr::rename(Date = date) %>%
    tidyr::gather(key = "Field", value = "Value", -Date, -Security)

}


#' Query Bloomberg for static data.
#'
#' @param securities A list of Bloomberg security IDs
#' @param fields Timeseries fields
#' @param overrides An optional names list of overrides to pass to Bloomberg
#' @param options An optional named list of options to pass to Bloomberg
#' @param tidy_data A boolean to specify whether we would like tidy or wide data
#'
#' @return A data frame of tidy data with columns Security, Field as many as required
#'         of ValueChr, ValueDate, ValueDouble, ValueInt and ValueLgl (logical/boolean).
#'         If tidy_data is set to FALSE, a wide data frame with one column per field.
#' @export
#'
#' @examples
#' bloomberg_query_static("DE0001135176 Govt", "ID_ISIN")
bloomberg_query_static <- function(securities,
                                   fields,
                                   options = NULL,
                                   overrides = NULL,
                                   tidy_data = TRUE) {

  if( is.null(BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE) ) {
    bloomberg_init()
  }

  if( ! BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE ) {
    return(tibble::tibble())
  }

  data_static <- Rblpapi::bdp(securities,
                              fields,
                              overrides = overrides,
                              options = options)

  Security <- rownames(data_static)
  data_static <- data_static %>% tibble::as_tibble()

  if (tidy_data) {

    col_types <- data_static %>% head(1) %>% sapply(dplyr::type_sum)
    all_frames <- list()

    if ( "chr" %in% col_types ) {
      all_frames[["chr"]] <- data_static[, col_types == "chr"] %>%
        dplyr::mutate(Security = Security) %>%
        tidyr::gather(key = "Field", value = "ValueChr", -Security)
    }

    if ( "date" %in% col_types ) {
      all_frames[["date"]] <- data_static[, col_types == "date"] %>%
        dplyr::mutate(Security = Security) %>%
        tidyr::gather(key = "Field", value = "ValueDate", -Security)
    }

    if ( "dbl" %in% col_types ) {
      all_frames[["dbl"]] <- data_static[, col_types == "dbl"] %>%
        dplyr::mutate(Security = Security) %>%
        tidyr::gather(key = "Field", value = "ValueDouble", -Security)
    }

    if ( "int" %in% col_types ) {
      all_frames[["int"]] <- data_static[, col_types == "int"] %>%
        dplyr::mutate(Security = Security) %>%
        tidyr::gather(key = "Field", value = "ValueInt", -Security)
    }

    if ( "lgl" %in% col_types ) {
      all_frames[["lgl"]] <- data_static[, col_types == "lgl"] %>%
        dplyr::mutate(Security = Security) %>%
        tidyr::gather(key = "Field", value = "ValueLgl", -Security)
    }

    result <- dplyr::bind_rows(all_frames)

  } else {

    result <- data_static %>%
      tibble::add_column(Security = Security, .before = 1)

  }

  result

}


#' Query Bloomberg for timeseries and static data.
#'
#' @param securities A list of Bloomberg security IDs
#' @param fields Timeseries fields
#' @param fields_static Static data fields
#' @param from_date Start date
#' @param to_date End date
#' @param options A named list of options to pass to Bloomberg
#'
#' @return A data frame of semi-tidy data
#' @export
#'
#' @examples
#' bloomberg_query_legacy("DE0001135176 Govt", "PX_LAST",
#'   from_date = "2019-05-08", to_date = "2019-05-10")
bloomberg_query_legacy <- function(securities,
                                   fields = NULL,
                                   fields_static = NULL,
                                   from_date = Sys.Date() - 365,
                                   to_date = Sys.Date(),
                                   options = NULL) {

  if( is.null(BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE) ) {
    bloomberg_init()
  }

  if( ! BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE ) {
    return(tibble::tibble())
  }
  # Fetch timeseries data

  data <- tibble::tibble(TICKER = "NA") # Allows joining when no timeseries data

  if ( ! is.null(fields) ) {
    data_list <- Rblpapi::bdh(
      securities,
      fields,
      start.date = as.Date(from_date),
      end.date = as.Date(to_date),
      options = NULL
    )
    if ( is.data.frame(data_list) & length(securities) == 1 ) {
      # Single security was requested so we have a frame rather than a list
      data <- data_list %>% dplyr::mutate(TICKER = securities[1])
    } else if ( is.data.frame(data_list) & length(securities) > 1 ) {
      # Unexpected state
      stop("Unexpected result in bloomberg_query(). More than one security selected but only a single data frame returned.")
    } else {
      # List result (one frame per security), so use the list names to fill the ticker column
      data <- dplyr::bind_rows(data_list, .id="TICKER")
    }
    data <- data %>%
      dplyr::rename(DATE = date)
  }

  # Fetch static data and join with timeseries data

  if ( ! is.null(fields_static) ) {

    data_static <- Rblpapi::bdp(securities, fields_static)
    data_static$TICKER <- rownames(data_static)

    data <- data %>% dplyr::full_join(data_static, by = "TICKER")

  }

  result <- tibble::as_tibble(data) %>%
    dplyr::filter(TICKER != "NA")

  result

}


#' Get a data frame of intraday prices for a list of securities
#'
#' @param securities A list of Bloomberg identifiers
#' @param field The field for which intraday data is to be retrieved
#' @param from_datetime Start datetime
#' @param to_datetime  End datetime
#' @param interval Interval of observations
#' @param options A named list of options to pass to Bloomberg
#'
#' @return A data frame of tidy data
#' @export
#'
#' @examples
#' bloomberg_query_intraday("DE0001135176 Govt", field = "open",
#'   from_datetime = Sys.time() - 60 * 60 * 24 * 60)
bloomberg_query_intraday <- function(securities,
                                     field = "open",
                                     from_datetime = Sys.time() - 60*60*24*60,
                                     to_datetime = NULL,
                                     interval = 5,
                                     timezone = "Europe/London",
                                     options = NULL) {

  if( is.null(BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE) ) {
    bloomberg_init()
  }

  if( ! BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE ) {
    return(tibble::tibble())
  }

  bars_list <- vector()

  for (security in securities) {
    raw_data <- Rblpapi::getBars(
      security,
      startTime = from_datetime,
      endTime = to_datetime,
      barInterval = interval,
      tz = timezone,
      options = NULL
    ) %>%
      dplyr::select(times, field) %>%
      dplyr::mutate(TICKER = security) %>%
      dplyr::rename(DATETIME = times)
    bars_list[security] <- list(raw_data)
  }

  result <- dplyr::bind_rows(bars_list)

  tibble::as_tibble(result)

}


#' Get a list of bond IDs from a pre-defined Bloomberg search
#'
#' @param search_name The name of a predefined Bloomberg search
#'
#' @return Data frame of Bloomberg bsrch results
#' @export
#'
#' @examples
#' bloomberg_query_search("COMDTY:Vessel")
bloomberg_query_search <- function(search_name) {

  if( is.null(BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE) ) {
    bloomberg_init()
  }

  if( ! BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE ) {
    return(tibble::tibble())
  }

  result <- Rblpapi::bsrch(search_name)

  tibble::as_tibble(result)

}


#' Get a table of data
#'
#' @param security A single security identifier
#' @param field A single field
#' @param options An optional named character vector with option values
#' @param overrides An optional named character vector with override values
#'
#' @return Data frame of results
#' @export
#'
#' @examples
#' bloomberg_query_table("RXZ9 Comdty", "FUT_DLVRBLE_BNDS_CUSIPS")
bloomberg_query_dataset <- function(security, field, options = NULL, overrides = NULL) {

  if( is.null(BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE) ) {
    bloomberg_init()
  }

  if( ! BLOOMBERG_INIT_CACHE$BLOOMBERG_AVAILABLE ) {
    return(tibble::tibble())
  }

  result <- Rblpapi::bds(
    security = security,
    field = field,
    options = options,
    overrides = overrides
  )

  tibble::as_tibble(result)

}


#' Get a curve from Bloomberg
#'
#' @param curve_id The Bloomberg code for the curve of interest
#' @param interp   A logical. If true linearly interpolated values will be returned for each day
#' @param max_maturity Maximum maturity for the underlying instruments
#'
#' @return A data frame of CurveID, Date, Value
#' @export
#'
#' @examples
#' library(FIRVr)
#' bloomberg_query_curve("YCSW0133 Index", interp = TRUE, max_maturity = Sys.Date() + 365)
bloomberg_query_curve <- function(curve_id, max_maturity = NULL, interp = FALSE) {

  points <- FIRVr::bloomberg_query_dataset(curve_id, "CURVE_MEMBERS")

  # Rates for the members of the curve
  df <- FIRVr::bloomberg_query(
    points$`Curve Members`,
    c("PX_LAST"),
    from_date = Sys.Date() - 10,
    to_date = Sys.Date()
  ) %>%
    # OIS rates will be lagged
    dplyr::group_by(
      Security
    ) %>%
    dplyr::filter(
      Date == max(Date)
    ) %>%
    dplyr::ungroup()

  # Maturities of the rates for the
  maturities <- FIRVr::bloomberg_query_static(
    points$`Curve Members`,
    c("MATURITY")
  ) %>%
    # There will be no maturity date for OIS. Assume today as the price will
    # likely be from the last working day anyway due to publication lag.
    dplyr::mutate(
      # Bug in the older version of dplyr in 2019-02-12 means this fails!
      # ValueDate = dplyr::if_else(is.na(ValueDate), Sys.Date(), ValueDate)
      # TODO: Use this work around for now...
      ValueDate = ifelse(is.na(ValueDate), Sys.Date(), ValueDate),
      ValueDate = as.Date(ValueDate, origin = "1970-01-01")
    ) %>%
    dplyr::rename(
      Maturity = ValueDate
    ) %>%
    dplyr::select(-Field)

  result <- df %>%
    dplyr::left_join(
      maturities,
      by = "Security"
    ) %>%
    dplyr::arrange(Maturity) %>%
    dplyr::mutate(Curve = curve_id) %>%
    dplyr::select(Curve, Maturity, Value)

  # Limit to shorter dates if required
  if ( ! is.null(max_maturity) ) {
    result <- result %>%
      dplyr::filter(Maturity <= as.Date(max_maturity))
  }

  # Interpolate values for all days if required
  if ( interp ) {
    result <- result %>%
      tidyr::complete(
        Curve,
        Maturity = seq.Date(min(Maturity), max(Maturity), by = "day")
      ) %>%
      dplyr::mutate(
        Value = approx(Maturity, Value, Maturity)$y
      )
  }

  result

}
