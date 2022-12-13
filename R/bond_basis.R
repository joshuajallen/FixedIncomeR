
#' Generate correlated series from a multivariate normal distribution
#'
#' @param means The means of the series.
#' @param stdevs The standard deviations of the series.
#' @param cor_mat Correlation matrix to impose.
#' @param field_desc A label for the values to be generated.
#' @param n_observations Number of data points in the time series.
#' @param column_prefix A prefix for column names in the resulting data_frame
#' @param empirical Logical. If true, means and cor_mat specify the
#'                  empirical rather than population properties.
#' @param seed Optional seed for random number generator.
#' @return A tibble with columns Date, Desc, Field and Value
#' @export
#' @examples
#' library(FIRVr)
#' generate_correlated_series(
#'   means = c(0.02672, 0.02754, 0.02758, 0.02801),
#'   stdevs = c(0.00070, 0.00070, 0.00070, 0.00070),
#'   cor_mat = matrix(
#'     c(1.00, 0.93, 0.91, 0.89,
#'       0.93, 1.00, 0.92, 0.90,
#'       0.91, 0.92, 1.00, 0.91,
#'       0.89, 0.90, 0.91, 1.00),
#'     ncol = 4
#'   ),
#'   field_descs = "Yield"
#' )
#' @importFrom magrittr "%>%"
generate_correlated_series <- function(means,
                                       stdevs,
                                       cor_mat,
                                       field_descs,
                                       n_observations = 260,
                                       start_date = Sys.Date(),
                                       empirical = TRUE,
                                       seed = NULL) {

  cov_mat <- stdevs %*% t(stdevs) * cor_mat

  dates <- seq(as.Date(start_date),
               as.Date(start_date) + n_observations*2,
               by = 1)
  dates <- dates[! (as.POSIXlt(dates)$wd %in% c(0,1))]

  if ( ! is.null(seed) ) {
    set.seed(seed)
  }

  df <- MASS::mvrnorm(n = n_observations,
                      mu = means,
                      Sigma = cov_mat,
                      empirical = empirical)

  if (is.null(rownames(cor_mat))) {
    df <- df %>% tibble::as_tibble(
      .name_repair = ~ paste0("Series_", seq_along(means))
    )
  } else {
    df <- df %>% tibble::as_tibble()
  }

  tidy_df <- df %>%
    tibble::add_column(Date = head(dates, n_observations), .before = 1) %>%
    tidyr::gather(key="Desc", value="Value", -Date)

  if ( length(field_descs) == 1 ) {
    tidy_df %>% tibble::add_column(Field = field_descs, .after = 2)
  } else {
    field_desc_map <- structure(field_descs, names = names(df))
    tidy_df %>%
      tibble::add_column(Field = field_desc_map[tidy_df$Desc] %>% as.character(),
                         .after = 2)
  }
}


#' Price a US Treasury bond given either price or yield
#'
#' @param maturity_date String or date specifying maturity date of bond.
#' @param coupon Coupon in decimal format.
#' @param effective_date String or date specifying when the bond started
#'                       accruing interest.
#' @param yield Yield in decimal format.
#' @param price Price.
#' @return A (QuantLib) FixedRateBond object
#' @export
#' @examples
#' library(FIRVr)
#' price_treasury_bond("2029-06-15", 0.025, "2009-06-15", yield = 0.021)
#' @importFrom magrittr "%>%"
price_treasury_bond <- function(maturity_date,
                                coupon,
                                effective_date,
                                yield = NA,
                                price = NA) {

  bond <- list(
    settlementDays = 1,
    # Assume issue date == effective date for our purposes
    issueDate = as.Date(effective_date),
    faceAmount = 100,
    dayCounter = "Thirty360",
    paymentConvention = "Unadjusted"
  )

  schedule <- list(
    effectiveDate = as.Date(effective_date),
    maturityDate = as.Date(maturity_date),
    period = "Semiannual",
    calendar = "UnitedStates/GovernmentBond",
    businessDayConvention = "Unadjusted",
    terminationDateConvention = "Unadjusted",
    dateGeneration = "Backward",
    endOfMonth = FALSE
  )

  calc <- list(
    dayCounter = "Actual360",
    compounding = "Compounded",
    freq = "Annual",
    durationType = "Modified"
  )

  RQuantLib::FixedRateBond(
    bond,
    coupon,
    schedule,
    calc,
    discountCurve = NULL,
    yield = yield,
    price = price
  )

}


#' Convert a tibble of N yield series into the alternative representation of
#' one yield series and N-1 spread series. This is because it is convenient
#' to think of changes in spread volatilities for bonds in the deliverable
#' basket, especially when they are close to being CTD. See e.g. Huggins
#' and Schaller.
#'
#' @param df A tibble with columns Date, Desc, Field and Value. The first
#'           occurring series will be used as the level series.
#' @return A tibble with columns Date, Desc, Field and Value. One level
#'         series and and N-1 spread series are included.
#' @export
#' @importFrom magrittr "%>%"
to_level_and_spreads <- function(df) {

  stopifnot(is.data.frame(df))

  # Use the first bond in the data frame fr the yield series
  if ( all(sort(colnames(df)) != c("Date", "Desc", "Field", "Value")) ) {
    stop("Requires a data frame with columns Date, Desc, Field and Value")
  }

  level_desc <- df$Desc[1]

  level_values <- df %>%
    dplyr::filter(Desc == level_desc)

  spread_values <- df %>%
    dplyr::filter(Desc != level_desc) %>%
    dplyr::left_join(
      level_values %>% dplyr::select(-Desc, -Field),
      by = "Date",
      suffix = c("", "_1")
    ) %>%
    dplyr::mutate(
      Value = Value - Value_1,
      Field = "Spread"
    ) %>%
    dplyr::select(-Value_1)

  levels_and_spreads <- level_values %>%
    dplyr::bind_rows(spread_values)

  levels_and_spreads

}


#' Estimate simulation model parameters from historical data. Allow for
#' volailties to be adjusted from those observed previously as e.g. yield
#' spread volatility can increase when bonds enter a CTD basket.
#'
#' @param df A tibble with columns Date, Desc, Field and Value.
#' @return A list of parameters to be used in a simulation of yield and
#'         yield spread developments (means, standard deviations and
#'         correlations), as well as field descriptions for each element.
#' @export
#' @importFrom magrittr "%>%"
extract_bond_basis_params <- function(df, volatility_adjustments = NULL) {

  stopifnot(is.data.frame(df))
  stopifnot(sort(colnames(df)) == c("Date", "Desc", "Field", "Value"))
  ordered_fields <- unique(df$Field)
  stopifnot(length(ordered_fields) == 2)
  stopifnot(ordered_fields[1] == "Yield" & ordered_fields[2] != "Yield")

  orig_order <- df$Desc %>% unique()
  field_descs <- df %>%
    # Preserve order of Descs on summarise
    mutate(Desc = factor(Desc, levels = unique(Desc))) %>%
    dplyr::group_by(Desc, Field) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    select(Field) %>%
    pull()

  df_wide <- df %>%
    select(-Field) %>%
    tidyr::spread(key = "Desc", value = "Value") %>%
    dplyr::select(-Date) %>%
    # Keep the same column order for clarity
    dplyr::select(orig_order)

  means <- df_wide %>% apply(2, mean)
  stdevs <- df_wide %>% apply(2, sd)
  correlations <- df_wide %>% cor(use = "pairwise.complete.obs")


  if ( ! is.null(volatility_adjustments) ) {
    if (length(volatility_adjustments) != length(stdevs)) {
      stop("Unexpected number of volatility adjustments. There should be one for each series.")
    } else {
      stdevs <- stdevs + volatility_adjustments
    }
  }

  list(
    "means" = means,
    "stdevs" = stdevs,
    "correlations" = correlations,
    "field_descs" = field_descs
  )

}


#' Generate a bond basis model. Simulate many possible outcomes for given
#' input parameters and calculate some derived fields based on each state.
#'
#' @param means The means of the timeseries to be generated.
#' @param stdevs The standard deviations of the timeseries to be generated.
#' @param correlations The correlation matrix of the timeseries to be generated.
#' @param n_periods The number of time periods for which to run the simulation.
#' @param n_simulations The number of simulations to perform.
#' @return A list of data frames containing the simulated yields, the model
#'         outputs, and the DO values.
#' @export
#' @importFrom magrittr "%>%"
bond_basis_model <- function(means,
                             stdevs,
                             correlations,
                             field_descs,
                             static_data,
                             forward_prices,
                             future_price,
                             evaluation_date = NULL,
                             n_periods = 63,
                             n_simulations = 1000) {

  stopifnot(is.data.frame(static_data))
  stopifnot(is.data.frame(forward_prices))
  stopifnot(is.numeric(future_price))
  stopifnot(is.numeric(n_periods))
  stopifnot(is.numeric(n_simulations))

  stopifnot(length(means) == length(stdevs))
  stopifnot(length(means) == nrow(correlations))
  stopifnot(length(means) == length(field_descs))
  stopifnot(length(means) == nrow(static_data))
  stopifnot(length(means) == nrow(forward_prices))

  #
  # Run some simulations based on the input parameters
  #

  simulated_df_list <- list()

  for (i in seq_len(n_simulations)) {
    tmp_df <- generate_correlated_series(means,
                                         stdevs,
                                         correlations,
                                         field_descs,
                                         n_observations = n_periods,
                                         start_date = Sys.Date()) %>%
      mutate(
        Simulation = i
      )
    simulated_df_list[[as.character(i)]] <- tmp_df
  }

  simulated_df <- dplyr::bind_rows(simulated_df_list)

  #
  # Sanity check. We should have one yield series and some spread series...
  #

  field_types <- simulated_df %>%
    group_by(Desc, Field) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    select(Field) %>%
    pull()

  stopifnot(sum("Yield" %in% field_types) == 1)
  stopifnot(all(field_types %>% unique() %>% sort() == c("Spread", "Yield")))

  #
  # Convert back from yield + spreads to yields
  #

  simulated_yields <- simulated_df %>%
    # Bind together the simulated yields for the first bond...
    dplyr::filter(
      Field == "Yield"
    ) %>%
    # ... and the yields calculated from those yields and the simulated spreads
    dplyr::bind_rows(
      simulated_df %>%
        dplyr::filter(
          Field != "Yield"
        ) %>%
        dplyr::mutate(Field = "Yield") %>%
        dplyr::left_join(
          simulated_df %>%
            dplyr::filter(
              Field == "Yield"
            ),
          by = c("Date", "Simulation", "Field"),
          suffix = c("", "_1")
        ) %>%
        dplyr::mutate(
          Value = Value + Value_1
        ) %>%
        dplyr::select(-Desc_1, -Value_1)
    )

  rm(simulated_df)

  #
  # Identify the terminal yields and evaluation date
  #

  terminal_yields <- simulated_yields %>%
    dplyr::filter(
      Date == max(Date)
    ) %>%
    dplyr::select(-Date) %>%
    arrange(Simulation, Desc)

  if (is.null(evaluation_date)) {
    evaluation_date <- max(simulated_yields$Date)
  }
  cat(paste0("Setting bond pricing evaluation date to ", evaluation_date, "\n"))
  RQuantLib::setEvaluationDate(as.Date(evaluation_date))

  #
  # Use the terminal yields to calculate derived fields (e.g. payoff)
  #

  terminal_yields <- terminal_yields %>%
    dplyr::left_join(static_data, by = "Desc") %>%
    dplyr::left_join(forward_prices, by = "Desc")

  static_data_errors <- terminal_yields %>%
    filter(Simulation == 1 & (is.na(MaturityDate) |
                              is.na(Coupon) |
                              is.na(EffectiveDate) |
                              is.na(ForwardPrice))) %>%
    select(-Field, -Value, -Simulation)

  if ( nrow(static_data_errors) > 0 ) {
    print(static_data_errors)
    stop("Missing bond data (see table above)")
  }

  model_output <- terminal_yields %>%
    # Calculate a dirty price using the joined static data.
    # Populate a list column so the pricing only needs to
    # be done once.
    dplyr::rowwise() %>%
    dplyr::mutate(
      PricedBond = list(
        price_treasury_bond(
          MaturityDate,
          Coupon,
          EffectiveDate,
          yield = Value
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    # Extract the necessary fields from the priced bond list
    dplyr::mutate(
      DirtyPrice = sapply(PricedBond, "[[", "dirtyPrice"),
      AccruedInterest = sapply(PricedBond, "[[", "accruedCoupon")
    ) %>%
    # Calculate a converted price and adjust onto a forward basis
    dplyr::mutate(
      ConvertedPrice = DirtyPrice / CF
    ) %>%
    dplyr::group_by(Desc) %>%
    dplyr::mutate(
      AvgConvertedPrice = mean(ConvertedPrice)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      FwdConvertedPrice = ConvertedPrice +
        ((ForwardPrice/CF) - AvgConvertedPrice)
    ) %>%
    dplyr::select(-ForwardPrice, -AvgConvertedPrice) %>%
    # Calculate the payoff in each case in order to identify CTD bond
    dplyr::mutate(
      InvoicePrice = (future_price * CF) + AccruedInterest,
      Payoff = (InvoicePrice / DirtyPrice) - 1
    ) %>%
    dplyr::group_by(Simulation) %>%
    dplyr::mutate(
      IsCTD = dplyr::if_else(Payoff == max(Payoff), TRUE, FALSE)
    ) %>%
    dplyr::ungroup()

  #
  # Return the outputs of the model
  #

  list(
    "simulated_yields" = simulated_yields,
    "model_output" = model_output,
    "DO_valuation" = model_output %>%
      dplyr::filter(IsCTD == TRUE) %>%
      select(Payoff)
  )

}

