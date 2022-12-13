

#' Get OIS curves for one or many of USD, EUR, GBP, CAD, JPY
#'
#' @param currencies A vector of currencies that may include USD, EUR, GBP, CAD, JPY
#' @param interp A logical flagging whether to interpolate curves
#' @param max_maturity Maximum maturity date of underlying points
#'
#' @return Data frame of results
#' @export
#'
#' @examples
#' library(FIRVr)
#' get_ois_curves(c("EUR", "USD"))
get_ois_curves <- function(currencies, interp = FALSE, max_maturity = NULL) {

  currency_config <- tibble::tibble(
    Currency = c("EUR", "USD", "GBP", "CAD", "JPY"),
    Curve = c("YCSW0133 Index",
              "YCSW0042 Index",
              "YCSW0141 Index",
              "YCSW0147 Index",
              "YCSW0195 Index"))

  curve_list <- list()

  for (ccy in currencies) {
    if (ccy %in% currency_config$Currency) {
      curve_list[[ccy]] <- FIRVr:::bloomberg_query_curve(
        currency_config %>% dplyr::filter(Currency == ccy) %>% dplyr::pull(Curve),
        interp = interp,
        max_maturity = max_maturity
      ) %>%
        dplyr::mutate(
          Currency = ccy,
          Value = Value / 100
        )
    }
  }

  dplyr::bind_rows(curve_list)

}


#' Build a table of CTDs for different contracts
#'
#' @param contracts A list of Bloomberg identifiers for the contracts to include
#' @param verbose Return additional columns
#'
#' @return Data frame of results
#' @export
#'
#' @examples
#' library(FIRVr)
#' build_ctd_table(c("DU", "RX"), verbose = FALSE)
build_ctd_table <- function(contracts, verbose = FALSE) {

  if ( length(contracts) < 1 ) {
    return(tibble::tibble())
  }

  # Get the necessary futures information from Bloomberg
  bbg_data <- FIRVr::get_ctd_dataset(contracts)

  if ( nrow(bbg_data) < 1 ) {
    return(tibble::tibble())
  }

  # Get OIS swap curves from Bloomberg
  ois_data <- FIRVr::get_ois_curves(
    unique(bbg_data$Currency),
    interp = TRUE,
    max_maturity = Sys.Date() + 365
  )

  # Use the raw data to calculate our derived fields
  df <- bbg_data %>%
    dplyr::mutate(
      FutureSettleDate = dplyr::if_else(
        ActualRepo < Coupon,
        LastDelivery,
        FirstDelivery
      ),
      DaysToDelivery = difftime(
        as.Date(FutureSettleDate),
        as.Date(SettleDate),
        units = "days"
      ) %>%
        as.integer(),
      Accrued = dplyr::if_else(
        Coupon == 0,
        0,
        FIRVr:::calculate_accrued_interest(
          settle = SettleDate,
          mature = Maturity,
          coupon = Coupon,
          freq = CouponFrequency,
          convention = DayCountConv
        )
      ),
      FwdAccrued = dplyr::if_else(
        Coupon == 0,
        0,
        FIRVr:::calculate_accrued_interest(
          settle = FutureSettleDate,
          mature = Maturity,
          coupon = Coupon,
          freq = CouponFrequency,
          convention = DayCountConv
        )
      ),
      FwdCleanPrice = FuturePrice * ConversionFactor,
      FwdDirtyPrice = FwdAccrued + FwdCleanPrice,
      ModDuration = FIRVr:::calculate_duration(
        settle = SettleDate,
        mature = Maturity,
        coupon = Coupon,
        freq = CouponFrequency,
        yield = dplyr::if_else(Yield < 0, 0, Yield),
        convention = DayCountConv,
        modified = TRUE,
        comp.freq = CouponFrequency
      ),
      FwdModDuration = FIRVr:::calculate_duration(
        settle = FutureSettleDate,
        mature = Maturity,
        coupon = Coupon,
        freq = CouponFrequency,
        yield = dplyr::if_else(Yield < 0, 0, Yield),
        convention = DayCountConv,
        modified = TRUE,
        comp.freq = CouponFrequency
      ),
      DeliverableCashValueOneContract = (ContractSize / ConversionFactor) *
                                          (FwdDirtyPrice/100),
      FuturePV01 = (DeliverableCashValueOneContract / 10000) * FwdModDuration,
      FwdBondPV01 = ((ContractSize * FwdDirtyPrice/100) / 10000) * FwdModDuration,
      GrossBasis = BondPrice - FuturePrice * ConversionFactor,
      # Note that we could need further corrections here if we expect
      # to go further out than the second future contract.
      # Note: this is an approximation due to no calendar adjustment.
      NextNextCoupon = PreviousCoupon + 2 * (NextCoupon - PreviousCoupon),
      CouponCorrection = dplyr::case_when(
        NextCoupon >= SettleDate &
          NextCoupon < FutureSettleDate &
          NextNextCoupon >= SettleDate &
          NextNextCoupon < FutureSettleDate ~ 2 * 100 * Coupon / CouponFrequency,
        NextCoupon >= SettleDate &
          NextCoupon < FutureSettleDate ~ 100 * Coupon / CouponFrequency,
        TRUE ~ 0
      ),
      NetBasis = ((BondPrice + Accrued - CouponCorrection) *
                   (1 + ActualRepo * (DaysToDelivery/360))) -
                     ((FuturePrice * ConversionFactor) + FwdAccrued),
      NetBasisBP = 100 * NetBasis / (FwdModDuration * FwdCleanPrice / 100),
      BondCarryBP = 10000 * (((Yield - ActualRepo) * (DaysToDelivery/360)) / ModDuration),
      InvoicePrice = (FuturePrice * ConversionFactor) + FwdAccrued,
      ImpliedRepoRate =
        ((InvoicePrice / (BondPrice + Accrued -CouponCorrection)) - 1) * (360/DaysToDelivery)
    ) %>%
    dplyr::left_join(
      ois_data %>% dplyr::select(Maturity, "OIS" = Value, Currency),
      by = c("FutureSettleDate" = "Maturity", "Currency")
    ) %>%
    dplyr::mutate(
      IRRvsOIS = ImpliedRepoRate - OIS
    )

  # Do not show if we have passed expected delivery date
  df <- df %>%
    dplyr::filter(DaysToDelivery > 0)

  # Only show all columns verbose was set
  if ( ! verbose ) {
    df <- df %>%
      dplyr::select(
        FutureCode,
        Bond,
        BondPrice,
        Yield,
        BondCarryBP,
        ConversionFactor,
        FuturePrice,
        Delivery = FutureSettleDate,
        FuturePV01,
        FwdBondPV01,
        ActualRepo,
        ImpliedRepoRate,
        IRRvsOIS,
        GrossBasis,
        NetBasis,
        NetBasisBP
      )
  }

  df %>%
    dplyr::arrange(
      FutureCode, dplyr::desc(ImpliedRepoRate)
    )

}


#' Vectorise the bond.TCF functions from jrvFinance::bond.TCF
#'
#' @param settle Settlement date
#' @param mature Maturity date
#' @param coupon Bond coupon as a decimal (0.1 for 10 percent)
#' @param freq Coupon frequency
#' @param convention Cashflow convention e.g. "30/360", "ACT/ACT", "ACT/360", "30/360E"
#' @param redemption_value Bullet redemption payment, defaults to 100
#'
#' @return A vector of accrued interest
#' @export
#'
#' @examples
#' library(FIRVr)
#' calculate_accrued_interest("2030-02-02", "2040-02-02", 0.025, 2, "ACT/ACT", 100)
calculate_accrued_interest <- function(settle,
                                       mature,
                                       coupon,
                                       freq,
                                       convention,
                                       redemption_value = 100) {
  bond_cf <- mapply(
    jrvFinance::bond.TCF,
    settle,
    mature,
    coupon,
    freq,
    convention,
    redemption_value,
    USE.NAMES = FALSE
  )

  bond_cf["accrued", ] %>% unlist() %>% as.numeric()

}


#' Helper function to vectorise duration calculation (bond.durations()
#' does not vectorise the convention argument)
#'
#' @param settle Settlement date
#' @param mature Maturity date
#' @param coupon Bond coupon as a decimal (0.1 for 10 percent)
#' @param freq Coupon frequency
#' @param yield Yield to maturity of the bond
#' @param convention Cashflow convention e.g. "30/360", "ACT/ACT", "ACT/360", "30/360E"
#' @param modified A logical, set TRUE for modified duration
#' @param comp.freq The frequency of compounding of the bond yield: 1 for annual, 2 for semi
#' @param redemption_value Bullet redemption payment, defaults to 100
#'
#' @return Calculated durations
#' @export
#'
#' @examples
#' library(FIRVr)
#' calculate_duration("2030-02-02", "2040-02-02", 0.025, 2, 0.212, "ACT/ACT",
#'                    TRUE, 2, 100)
calculate_duration <- function(settle,
                               mature,
                               coupon,
                               freq,
                               yield,
                               convention,
                               modified,
                               comp.freq,
                               redemption_value = 100) {
  mapply(jrvFinance::bond.duration, settle, mature, coupon, freq, yield,
         convention, modified, comp.freq, redemption_value, USE.NAMES = FALSE)
}


#' Get CTD data from Bloomberg
#'
#' @param contracts A future codes for the contracts to include
#'
#' @return Data frame of results
#' @export
#'
#' @examples
#' library(FIRVr)
#' get_ctd_dataset(c("DU", "TY"))
get_ctd_dataset <- function(contracts, sequence = c("A", "B")) {

  # First, map generic tickers to actual contracts
  front <- paste0(contracts, "1 Comdty")
  back <- paste0(contracts, "2 Comdty")

  bbg_static_data <- bloomberg_query_static(
    c(front, back),
    "FUT_CUR_GEN_TICKER"
  )

  if ( nrow(bbg_static_data) < 1 ) {
    return(tibble::tibble())
  }

  all_contracts <- bbg_static_data %>%
    dplyr::pull(ValueChr)

  # Get the deliverable table
  bbg_data_list <- list()
  for ( contract in paste(all_contracts, "Comdty") ) {
    bbg_data_list[[contract]] <- bloomberg_query_dataset(
      contract,
      "FUT_DLVRBLE_BNDS_ISINS"
    ) %>%
      dplyr::mutate(
        Security = contract,
        ISIN = substr(`ISIN of Deliverable Bonds`, 1, 12)
      ) %>%
      dplyr::rename(
        ConversionFactor = `Conversion Factor`
      ) %>%
      dplyr::select(-`ISIN of Deliverable Bonds`)

  }

  bbg_data <- dplyr::bind_rows(bbg_data_list)

  # Get future data including contract codes e.g. RXZ9 from RXA Comdty
  future_data <- bloomberg_query_static(
    unique(bbg_data$Security),
    c("SECURITY_DES",
      "PX_LAST",
      "CRNCY",
      "FUT_CONT_SIZE",
      "FUT_DLV_DT_FIRST",
      "FUT_DLV_DT_LAST",
      "FUT_ACTUAL_REPO_RT"),
    tidy_data = FALSE
  ) %>%
    dplyr::mutate(
      FUT_ACTUAL_REPO_RT = FUT_ACTUAL_REPO_RT / 100
    ) %>%
    dplyr::rename(
      FutureCode = SECURITY_DES,
      FuturePrice = PX_LAST,
      Currency = CRNCY,
      ContractSize = FUT_CONT_SIZE,
      FirstDelivery = FUT_DLV_DT_FIRST,
      LastDelivery = FUT_DLV_DT_LAST,
      ActualRepo = FUT_ACTUAL_REPO_RT
    )

  # Get bond data
  bond_data <- bloomberg_query_static(
    paste(unique(bbg_data$ISIN), "Govt"),
    c("ID_ISIN",
      "SECURITY_DES",
      "ISSUE_DT",
      "MATURITY",
      "CPN",
      "NXT_CPN_DT",
      "PREV_CPN_DT",
      "YLD_YTM_MID",
      "CPN_FREQ",
      "DAY_CNT_DES",
      "SETTLE_DT",
      "PX_LAST",
      "RISK_MID"
    ),
    tidy_data = FALSE
  ) %>%
    dplyr::mutate(
      CPN = CPN / 100,
      YLD_YTM_MID = YLD_YTM_MID / 100
    ) %>%
    dplyr::rename(
      ISIN = ID_ISIN,
      Bond = SECURITY_DES,
      IssueDate = ISSUE_DT,
      Maturity = MATURITY,
      BondPrice = PX_LAST,
      Coupon = CPN,
      RiskMid = RISK_MID,
      NextCoupon = NXT_CPN_DT,
      PreviousCoupon = PREV_CPN_DT,
      CouponFrequency = CPN_FREQ,
      Yield = YLD_YTM_MID,
      DayCountConv = DAY_CNT_DES,
      SettleDate = SETTLE_DT
    ) %>%
    dplyr::select(-Security)

  # Join in future and bond data
  bbg_data %>%
    dplyr::left_join(future_data, by = "Security") %>%
    dplyr::left_join(bond_data, by = "ISIN")

}


