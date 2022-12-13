
#include <math.h>
#include <ql/quantlib.hpp>
#include <Rcpp.h>

using namespace QuantLib;


BusinessDayConvention getBusinessDayConvention(std::string s){

  transform(s.begin(), s.end(), s.begin(), tolower);

  if (s == "following")
    return Following;
  else if (s == "modifiedfollowing")
    return ModifiedFollowing;
  else if (s == "preceding")
    return Preceding;
  else if (s == "modifiedpreceding")
    return ModifiedPreceding;
  else if (s == "unadjusted")
    return Unadjusted;
  else if (s == "halfmonthmodifiedfollowing")
    return HalfMonthModifiedFollowing;
  else if (s == "nearest")
    return Nearest;
  else
    return Unadjusted;
}

DayCounter getDayCounter(std::string s){

  transform(s.begin(), s.end(), s.begin(), tolower);

  if (s == "actual360")
    return Actual360();
  else if (s == "actual365fixed")
    return Actual365Fixed();
  else if (s == "actualactual")
    return ActualActual();
  else if (s == "business252")
    return Business252();
  else if (s == "onedaycounter")
    return OneDayCounter();
  else if (s == "simpledaycounter")
    return SimpleDayCounter();
  else if (s == "thirty360")
    return Thirty360();
  else if (s == "actual365noleap")
    return Actual365Fixed(Actual365Fixed::NoLeap);
  else if (s == "actual365fixednoleap")
    return Actual365Fixed(Actual365Fixed::NoLeap);
  else if (s == "actualactualisma")
    return ActualActual(QuantLib::ActualActual::ISMA);
  else if (s == "actualactualbond")
    return ActualActual(QuantLib::ActualActual::Bond);
  else if (s == "actualactualisda")
    return ActualActual(QuantLib::ActualActual::ISDA);
  else if (s == "actualactualhistorical")
    return ActualActual(QuantLib::ActualActual::Historical);
  else if (s == "actualactualafb")
    return ActualActual(QuantLib::ActualActual::AFB);
  else if (s == "actualactualeuro")
    return ActualActual(QuantLib::ActualActual::Euro);
  else {
    std::cout << "Warning: No day count specified so using ActualActual::Euro" << std::endl;
    return ActualActual(QuantLib::ActualActual::Euro);
  }

}

Frequency getFrequency(std::string s){

  transform(s.begin(), s.end(), s.begin(), tolower);

  if (s == "once")
    return Once;
  else if (s == "annual")
    return Annual;
  else if (s == "semiannual")
    return Semiannual;
  else if (s == "everyfourthmonth")
    return EveryFourthMonth;
  else if (s == "quarterly")
    return Quarterly;
  else if (s == "bimonthly")
    return Bimonthly;
  else if (s == "monthly")
    return Monthly;
  else if (s == "everyfourthweek")
    return EveryFourthWeek;
  else if (s == "biweekly")
    return Biweekly;
  else if (s == "weekly")
    return Weekly;
  else if (s == "daily")
    return Daily;
  else {
    std::cout << "Warning: No frequency specified so using OtherFrequency" << std::endl;
    return OtherFrequency;
  }
}


// [[Rcpp::export]]
Rcpp::DataFrame fitPiecewiseBondCurve(
    std::vector<Rcpp::Date> maturityDates,
    std::vector<double> cleanPrices,
    std::vector<double> coupons,
    Rcpp::Date startDate,
    Rcpp::List config)
{

  // Should probably be TARGET / NYC depending on currency?
  Calendar calendar = NullCalendar();

  const double FACE_VALUE = 100.0;
  double settlementDays = Rcpp::as<double>(config["settlementDays"]);
  double forwardPeriodMonths = Rcpp::as<double>(config["forwardPeriodMonths"]);
  QuantLib::Period forwardPeriod = QuantLib::Period(forwardPeriodMonths*Months);
  std::string businessDayConv = Rcpp::as<std::string>(config["businessDayConvention"]);
  std::string dayCountConv    = Rcpp::as<std::string>(config["dayCountConvention"]);
  std::string couponFrequency = Rcpp::as<std::string>(config["couponFrequency"]);
  std::string trait  = Rcpp::as<std::string>(config["trait"]);
  std::string interp = Rcpp::as<std::string>(config["interp"]);

  transform(trait.begin(),  trait.end(),  trait.begin(),  tolower);
  transform(interp.begin(), interp.end(), interp.begin(), tolower);

  Frequency frequency = getFrequency(couponFrequency);
  BusinessDayConvention convention = getBusinessDayConvention(businessDayConv);
  DayCounter dayCounter = getDayCounter(dayCountConv);
  Real redemption = 100.0;

  Date today = Date(Day(startDate.getDay()), Month(startDate.getMonth()), Year(startDate.getYear()));
  Settings::instance().evaluationDate() = today;
  Date bondSettlementDate = calendar.advance(today, settlementDays*Days);

  // Clean price quotes

  const Size numberOfBonds = maturityDates.size();

  std::vector< boost::shared_ptr<SimpleQuote> > quote;
  for (Size i = 0; i < numberOfBonds; i++) {
    boost::shared_ptr<SimpleQuote> cp(new SimpleQuote(cleanPrices[i]));
    quote.push_back(cp);
  }

  RelinkableHandle<Quote> quoteHandle[numberOfBonds];
  for (Size i = 0; i < numberOfBonds; i++) {
    quoteHandle[i].linkTo(quote[i]);
  }

  // Construct a vector of instruments to be bootstrapped

  std::vector< boost::shared_ptr<RateHelper> > instruments;

  Date maxMaturity = today;

  for (Size j = 0; j < maturityDates.size(); j++) {

    Rcpp::Date rcppDate = maturityDates[j];

    Date maturity = Date(Day(rcppDate.getDay()),
                         Month(rcppDate.getMonth()),
                         Year(rcppDate.getYear()));

    if ( maturity > maxMaturity ) {
      maxMaturity = maturity;
    }

    Schedule schedule(bondSettlementDate, maturity, Period(frequency),
                      calendar, convention, convention,
                      DateGeneration::Backward, false);

    boost::shared_ptr<RateHelper> helper(
        new FixedRateBondHelper(quoteHandle[j],
                                settlementDays,
                                FACE_VALUE,
                                schedule,
                                std::vector<Rate>(1, coupons[j]),
                                dayCounter,
                                convention,
                                redemption));

    instruments.push_back(helper);

  }

  // Build the model. Feels like this should be possible with templates...

  QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure> curve;

  if ( trait == "zeroyield" ) {
    if ( interp == "linear" ) {
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<ZeroYield, Linear>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    } else if ( interp == "cubic" ) {
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<ZeroYield, Cubic>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    } else {
      if ( trait != "loglinear" ) {
        std::cout << "Defaulting to LogLinear interpolation" << std::endl;
      }
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<ZeroYield, LogLinear>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    }
  } else if ( trait == "forwardrate" ) {
    if ( interp == "linear" ) {
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<ForwardRate, Linear>(
              settlementDays, calendar, instruments, dayCounter
          )
      );
      curve = termStructure;
    } else if ( interp == "cubic" ) {
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<ForwardRate, Cubic>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    } else {
      if ( trait != "loglinear" ) {
        std::cout << "Defaulting to LogLinear interpolation" << std::endl;
      }
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<ForwardRate, LogLinear>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    }
  } else {
    if ( trait != "discount" ) {
      std::cout << "Defaulting to Discount trait" << std::endl;
    }
    if ( interp == "linear" ) {
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<Discount, Linear>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    } else if ( interp == "cubic" ) {
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<Discount, Cubic>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    } else {
      if ( interp != "loglinear" ) {
        std::cout << "Defaulting to LogLinear interpolation" << std::endl;
      }
      boost::shared_ptr<YieldTermStructure> termStructure(
          new PiecewiseYieldCurve<Discount, LogLinear>(
              settlementDays, calendar, instruments, dayCounter)
      );
      curve = termStructure;
    }

  }

  // Build a data structure for the fitted curve and return as data frame

  QuantLib::Date current = curve->referenceDate();
  Date curveMaxDate = curve->maxDate();
  int n = curveMaxDate - curve->referenceDate();

  Rcpp::DateVector dates(n);
  Rcpp::NumericVector zeroRates(n);
  Rcpp::NumericVector dFactors(n);
  Rcpp::NumericVector forwards(n);

  for (int i = 0; i < n; i++) {
    QuantLib::Date d = current;
    QuantLib::Date fwdDate = calendar.advance(current, forwardPeriod, convention);
    dates[i] =  Rcpp::Date(d.month(), d.dayOfMonth(), d.year());
    zeroRates[i] = curve->zeroRate(current, dayCounter, Continuous);
    dFactors[i] = curve->discount(current);
    if ( fwdDate < curveMaxDate ) {
      forwards[i] = curve->forwardRate(current, fwdDate, dayCounter, Continuous);
    } else {
      forwards[i] = NA_REAL;
    }
    current++;
  }

  return(Rcpp::DataFrame::create(
      Rcpp::Named("Date") = dates,
      Rcpp::Named("ZeroRate") = zeroRates,
      Rcpp::Named("Discount") = dFactors,
      Rcpp::Named("Forward") = forwards
  )
  );

}
