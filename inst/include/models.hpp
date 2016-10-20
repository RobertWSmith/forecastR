// #ifndef __MODELS_HPP
// #define __MODELS_HPP
//
// #include <Rcpp.h>
//
// extern "C" {
// #define class xts_class
// #include <xts.h>
// #undef class
// }
//
//
// class TimeSeriesModel {
// public:
//   TimeSeriesModel();
//   TimeSeriesModel(NumericMatrix);
//   TimeSeriesModel(NumericMatrix, List);
//   TimeSeriesModel(const TimeSeriesModel&);
//   ~TimeSeriesModel();
//
//   virtual void fit() = 0;
//   virtual void fit(NumericMatrix) = 0;
//   virtual void fit(List) = 0;
//   virtual void fit(NumericMatrix, List) = 0;
//
//   virtual List summary() = 0;
//   virtual List fit_stats() = 0;
//   virtual NumericVector residuals() = 0;
//   virtual NumericVector fitted() = 0;
//   virtual DataFrame tidy() = 0;
//
// protected:
//   // function must return list
//   List model_results;
//   List model_function_args;
//   NumericMatrix model_data;
// };
//
// class Forecast {
// public:
//   Forecast();
//   Forecast(const TimeSeriesModel &);
//   Forecast(const Forecast &);
//   ~Forecast();
//
// protected:
//   const TimeSeriesModel& tsm;
//   List forecast_output;
// };
//
// class ARIMA : public TimeSeriesModel
// {
// public:
//   ARIMA();
//   ARIMA(NumericMatrix);
//   ARIMA(NumericMatrix, List);
//   ARIMA(const TimeSeriesModel&);
//   ~ARIMA();
//
//   void fit();
//   void fit(NumericMatrix);
//   void fit(List);
//   void fit(NumericMatrix, List);
//
//   List summary();
//   List fit_stats();
//   NumericVector residuals();
//   NumericVector fitted();
//   DataFrame tidy();
// };
//
// class ARFIMA : public TimeSeriesModel
// {
// public:
//   ARFIMA();
//   ARFIMA(NumericMatrix);
//   ARFIMA(NumericMatrix, List);
//   ARFIMA(const TimeSeriesModel&);
//   ~ARFIMA();
//
//   void fit();
//   void fit(NumericMatrix);
//   void fit(List);
//   void fit(NumericMatrix, List);
//
//   List summary();
//   List fit_stats();
//   NumericVector residuals();
//   NumericVector fitted();
//   DataFrame tidy();
// };
//
//
// class ETS : public TimeSeriesModel
// {
// public:
//   ETS();
//   ETS(NumericMatrix);
//   ETS(NumericMatrix, List);
//   ETS(const TimeSeriesModel&);
//   ~ETS();
//
//   void fit();
//   void fit(NumericMatrix);
//   void fit(List);
//   void fit(NumericMatrix, List);
//
//   List summary();
//   List fit_stats();
//   NumericVector residuals();
//   NumericVector fitted();
//   DataFrame tidy();
// };
//
// class NNETAR : public TimeSeriesModel
// {
// public:
//   NNETAR();
//   NNETAR(NumericMatrix);
//   NNETAR(NumericMatrix, List);
//   NNETAR(const TimeSeriesModel&);
//   ~NNETAR();
//
//   void fit();
//   void fit(NumericMatrix);
//   void fit(List);
//   void fit(NumericMatrix, List);
//
//   List summary();
//   List fit_stats();
//   NumericVector residuals();
//   NumericVector fitted();
//   DataFrame tidy();
// };
//
//
// class STLM : public TimeSeriesModel
// {
// public:
//   STLM();
//   STLM(NumericMatrix);
//   STLM(NumericMatrix, List);
//   STLM(const TimeSeriesModel&);
//   ~STLM();
//
//   void fit();
//   void fit(NumericMatrix);
//   void fit(List);
//   void fit(NumericMatrix, List);
//
//   List summary();
//   List fit_stats();
//   NumericVector residuals();
//   NumericVector fitted();
//   DataFrame tidy();
// };
//
//
// class TBATS : public TimeSeriesModel
// {
// public:
//   TBATS();
//   TBATS(NumericMatrix);
//   TBATS(NumericMatrix, List);
//   TBATS(const TimeSeriesModel&);
//   ~TBATS();
//
//   void fit();
//   void fit(NumericMatrix);
//   void fit(List);
//   void fit(NumericMatrix, List);
//
//   List summary();
//   List fit_stats();
//   NumericVector residuals();
//   NumericVector fitted();
//   DataFrame tidy();
// };
//
// }
//
// #endif  /// __MODELS_HPP
