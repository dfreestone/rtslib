#include <Rcpp.h>
using namespace Rcpp;
// TODO(David): Have to test this. I have no idea if it works after the changes.

// [[Rcpp::export]]
NumericVector c_flushes(NumericVector data, int nevents)
{
  NumericVector flush(nevents);

  int num_arrays_index = 17;  // number of arrays in the program written
  int num_events_index = 18;  // number of events in the flush written
                              // we start the events at:
                              //    num_events_index + data[num_arrays_index]

  int flush_number = 1;
  int flush_end = num_events_index + data[num_arrays_index] + data[num_events_index];

  for (int eventIndex=0; eventIndex<nevents; ++eventIndex)
  {
    flush[eventIndex] = flush_number;

    if (eventIndex == flush_end)
    {
      ++flush_number;
      flush_end += 1 + num_events_index + data[num_arrays_index] + data[num_events_index];
    }
  } // for
  return (flush);
} // function

// [[Rcpp::export]]
NumericVector c_parameter_ids(NumericVector event, int event_count,
                              int start_code, int stop_code)
{
  NumericVector ids(event_count);

  int state = 0;
  int id_count = 0;
  for (int eventIndex = 0; eventIndex < event_count; eventIndex++)
  {
    if (state == 1) {
      ids[eventIndex] = ++id_count;
    } // if

    if ((state == 1) && (event[eventIndex] == stop_code))
      state = 0;

    if ((state == 0) && (event[eventIndex] == start_code))
    {
      state = 1;
      id_count = 0;
    } // if
  } // for
  return(ids);
}
