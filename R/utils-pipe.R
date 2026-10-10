#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# Suppress R CMD check NOTEs for Tidyverse / SQL NSE symbols
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "RecrID", "activity_pattern", "Time_segment", "Counts_transport_type_ID",
    "groupsRecr_at", "number of locations", "groupsRecr_act_ts", "number_of_locations",
    "V4", "V3", "V2", "V1", "NumberAvail", "factivity_pattern", "fgroupsRecr_at",
    "d1", "sol5", "units", "number_of_visits", "activity_pattern_definition",
    "transport_type_ID", "transport_type_definition", "Time_available_per_time_segment",
    "distances_km", "total_activities_costs", "fuel_cost_total", "expected_visiting_duration",
    "PivotTable", "PivotFilterOverrides", ".GUI", "conn", "linear_solution",
    "Table_l1", "timeMax", "lin_cond_max", "ActMax", "qp_solution", "n_distinct",
    "n", "Sum_timeMax", "recreation_areas"
  ))
}
