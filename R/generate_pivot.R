#' Generate a Pivot Table by Recreation Area and Zip Code
#'
#' Generates a pivot table summarizing recreation areas and transport types as
#' rows and zip codes as columns.
#'
#' @param my_data A data frame containing recreation model results (e.g. 13 variables
#'   including recreation areas, transport types, zip codes, distances, fuel costs,
#'   activity costs, visits, and expected visiting duration).
#'
#' @return An object of class \code{PivotTable} (\code{\link[pivottabler]{PivotTable}})
#'   containing summary statistics (counts of household groups, mean distances,
#'   mean fuel costs, mean activity costs, sum and mean of visits, and sum and mean
#'   of expected visiting duration) structured across recreation areas, transport
#'   modes, and zip codes.
#'
#' @importFrom pivottabler PivotTable
#' @export
generate_pivot <- function(my_data) {
    pt <- pivottabler::PivotTable$new()
    pt$addData(my_data) # Loads data automatically from data/
    pt$addRowDataGroups("recreation_areas",outlineBefore=list(groupStyleDeclarations=list(color="blue")),
                        outlineAfter=list(isEmpty=FALSE,
                                          mergeSpace="dataGroupsOnly",
                                          caption="Total ({value})",
                                          groupStyleDeclarations=list("font-style"="italic")),
                        outlineTotal=list(groupStyleDeclarations=list(color="blue"),
                                          cellStyleDeclarations=list("color"="blue")))
    pt$addRowDataGroups("transport_type_definition", header = "transport type",
    totalCaption = "All_transport_types", styleDeclaration = list("color" = "black","background-color" = "lightgreen"))
    pt$addColumnDataGroups("zip_code",totalCaption = "All_zip_codes", expandExistingTotals=TRUE,
    styleDeclarations = list("color" = "white", "font-weight" = "bold","background-color" = "blue"))
    pt$defineCalculation(calculationName = "distances_km",summariseExpression = "mean(as.numeric(distances_km))",
                             format = "%.2f")
    pt$defineCalculation(calculationName = "Count_hgpID", summariseExpression = "n()", format = "%.2f", visible = TRUE)
    pt$defineCalculation(calculationName = "total_fuel_cost", summariseExpression = "mean(as.numeric(fuel_cost_total))",
                         format = "%.2f")
    pt$defineCalculation(calculationName = "T_activ_costs",summariseExpression = "mean(as.numeric(total_activities_costs))",format = "%.2f ")
    pt$defineCalculation(calculationName = "Sum_of_number_of_visits",
                         summariseExpression = "sum(number_of_visits)",format = "%.2f", visible = TRUE)
    pt$defineCalculation(calculationName = "expected_visiting_duration",
                         summariseExpression = "sum(expected_visiting_duration)",format = "%.2f", visible = TRUE)
    pt$defineCalculation(calculationName = "mean_of_visits", summariseExpression = "mean(number_of_visits)", format = "%.2f")
    pt$defineCalculation(calculationName = "mean_exp_visit_duration",
                         summariseExpression = "mean(expected_visiting_duration)",format = "%.2f")
    pt$evaluatePivot()
    return(pt)
}
