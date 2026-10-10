#' Generate Percentage of Grand Total Expected Visiting Duration
#'
#' Generates a pivot table calculating the percentage of the Grand Total expected
#' visiting duration across all recreation areas, transport types, and zip codes.
#'
#' @param my_data A data frame containing recreation model results (e.g. 13 variables
#'   including recreation areas, transport types, zip codes, and expected visiting duration).
#'
#' @return An object of class \code{PivotTable} (\code{\link[pivottabler]{PivotTable}})
#'   containing the Grand Total percentages of expected visiting duration across
#'   recreation areas, transport types, and zip codes.
#'
#' @importFrom pivottabler PivotTable PivotFilterOverrides
#' @export
percent_GT_EVD_pivot <- function(my_data) {
    pt3 <- PivotTable$new()
    pt3$addData(my_data)
    pt3$addRowDataGroups("recreation_areas",outlineBefore = list(groupStyleDeclarations = list(color = "blue")),
                         outlineAfter = list(isEmpty = FALSE,
                                             mergeSpace = "dataGroupsOnly",
                                             caption = "Total ({value})",
                                             groupStyleDeclarations = list("font-style" = "italic")),
                         outlineTotal = list(groupStyleDeclarations = list(color = "blue"),
                                             cellStyleDeclarations = list("color" = "blue")))
    pt3$addRowDataGroups("transport_type_definition",header = "transport type", totalCaption = "All_transport_types",styleDeclaration = list("color" = "black","background-color" = "lightgreen"))
    pt3$addColumnDataGroups("zip_code",totalCaption = "All_zip_codes", styleDeclarations = list("color" = "white",
    "font-weight" = "bold","background-color" = "blue"))
    pt3$defineCalculation(calculationName = "Count_hgpID", summariseExpression = "n()", format = "%.2f", visible = TRUE)
    pt3$defineCalculation(calculationName = "total_exp_visit_duration",
    summariseExpression = "sum(expected_visiting_duration)",format = "%.1f", visible = TRUE)
    filterOverrides <- PivotFilterOverrides$new(pt3, removeAllFilters = TRUE)
    pt3$defineCalculation(calculationName = "GrandTotalRecreationAreas", filters = filterOverrides,
                          summariseExpression = "sum(expected_visiting_duration)",
                          format = "%.2f", caption = "GrandTotalRecreationAreas")
    pt3$defineCalculation(calculationName = "Exp_vs_dur_%_GT",type = "calculation",
                          basedOn = c("total_exp_visit_duration","GrandTotalRecreationAreas"),
                          calculationExpression = "values$total_exp_visit_duration/values$GrandTotalRecreationAreas * 100",
                          caption = "% of All", format = "%.2f")
    pt3$evaluatePivot()
    return(pt3)
}
