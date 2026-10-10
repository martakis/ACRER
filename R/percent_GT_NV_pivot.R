#' Generate Percentage of Grand Total Number of Visits
#'
#' Generates a pivot table calculating the percentage of the Grand Total number of
#' visits across all recreation areas, transport types, and zip codes.
#'
#' @param my_data A data frame containing recreation model results (e.g. 13 variables
#'   including recreation areas, transport types, zip codes, and number of visits).
#'
#' @return An object of class \code{PivotTable} (\code{\link[pivottabler]{PivotTable}})
#'   containing the Grand Total percentages of visits across recreation areas,
#'   transport types, and zip codes.
#'
#' @importFrom pivottabler PivotTable PivotFilterOverrides
#' @export
percent_GT_NV_pivot <- function(my_data) {
pt1 <- PivotTable$new()
pt1$addData(my_data)
pt1$addRowDataGroups("recreation_areas",outlineBefore = list(groupStyleDeclarations = list(color = "blue")),
                    outlineAfter = list(isEmpty = FALSE,
                                        mergeSpace = "dataGroupsOnly",
                                        caption = "Total ({value})",
                                        groupStyleDeclarations = list("font-style" = "italic")),
                    outlineTotal = list(groupStyleDeclarations = list(color = "blue"),
                                        cellStyleDeclarations = list("color" = "blue")))
pt1$addRowDataGroups("transport_type_definition",header = "transport type", totalCaption = "All_transport_types",styleDeclaration = list("color" = "black","background-color" = "lightgreen"))
pt1$addColumnDataGroups("zip_code",totalCaption = "All_zip_codes", styleDeclarations = list("color" = "white",
                       "font-weight" = "bold","background-color" = "blue"))
pt1$defineCalculation(calculationName = "Count_hgpID", summariseExpression = "n()", format = "%.2f", visible = TRUE)
pt1$defineCalculation(calculationName = "sum_number_of_visits", summariseExpression = "sum(number_of_visits)", format = "%.2f", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt1, removeAllFilters = TRUE)
pt1$defineCalculation(calculationName = "GrandTotalRecreationAreas", filters = filterOverrides,
                     summariseExpression = "sum(number_of_visits)", format = "%.2f", caption = "GrandTotalRecreationAreas")
pt1$defineCalculation(calculationName = "number_of_visits_%_GT",type = "calculation", basedOn = c("sum_number_of_visits","GrandTotalRecreationAreas"),calculationExpression = "values$sum_number_of_visits/values$GrandTotalRecreationAreas * 100",
                      caption = "% of All", format = "%.2f")
pt1$evaluatePivot()
return(pt1)
}
