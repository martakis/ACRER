#' Generate Percentage of Expected Visiting Duration per Recreation Area
#'
#' Generates a pivot table calculating the percentage of the row total expected
#' visiting duration per recreation area across zip codes and transport types.
#'
#' @param my_data A data frame containing recreation model results (e.g. 13 variables
#'   including recreation areas, transport types, zip codes, and expected visiting duration).
#'
#' @return An object of class \code{PivotTable} (\code{\link[pivottabler]{PivotTable}})
#'   containing the calculated percentages of expected visiting duration per recreation area,
#'   allowing comparison of destination area shares across zip codes and transport modes.
#'
#' @importFrom pivottabler PivotTable PivotFilterOverrides
#' @export
percent_EVD_Recr <- function(my_data) {
    pt6<- PivotTable$new()
    pt6$addData(my_data)
    pt6$addRowDataGroups("recreation_areas",outlineBefore = list(groupStyleDeclarations = list(color = "blue")),
                          outlineAfter = list(isEmpty = FALSE,
                                              mergeSpace = "dataGroupsOnly",
                                              caption = "Total ({value})",
                                              groupStyleDeclarations = list("font-style" = "italic")),
                          outlineTotal = list(groupStyleDeclarations = list(color = "blue"),
                                              cellStyleDeclarations = list("color" = "blue")))
    pt6$addRowDataGroups("transport_type_definition",header = "transport type", totalCaption = "All_transport_types",styleDeclaration = list("color" = "black","background-color" = "lightgreen"))
    pt6$addColumnDataGroups("zip_code",totalCaption = "All_recreation_areas", styleDeclarations = list("color" = "white", "font-weight" = "bold","background-color" = "blue"))

    pt6$defineCalculation(calculationName = "Sum_of_expected_visiting_duration", summariseExpression = "sum(expected_visiting_duration)", format = "%.2f", visible = TRUE)
    pt6$defineCalculation(calculationName = "Count_hgpID", summariseExpression = "n()", format = "%.2f", visible = TRUE)
    filterOverrides <- PivotFilterOverrides$new(pt6, keepOnlyFiltersFor = "recreation_areas")
    pt6$defineCalculation(calculationName = "recreation_areas_Total", filters = filterOverrides,
                           summariseExpression = "sum(expected_visiting_duration)", caption = "recreation_areas_Total", visible = TRUE)
    pt6$defineCalculation(calculationName = "N_of_vis_%_Recr",
                           type = "calculation", basedOn = c("Sum_of_expected_visiting_duration","recreation_areas_Total"),format = "%.2f",
                           calculationExpression = "values$Sum_of_expected_visiting_duration/values$recreation_areas_Total * 100")
    pt6$evaluatePivot()
    return(pt6)
}
