#' Generate Percentage of Number of Visits per Recreation Area
#'
#' Generates a pivot table calculating the percentage of the row total number of
#' visits per recreation area across zip codes and transport types.
#'
#' @param my_data A data frame containing recreation model results (e.g. 13 variables
#'   including recreation areas, transport types, zip codes, and number of visits).
#'
#' @return An object of class \code{PivotTable} (\code{\link[pivottabler]{PivotTable}})
#'   containing the calculated percentages of visits per recreation area across
#'   zip codes and transport types.
#'
#' @importFrom pivottabler PivotTable PivotFilterOverrides
#' @export
percent_NV_Recr <- function(my_data) {
ppt4<- PivotTable$new()
ppt4$addData(my_data)
ppt4$addRowDataGroups("recreation_areas",outlineBefore = list(groupStyleDeclarations = list(color = "blue")),
                     outlineAfter = list(isEmpty = FALSE,
                                         mergeSpace = "dataGroupsOnly",
                                         caption = "Total ({value})",
                                         groupStyleDeclarations = list("font-style" = "italic")),
                     outlineTotal = list(groupStyleDeclarations = list(color = "blue"),
                                         cellStyleDeclarations = list("color" = "blue")))
ppt4$addRowDataGroups("transport_type_definition",header = "transport type", totalCaption = "All_transport_types",styleDeclaration = list("color" = "black","background-color" = "lightgreen"))
ppt4$addColumnDataGroups("zip_code",totalCaption = "All_recreation_areas", styleDeclarations = list("color" = "white", "font-weight" = "bold","background-color" = "blue"))
ppt4$defineCalculation(calculationName = "Sum_of_number_of_visits", summariseExpression = "sum(number_of_visits)", format = "%.2f", visible = TRUE)
ppt4$defineCalculation(calculationName = "Count_hgpID", summariseExpression = "n()", format = "%.2f", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(ppt4, keepOnlyFiltersFor = "recreation_areas")
ppt4$defineCalculation(calculationName = "recreation_areas_Total", filters = filterOverrides,
                     summariseExpression = "sum(number_of_visits)", caption = "recreation_areas_Total", visible = TRUE)
ppt4$defineCalculation(calculationName = "N_of_vis_%_Recr",
                     type = "calculation", basedOn = c("Sum_of_number_of_visits","recreation_areas_Total"),format = "%.2f",
                     calculationExpression = "values$Sum_of_number_of_visits/values$recreation_areas_Total * 100")
ppt4$evaluatePivot()
return(ppt4)
}
