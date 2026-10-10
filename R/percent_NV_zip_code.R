#' Generate Percentage of Number of Visits per Zip Code
#'
#' Generates a pivot table calculating the percentage of the row total number of
#' visits per zip code across recreation areas and transport types.
#'
#' @param my_data A data frame containing recreation model results (e.g. 13 variables
#'   including recreation areas, transport types, zip codes, and number of visits).
#'
#' @return An object of class \code{PivotTable} (\code{\link[pivottabler]{PivotTable}})
#'   containing the calculated percentages of visits per zip code across
#'   recreation areas and transport types.
#'
#' @importFrom pivottabler PivotTable PivotFilterOverrides
#' @export
percent_NV_zip_code <- function(my_data) {
pt2<- PivotTable$new()
pt2$addData(my_data)
pt2$addRowDataGroups("zip_code",outlineBefore = list(groupStyleDeclarations = list(color = "blue")),
                     outlineAfter = list(isEmpty = FALSE,
                                         mergeSpace = "dataGroupsOnly",
                                         caption = "Total ({value})",
                                         groupStyleDeclarations = list("font-style" = "italic")),
                     outlineTotal = list(groupStyleDeclarations = list(color = "blue"),
                                         cellStyleDeclarations = list("color" = "blue")))
pt2$addRowDataGroups("transport_type_definition",header = "transport type", totalCaption = "All_transport_types",styleDeclaration = list("color" = "black","background-color" = "lightgreen"))
pt2$addColumnDataGroups("recreation_areas",totalCaption = "All_recreation_areas", styleDeclarations = list("color" = "white", "font-weight" = "bold","background-color" = "blue"))
pt2$defineCalculation(calculationName = "Sum_of_number_of_visits", summariseExpression = "sum(number_of_visits)", format = "%.2f", visible = TRUE)
pt2$defineCalculation(calculationName = "Count_hgpID", summariseExpression = "n()", format = "%.2f", visible = TRUE)
filterOverrides <- PivotFilterOverrides$new(pt2, keepOnlyFiltersFor = "zip_code")
pt2$defineCalculation(calculationName = "zip_codes_Total", filters = filterOverrides,
                     summariseExpression = "sum(number_of_visits)", caption = "zip_codes_Total", visible = TRUE)
pt2$defineCalculation(calculationName = "N_of_vis_%_zipcode",
                     type = "calculation", basedOn = c("Sum_of_number_of_visits","zip_codes_Total"),format = "%.2f",
                     calculationExpression = "values$Sum_of_number_of_visits/values$zip_codes_Total * 100")
pt2$evaluatePivot()
return(pt2)
}
