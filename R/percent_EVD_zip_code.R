#' Generate Percentage of Expected Visiting Duration per Zip Code
#'
#' Generates a pivot table calculating the percentage of the row total expected
#' visiting duration per zip code across recreation areas and transport types.
#'
#' @param my_data A data frame containing recreation model results (e.g. 13 variables
#'   including recreation areas, transport types, zip codes, and expected visiting duration).
#'
#' @return An object of class \code{PivotTable} (\code{\link[pivottabler]{PivotTable}})
#'   containing the calculated percentages of expected visiting duration per zip code,
#'   allowing identification of origin areas with the highest duration shares.
#'
#' @importFrom pivottabler PivotTable PivotFilterOverrides
#' @export
percent_EVD_zip_code <- function(my_data) {
    pt5<- PivotTable$new()
    pt5$addData(my_data)
    pt5$addRowDataGroups("zip_code",outlineBefore = list(groupStyleDeclarations = list(color = "blue")),
                         outlineAfter = list(isEmpty = FALSE,
                                             mergeSpace = "dataGroupsOnly",
                                             caption = "Total ({value})",
                                             groupStyleDeclarations = list("font-style" = "italic")),
                         outlineTotal = list(groupStyleDeclarations = list(color = "blue"),
                                             cellStyleDeclarations = list("color" = "blue")))
    pt5$addRowDataGroups("transport_type_definition",header = "transport type", totalCaption = "All_transport_types",styleDeclaration = list("color" = "black","background-color" = "lightgreen"))
    pt5$addColumnDataGroups("recreation_areas",totalCaption = "All_recreation_areas", styleDeclarations = list("color" = "white", "font-weight" = "bold","background-color" = "blue"))
    pt5$defineCalculation(calculationName = "expected_visiting_duration", summariseExpression = "sum(expected_visiting_duration)", format = "%.2f", visible = TRUE)
    pt5$defineCalculation(calculationName = "Count_hgpID", summariseExpression = "n()", format = "%.2f", visible = TRUE)
    filterOverrides <- PivotFilterOverrides$new(pt5, keepOnlyFiltersFor = "zip_code")
    pt5$defineCalculation(calculationName = "zip_codes_Total", filters = filterOverrides,
                          summariseExpression = "sum(expected_visiting_duration)", caption = "zip_codes_Total", visible = TRUE)
    pt5$defineCalculation(calculationName = "N_of_evd_%_zipcode",
                          type = "calculation", basedOn = c("expected_visiting_duration","zip_codes_Total"),format = "%.2f",
                          calculationExpression = "values$expected_visiting_duration/values$zip_codes_Total * 100")
    pt5$evaluatePivot()
    return(pt5)
}
