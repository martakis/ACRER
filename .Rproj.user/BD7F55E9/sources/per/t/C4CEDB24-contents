#' Main script for the model ACRER.
#'
#' The model ACRER is a recreation model and simulates the number of persons per household and group types, the expected visiting duration as well as the number of visits from a zip code in a city to the destination area.
#'
#'   Several tables are provided in order to compute the abovementioned simulations:
#'     Table_2_RecrID_type_coordinates_prices: 33 records, recreation areas with their Lattitude and Longitude in degrees, their type (eg. sights,park,forest#'/nature area) and the likely entrance prices per recreation area.
#'   Table3_RecrID_activity_pattern: 221 records, Recreation areas ID, recreation areas with their activities pattern.
#'   Table_5_activities_definition_costs: 10 records, activity pattern definition and costs in ??? per activity
#'   Table_14_hgpID_groups_types: 52 records, household group ID, household types definition,group types definition,numer of adults,number of children,presence#' /absence of dog,number of groups and persons.
#'   Table_16_hgpID_min_max_budgets: 52 records, household group ID,household types budget per year,maximum budget (for recreation) per year and maximum budget (for recreation) per day.
#'   Table7_zip_hgpID_budget: 468 records, zip code,household types,group types#',year,numer of adults,number of children,presence/absence of dog,number of groups and persons,maximum budget (for recreation) per year and maximum budget (for recreation) per day.
#'   Table_9_zip_code_RecrID_distances: 891 records,Recreattion ID,zip code, transport type,distances in km, transport travel time in sec and transport speed in km/h.
#'   Table_8_hgpID_distance_costs: 124 records,transport type,household types,group types,maximum of total time (%) per transport type,wait time per full travel, minimum and maximum distance used per transport type,fixed and per km costs per transport type,per person and per km costs per transport type.
#'   Table_10_hgpId_activity_budget: 260 records,household types,group types
#'   ,maximum and minimum number of times per activity,maximum and minimum time to spend per activity.
#'   Time_segment_selected: 80 records,Recreation areas ID,number of visits  available per time segment and time available per time segment.
#'    zip_code_coordinates: 9 records, zip codes with their Lattitude and Longitude in degrees.


## find the working directory

WD <- getwd()
WD <- paste(WD, "/R/", sep = "")
WD1 <- paste(WD, "crosstab_E.R", sep = "")
WD3 <- paste(WD, "combinations.R", sep = "")
WD5 <- paste(WD, "Run_Queries.R", sep = "")
WD6 <- paste(WD, "expected_visit_duration.R", sep = "")
WD7 <- paste(WD, "InstallPackages.R", sep = "")

source(WD1, local = FALSE)
source(WD3, local = FALSE)
source(WD5, local = FALSE)
source(WD6, local = FALSE)
source(WD7, local = FALSE)

WD00 <- paste(WD, "NGroup0.R", sep = "")
WD11 <- paste(WD, "NGroup1.R", sep = "")
WD22 <- paste(WD, "NGroup2.R", sep = "")
WD33 <- paste(WD, "NGroup3.R", sep = "")
WD44 <- paste(WD, "NGroup4.R", sep = "")

source(WD00, local = FALSE)
source(WD11, local = FALSE)
source(WD22, local = FALSE)
source(WD33, local = FALSE)
source(WD44, local = FALSE)

# usage
required.packages <- c( "data.table", "DBI", "devtools", "dplyr",  "DT","flextable", "ggplot2", "intpoint","kableExtra", "knitr","openxlsx", "pivottabler","htmlwidgets","htmltools", "pander", "pracma", "quadprog", "rpivotTable","reshape2", "rmarkdown","rstudioapi", "rmarkdown", "rJava", "rjson", "roxygen2","RSQLite", "svDialogs", "sqldf", "tinytex", "tools")
InstallPackages(required.packages)


# Get the required system name: Windows (for Windows 7,8 and 10) or Darwin (for OS X
# 10.12 and above)
System_information <- Sys.info()["sysname"]

library(rJava)
library(rjson)
library(dplyr)
library(DBI)
library(RSQLite)
library(ggplot2)
library(reshape2)
library(quadprog)
library(intpoint)
library(rpivotTable)
library(devtools)
library(roxygen2)
library(tools)
library(data.table)
library(openxlsx)
library(pivottabler)
library(htmlwidgets)
library(data.table)
library(rmarkdown)
library(htmltools)
library(pander)
library(pracma)
library(knitr)
library(DT)
library(kableExtra)
library(sqldf)
library(svDialogs)
library(rstudioapi)
library(tinytex)
library(flextable)

## Get from the database the required basic tables or the Final_table

if (System_information == "Windows") {

    WD <- getwd()
    WD2 <- paste(WD, "/Wageningen_Basic_Tables.sqlite", sep = "")
    conn <- dbConnect(RSQLite::SQLite(), WD2)
    tables_list <- dbListTables(conn)


    # check if the Final_table, data, ActInfo, Time_info, zip_code_coordinates and
    # Table_2_RecrID_type_coordinates_prices are present

    if ((tables_list[4] == "Final_table") & (tables_list[19] == "data") & (tables_list[1] ==
        "ActInfo") & (tables_list[17] == "TimeInfo") & (tables_list[26] == "zip_code_coordinates") &
        (tables_list[13] == "Table_2_RecrID_type_coordinates_prices") & (tables_list[3] ==
        "Basic_TableB")) {

        Final_table <- dbGetQuery(conn, "SELECT * FROM Final_table")
        data <- dbGetQuery(conn, "SELECT * FROM data")
        ActInfo <- dbGetQuery(conn, "SELECT * FROM ActInfo")
        TimeInfo <- dbGetQuery(conn, "SELECT * FROM TimeInfo")
        zip_code_coordinates <- dbGetQuery(conn, "SELECT * FROM zip_code_coordinates")
        Table_2_RecrID_type_coordinates_prices <- dbGetQuery(conn, "SELECT * FROM Table_2_RecrID_type_coordinates_prices")
        Basic_TableB <- dbGetQuery(conn, "SELECT * FROM Basic_TableB")
        logical_tables = c("TRUE")
    } else {

        # . 1 'Table_2_RecrID_type_coordinates_prices'. Recreation area definitions. 33
        # records Field Names: RecrID, recreation_area, Latitude_in_degrees,
        # Longitude_in_degrees, type_of_recreation_areas,
        # type_of_recreation_areas_definition, type_of_recreation_areas_definition,
        # price_of_sights_or_park_per_child, price_of_parking Primary key: RecrID

        Table_2_RecrID_type_coordinates_prices <- dbGetQuery(conn, "SELECT * FROM Table_2_RecrID_type_coordinates_prices")
        # Table_2_RecrID_type_coordinates_prices <- sqlFetch(conn,
        # 'Table_2_RecrID_type_coordinates_prices')

        # . 2 'Table3_RecrID_activity_pattern'. Type of activities per recreation area.  221
        # records Field Names: RecrID, activity_pattern, recreation_areas,
        # type_of_recreation_areas, type_of_recreation_areas_definition Primary key: RecrID
        # Secondary key: activity_pattern

        Table3_RecrID_activity_pattern <- dbGetQuery(conn, "SELECT * FROM Table3_RecrID_activity_pattern")
        # Table3_RecrID_activity_pattern <- sqlFetch(conn, 'Table3_RecrID_activity_pattern')

        # . 3 'Table_5_activities_definition_costs'. Activities pattern definitions and
        # costs per activity 10 records Field Names: activity_pattern,
        # activity_pattern_definition, fixed_costs_per_activity,
        # costs_per_hour_per_activity,
        # costs_per_person_per_activity,costs_per_hour_and_person_per_activity Primary key:
        # activity_pattern

        Table_5_activities_definition_costs <- dbGetQuery(conn, "SELECT * FROM Table_5_activities_definition_costs")
        # Table_5_activities_definition_costs <- sqlFetch(conn,
        # 'Table_5_activities_definition_costs')

        # . 4 'Table_14_hgpID_groups_types'. Houshold_types with their corresponding
        # groups_types (recreation person) 52 records Field Names: hgpID, household_types,
        # household_types_definition, group_types, group_types_definition, number_of_adults,
        # age_of_adults, number_of_children, age_of_the_child, dog, number_of_groups,
        # number_of_persons Primary key: hgpID

        Table_14_hgpID_groups_types <- dbGetQuery(conn, "SELECT * FROM Table_14_hgpID_groups_types")
        # Table_14_hgpID_groups_types <- sqlFetch(conn, 'Table_14_hgpID_groups_types')

        # . 5 'Table_16_hgpID_min_max_budgets'. househod_types-groups_types (hgpID) with
        # their corresponding budget per year and the amount of money paid out per day and
        # year 52 records Field Names: hgpID, household_types_budget_per_year,
        # number_of_groups, number_of_persons, maximum_budget_per_day,
        # maximum_budget_per_year, Year Primary key: hgpID

        Table_16_hgpID_min_max_budgets <- dbGetQuery(conn, "SELECT * FROM Table_16_hgpID_min_max_budgets")
        # Table_16_hgpID_min_max_budgets <- sqlFetch(conn, 'Table_16_hgpID_min_max_budgets')

        # . 6 'Table7_zip_hgpID_budget' Combinations of Houshold_types with their
        # corresponding groups_types (recreation person) per zip code 450 records Field
        # Names: hgpID, ID_origin_area, zip_code, household_types,
        # household_types_definition, group_types, group_types_definition, Year,
        # number_of_adults, number_of_children, number_of_groups, number_of_persons,
        # maximum_budget_per_day, maximum_budget_per_year Primary key: hgpID Secondary key:
        # ID_origin_area

        Table7_zip_hgpID_budget <- dbGetQuery(conn, "SELECT * FROM Table7_zip_hgpID_budget")
        # Table7_zip_hgpID_budget <- sqlFetch(conn, 'Table7_zip_hgpID_budget')

        # . 7 'Table_9_zip_code_RecrID_distances' Combinations of zip codes, recreation
        # areas and their corresponding transport types distances (in km) from zip codes to
        # recreation areas, transport travel time in sec and transport speed inkm/hour. 891
        # records Field Names: ID_origin_area, RecrID, transport_type_ID, zip_code,
        # recreation_areas, distances_km, transport_travel_time_sec,
        # transport_speed_km_per_hour Primary key: ID_origin_area Secondary keys: RecrID,
        # transport_type_ID

        Table_9_zip_code_RecrID_distances <- dbGetQuery(conn, "SELECT * FROM Table_9_zip_code_RecrID_distances")
        # Table_9_zip_code_RecrID_distances <- sqlFetch(conn,
        # 'Table_9_zip_code_RecrID_distances')

        # . 8 'Table_8_hgpID_distance_costs'. Combinations of household_types, group_types
        # with their corresponding transport type, the percentage of the maximum time of the
        # total time, the wait time per full travel, the minimum and maximum distance per
        # transport type, the speed per transport type and finaly the correspondings costs
        # per transport type, fixed costs, per km costs per person costs and per km and
        # persons costs.  124 records Field Names: hgpID, transport_type_ID,
        # household_types, group_types, household_types_definition, group_types_definition,
        # maximum_of_total_time, wait_time_per_full_travel, minimum_distance_used,
        # maximum_distance_used, speed_of_transport_type, fixed_costs_per_transport_type,
        # per_km_costs_per_transport_type, per_person_costs_per_transport_type,
        # per_km_en_per_person_costs_per_transport_type Primary key: hgpID Secondary key:
        # transport_type_ID

        Table_8_hgpID_distance_costs <- dbGetQuery(conn, "SELECT * FROM Table_8_hgpID_distance_costs")
        # Table_8_hgpID_distance_costs <- sqlFetch(conn, 'Table_8_hgpID_distance_costs')

        # . 9 'Table_10_hgpId_activity_budget'. Combinations of household_types, group_types
        # with their corresponding activity pattern, the minimum and maximum number of times
        # per activity and finaly the minimum and maximum amount of time to spend per
        # activity. 260 records Field Names: hgpID, activity_pattern, activity_pattern,
        # household_types, household_types_definition, group_types, group_types_definition,
        # minimum_number_of_times_per_activity, maximum_number_of_times_per_activity,
        # to_spend_minimum_time_per_activity, to_spend_maximum_time_per_activity Primary
        # key: hgpID Secondary key: activity_pattern

        Table_10_hgpID_activity_budget <- dbGetQuery(conn, "SELECT * FROM Table_10_hgpID_activity_budget")
        # Table_10_hgpID_activity_budget <- sqlFetch(conn, 'Table_10_hgpID_activity_budget')

        # . 10 'Time_segment_selected. 4 time segments are chosen with their corresponding
        # time available per segment (in min) and the number of visits per time segment 79
        # records' Field Names: RecrID, Time_segment, number_available_per_time_segment,
        # Time_available_per_time_segment Primary key: RecrID Secondary key: Time_segment

        Time_segment_selected <- dbGetQuery(conn, "SELECT * FROM Time_segment_selected")
        # Time_segment_selected <- sqlFetch(conn, 'Time_segment_selected')

        # . 11 'zip_code_coordinates'. The zip codes coordinates 9 records Field Names:
        # zip_code, Latitude, Longitude Primary key: zip_code

        Table_15_Transport_Type_definitions <- dbGetQuery(conn, "SELECT * FROM Table_15_Transport_Type_definitions")
        # Field Names:
        # transport_type_ID,transport_type_definitions,transport_speed_km_per_hour,minimum_distance_per_transport_used,maximum_distance_per_transport_used
        # Primary key: transport_type_ID

        zip_code_coordinates <- dbGetQuery(conn, "SELECT * FROM zip_code_coordinates")
        # zip_code_coordinates <- sqlFetch(conn, 'zip_code_coordinates')
        logical_tables = c("FALSE")
    }
} else {

    WD <- getwd()
    WD2 <- paste(WD, "/Wageningen_Basic_Tables.sqlite", sep = "")
    conn <- dbConnect(SQLite(), WD2)
    tables_list <- dbListTables(conn)

    if ((tables_list[4] == "Final_table") & (tables_list[19] == "data") & (tables_list[1] ==
        "ActInfo") & (tables_list[17] == "TimeInfo") & (tables_list[26] == "zip_code_coordinates") &
        (tables_list[13] == "Table_2_RecrID_type_coordinates_prices") & (tables_list[3] ==
        "Basic_TableB")) {

        Final_table <- dbGetQuery(conn, "SELECT * FROM Final_table")
        data <- dbGetQuery(conn, "SELECT * FROM data")
        ActInfo <- dbGetQuery(conn, "SELECT * FROM ActInfo")
        TimeInfo <- dbGetQuery(conn, "SELECT * FROM TimeInfo")
        zip_code_coordinates <- dbGetQuery(conn, "SELECT * FROM zip_code_coordinates")
        Table_2_RecrID_type_coordinates_prices <- dbGetQuery(conn, "SELECT * FROM Table_2_RecrID_type_coordinates_prices")
        Basic_TableB <- dbGetQuery(conn, "SELECT * FROM Basic_TableB")
        logical_tables = c("TRUE")
    } else {

        # . 1 'Table_2_RecrID_type_coordinates_prices'. Recreation area definitions. 33
        # records Field Names: RecrID, recreation_area, Latitude_in_degrees,
        # Longitude_in_degrees, type_of_recreation_areas,
        # type_of_recreation_areas_definition, type_of_recreation_areas_definition,
        # price_of_sights_or_park_per_child, price_of_parking Primary key: RecrID

        Table_2_RecrID_type_coordinates_prices <- dbGetQuery(conn, "SELECT * FROM Table_2_RecrID_type_coordinates_prices")

        # . 2 'Table3_RecrID_activity_pattern'. Type of activities per recreation area.  221
        # records Field Names: RecrID, activity_pattern, recreation_areas,
        # type_of_recreation_areas, type_of_recreation_areas_definition Primary key: RecrID
        # Secondary key: activity_pattern

        Table3_RecrID_activity_pattern <- dbGetQuery(conn, "SELECT * FROM Table3_RecrID_activity_pattern")

        # . 3 'Table_5_activities_definition_costs'. Activities pattern definitions and
        # costs per activity 10 records Field Names: activity_pattern,
        # activity_pattern_definition, fixed_costs_per_activity,
        # costs_per_hour_per_activity,
        # costs_per_person_per_activity,costs_per_hour_and_person_per_activity Primary key:
        # activity_pattern

        Table_5_activities_definition_costs <- dbGetQuery(conn, "SELECT * FROM Table_5_activities_definition_costs")

        # . 4 'Table_14_hgpID_groups_types'. Houshold_types with their corresponding
        # groups_types (recreation person(s) with or without a dog) 52 records' Field Names:
        # hgpID, household_types, household_types_definition, group_types,
        # group_types_definition, number_of_adults, age_of_adults, number_of_children,
        # age_of_the_child, dog, number_of_groups, number_of_persons Primary key: hgpID

        Table_14_hgpID_groups_types <- dbGetQuery(conn, "SELECT * FROM Table_14_hgpID_groups_types")

        # . 5 'Table_16_hgpID_min_max_budgets. househod_types-groups_types (hgpID) with
        # their corresponding budget per year and the amount of money paid out per day and
        # year 52 records' Field Names: hgpID, household_types_budget_per_year,
        # number_of_groups, number_of_persons, maximum_budget_per_day,
        # maximum_budget_per_year, Year Primary key: hgpID

        Table_16_hgpID_min_max_budgets <- dbGetQuery(conn, "SELECT * FROM Table_16_hgpID_min_max_budgets")

        # . 6 'Table7_zip_hgpID_budget' Combinations of Houshold_types with their
        # corresponding groups_types (recreation person) per zip code 468 records Field
        # Names: hgpID, ID_origin_area, zip_code, household_types,
        # household_types_definition, group_types, group_types_definition, Year,
        # number_of_adults, number_of_children, number_of_groups, number_of_persons,
        # maximum_budget_per_day, maximum_budget_per_year Primary key: hgpID Secondary key:
        # ID_origin_area

        Table7_zip_hgpID_budget <- dbGetQuery(conn, "SELECT * FROM Table7_zip_hgpID_budget")

        # . 7 'Table_9_zip_code_RecrID_distances' Combinations of zip codes, recreation
        # areas and their corresponding transport types distances (in km) from zip codes to
        # recreation areas, transport travel time in sec and transport speed inkm/hour. 891
        # records Field Names: ID_origin_area, RecrID, transport_type_ID, zip_code,
        # recreation_areas, distances_km, transport_travel_time_sec,
        # transport_speed_km_per_hour Primary key: ID_origin_area Secondary keys: RecrID,
        # transport_type_ID

        Table_9_zip_code_RecrID_distances <- dbGetQuery(conn, "SELECT * FROM Table_9_zip_code_RecrID_distances")

        # . 8 'Table_8_hgpID_distance_costs'. Combinations of household_types, group_types
        # with their corresponding transport type, the percentage of the maximum time of the
        # total time, the wait time per full travel, the minimum and maximum distance per
        # transport type, the speed per transport type and finaly the correspondings costs
        # per transport type, fixed costs, per km costs per person costs and per km and
        # persons costs.  124 records Field Names: hgpID, transport_type_ID,
        # household_types, group_types, household_types_definition, group_types_definition,
        # maximum_of_total_time, wait_time_per_full_travel, minimum_distance_used,
        # maximum_distance_used, speed_of_transport_type, fixed_costs_per_transport_type,
        # per_km_costs_per_transport_type, per_person_costs_per_transport_type,
        # per_km_en_per_person_costs_per_transport_type Primary key: hgpID Secondary key:
        # transport_type_ID

        Table_8_hgpID_distance_costs <- dbGetQuery(conn, "SELECT * FROM Table_8_hgpID_distance_costs")

        # . 9 'Table_10_hgpId_activity_budget'. Combinations of household_types, group_types
        # with their corresponding activity pattern, the minimum and maximum number of times
        # per activity and finaly the minimum and maximum amount of time to spend per
        # activity. 260 records Field Names: hgpID, activity_pattern, activity_pattern,
        # household_types, household_types_definition, group_types, group_types_definition,
        # minimum_number_of_times_per_activity, maximum_number_of_times_per_activity,
        # to_spend_minimum_time_per_activity, to_spend_maximum_time_per_activity Primary
        # key: hgpID Secondary key: activity_pattern

        Table_10_hgpID_activity_budget <- dbGetQuery(conn, "SELECT * FROM Table_10_hgpID_activity_budget")

        # . 10 'Time_segment_selected. 4 time segments are chosen with their corresponding
        # time available per segment (in min) and the number of visits per time segment 80
        # records' Field Names: RecrID, Time_segment,
        # number_available_per_time_segment,Time_available_per_time_segment Primary key:
        # RecrID Secondary key: Time_segment

        Table_15_Transport_Type_definitions <- dbGetQuery(conn, "SELECT * FROM Table_15_Transport_Type_definitions")

        # Field Names:
        # transport_type_ID,transport_type_definitions,transport_speed_km_per_hour,minimum_distance_per_transport_used,maximum_distance_per_transport_used
        # Primary key: transport_type_ID

        Time_segment_selected <- dbGetQuery(conn, "SELECT * FROM Time_segment_selected")


        # . 11 'zip_code_coordinates'. The zip codes coordinates 9 records Field Names:
        # zip_code, Latitude, Longitude Primary key: zip_code

        zip_code_coordinates <- dbGetQuery(conn, "SELECT * FROM zip_code_coordinates")
        logical_tables = c("FALSE")
    }
}



# ' Display the Basic tables

if (logical_tables == FALSE) {
    dataset1 <- list(Table_2_RecrID_type_coordinates_prices, Table3_RecrID_activity_pattern,
        Table_5_activities_definition_costs, Table_14_hgpID_groups_types, Table_16_hgpID_min_max_budgets,
        Table7_zip_hgpID_budget, Table_9_zip_code_RecrID_distances, Table_8_hgpID_distance_costs,
        Table_10_hgpID_activity_budget, Time_segment_selected, Table_15_Transport_Type_definitions,
        zip_code_coordinates)
    saveRDS(dataset1,"data/dataset1.Rds")
    # The Basic Tables can be displayed by Running the presentation of the
    # Basic_Tables.Rmd, in the R markdown file
    rm(dataset1)
    # Run the different queries from the Basic tables (dataset1 list of Basic tables) in
    # order to obtain the Finaltable by using the function 'Run_Queries'
    dataset1 <- readRDS("data/dataset1.rds")
    list5 <- Run_Queries(dataset1)

    # After running the queries we get the Final_table, data, ActInfo and Time_Info
    # tables from the list5 and we save them in the file:dataset2.Rdata The SQL-created
    # Tables can be displayed by Run the presentation of the SQL_created_tables.Rmd, R
    # markdown file


    Final_table <- list5[[1]]
    data <- list5[[2]]
    ActInfo <- list5[[3]]
    TimeInfo <- list5[[4]]
    Basic_TableB <- list5[[5]]
}

dataset2 <- list(Final_table, data, ActInfo, TimeInfo)
saveRDS(dataset2,"data/dataset2.Rds")
rm(dataset2)




# use the R Utils Package for head and str use the base package for the Object
# Classes


head(data, 10)
class(data)

# The data set (data) is defined by the combinations of the variables: RecrID,
# activity pattern, Time_segment and transport_type_ID The transport_type_ ID
# variable is not important in the analysis.  In the analysis only the unique
# combinations of the variables: RecrID, activity_patern and Time_segment are
# needed.  However, the shorter vector used by these three variables being
# determined to expand again is the design vector of these three variables needed
# cdata: (487 x 4) (see Excel workbook: Wag_documentation_English,
# sheet:crostab_procedure) The presence of the RecrID on the combinations of
# activity_pattern and Time_segment (34 combinations) are given by the variable
# groupsRecr_at by using the function crosstab.  These groups we further used for
# the quadratic programming optimisation procedure

cdata <- data %>% group_by(RecrID, activity_pattern, Time_segment) %>% summarise(Counts_transport_type_ID = n())
Rmatrix <- as.matrix(x = cdata[, 1], dimnames = list(NULL, "RecrID"))
Cmatrix <- as.matrix(x = cdata[, 2:3], dimnames = list(NULL, "activity_pattern", "Time_segment"))
C1 <- vector("numeric")
# the function crosstab construct a matrix of 33 rows (the number of RecrID) and 34
# columns (the combinations of activity_pattern and Time_segment) with the
# present/absence of the RecrID per combination where the sum of each row is greater
# than zero After that this function computes the groups with the same activities in
# the same time segments per destination area (see Wag_documentation_English.xlsx,
# Excel sheet: crostab_procedure)

list10 <- crosstab_E(Rmatrix, Cmatrix, C1)
f9 <- as.matrix(list10[[4]])
cdata <- as.matrix(cdata)
cdata <- cbind(cdata, f9)
colnames(cdata)[colnames(cdata) == ""] <- "groupsRecr_at"
data1 <- cbind(cdata[, 1], cdata[, 5])
data1 <- as.data.frame(data1)
names(data1)[1:2] <- c("RecrID", "groupsRecr_at")
df <- data1

# use the library dpryl for the count/tally observations by group

z <- df %>% group_by(RecrID, groupsRecr_at) %>% summarize_all(n_distinct)
z1 <- tally(group_by(z, groupsRecr_at), sort = FALSE)
names(z1)[2:2] <- c("number of locations")
z1 <- data.frame(z1)
data1 <- cbind(cdata[, 5], cdata[, 2], cdata[, 3])
data1 <- as.data.frame(data1)
names(data1)[1:3] <- c("groupsRecr_at", "activity_pattern", "Time_segment")
data2 <- group_by(data1, groupsRecr_at, activity_pattern, Time_segment)
data2 <- tally(data2, sort = TRUE)
names(data2)[4:4] <- c("number_of_locations")
data2 <- data.frame(data2)
data2 <- data2[order(data2$groupsRecr_at, data2$activity_pattern, data2$Time_segment),
    ]
Rmatrix <- as.matrix(x = data2[, 1:2], dimnames = list(NULL, "groupsRecr_at", "activity_pattern"))
Cmatrix <- as.matrix(x = data2[, 3], dimnames = list(NULL, "Time_segment"))
# The presence of the RecrID and activity_pattern on the Time_segment (4
# combinations) are given by the variable groupsRecr_act_ts by using the function
# crosstab (see Wag_documentation_English.xlsx, Excel sheet: timeMax) These groups
# we further used for the linear programming optimisation procedure

list1 <- crosstab_E(Rmatrix, Cmatrix, C1)
groupsRecr_act_ts <- as.matrix(list1[[4]])
Table <- as.matrix(list1[[3]])

# data2 groupsRecr_at, activity_pattern, TimeSegment, number_of_locations,
# groupsRecr_act_ts

data2 <- cbind(as.matrix(data2), groupsRecr_act_ts)
data2 <- as.data.frame(data2)
names(data2)[5] <- c("groupsRecr_act_ts")

# select groupsRecr_at(1), activity_pattern(2), groupsRecr_act_ts(5),
# first(NumberOfLocations(4)) from data2 groupby
# groupsRecr_at(1),activity_pattern(2),groupsRecr_act_ts(5) use the library dpryl

data3a <- group_by(data2, groupsRecr_at, activity_pattern, groupsRecr_act_ts, number_of_locations)
data3a <- dplyr::select(data3a, -(Time_segment))
data3b <- summarize_all(data3a, n_distinct)
Gdata3 <- group_by(data3b, groupsRecr_at, number_of_locations)
Gdata3 <- dplyr::select(Gdata3, -(activity_pattern), -(groupsRecr_act_ts))
Gdata3 <- summarise_all(Gdata3, n_distinct)
totaal_aantal_locaties = sum(Gdata3[, 2])

# TimeAvail == TimeInfo

TimeAvail <- TimeInfo
TimeAvail <- TimeAvail[, -1]
fgroupsRecr_act_ts <- factor(as.matrix(data3b[, 3]))
# the number of levels of the factor groupsRecr_act_ts
levels3 <- rev(as.numeric(levels(fgroupsRecr_act_ts)))

# Construction of the design matrices: matrix design2 with the use of the
# model.matrix function from the stats package Insert - 1 in the formula:
# ~factivity_pattern to suppress the intercept (see Wag_documentation_English.xlsx,
# Excel sheet: timeMax and quadprog)

factivity_pattern <- factor(as.matrix(data3b[, 2]))
design2 <- model.matrix(~factivity_pattern - 1)
design2 <- as.data.frame(design2)
design2 <- t(design2)

# Construction of the design matrices: matrix design3 with the use of the
# model.matrix function from the stats package Insert - 1 inthe formula:
# ~fgroupsRecr_act_ts to suppress the intercept

design3 <- model.matrix(~fgroupsRecr_act_ts - 1)
design3 <- as.data.frame(design3)

design3 <- t(design3)

# constraints on available time c3*design3<=timeMax

# select the hulp combination matrix c3 (16 x 4) for the construction of the timeMax
# vector

# Intialization

Matrix_timeMax <- matrix(ncol = 0, nrow = 0)
zcount <- 0
number <- vector(mode = "logical", length = 0)
number_of_time_segments <- length(levels3)
number_in_square <- number_of_time_segments * number_of_time_segments
total_numbers <- number_in_square * number_of_time_segments
c3 <- matrix(rep(0, total_numbers), nrow = number_in_square, ncol = number_of_time_segments)
timeMax <- matrix(c(rep(0, number_in_square)), ncol = 1, nrow = number_in_square)
TemptimeMax <- matrix(c(rep(0, number_in_square)), ncol = 1, nrow = number_in_square)
TempMatrix <- matrix(c(rep(0, number_in_square)), nrow = number_in_square)
icount <- matrix(c(rep(1, number_of_time_segments)), ncol = 1)
jcount <- matrix(c(rep(1, number_of_time_segments)), ncol = 1)


while (zcount < 5001) {
    if (number_of_time_segments == 2) {
        list50 <- combinations(number_of_time_segments)
        if (length(list50[[1]] != 0)) {
            c3 <- list50[[1]]
            TempMatrix <- cbind(TempMatrix, c3)
            if (length(number) == 0) {
                number <- icount
                icount <- icount + jcount
            } else {
                number <- rbind(number, icount)
                icount <- icount + jcount
            }
        } else {
            next
        }
    } else if (number_of_time_segments == 3) {
        list50 <- combinations(number_of_time_segments)
        if (length(list50[[1]] != 0)) {
            c3 <- list50[[1]]
            TempMatrix <- cbind(TempMatrix, c3)
            if (length(number) == 0) {
                number <- icount
                icount <- icount + jcount
            } else {
                number <- rbind(number, icount)
                icount <- icount + jcount
            }
        } else {
            next
        }
    } else {
        list50 <- combinations(number_of_time_segments)
        if (length(list50[[1]] != 0)) {
            c3 <- list50[[1]]
            TempMatrix <- cbind(TempMatrix, c3)
            if (length(number) == 0) {
                number <- icount
                icount <- icount + jcount
            } else {
                number <- rbind(number, icount)
                icount <- icount + jcount
            }
        } else {
            next
        }
    }

    data6 <- cbind(Table, data3b[, 3])
    data6 <- data6[, c(5, 4, 3, 2, 1)]
    data7 <- group_by(data6, groupsRecr_act_ts)
    data7 <- unique(data7)
    data7 <- as.matrix(data7)
    colnames(data7) <- c()
    colnames(c3) <- c()
    data7 <- apply(data7, 2, as.numeric)
    data8 <- data7[, 2:dim(data7)[2]]
    TimeAvail <- as.matrix(TimeAvail)
    TimeAvail <- apply(TimeAvail, 2, as.numeric)
    size1 <- dim(Matrix_timeMax)
    # construction of timeMax vector

    p <- c3 %*% data8
    p <- p > 0

    tMT <- p %*% TimeAvail
    sumtMT <- sum(tMT)
    if (nrow(tMT) == number_in_square) {
        if ((size1[1] == 0) & (size1[2] == 0)) {
            Matrix_timeMax <- tMT
            zcount = zcount + 1
        } else {
            Matrix_timeMax <- cbind(Matrix_timeMax, tMT)
            zcount = zcount + 1
        }
    }
}

TempMatrix <- TempMatrix[, -1]
TempMatrix <- t(TempMatrix)
Matrix_timeMax <- t(Matrix_timeMax)
SizeMatrix_timeMax <- dim(Matrix_timeMax)
numbertimeMax <- (1:SizeMatrix_timeMax[[1]])
Sum_timeMax <- rowSums(Matrix_timeMax)
Matrix_timeMax <- cbind(Matrix_timeMax, Sum_timeMax)
Matrix_timeMax <- cbind(numbertimeMax, Matrix_timeMax)
Matrix_timeMax <- data.frame(Matrix_timeMax)
Matrix_timeMax <- Matrix_timeMax[order(Matrix_timeMax[number_in_square + 2]), ]

TempMatrix <- cbind(number, TempMatrix)
numbertimeMax <- Matrix_timeMax[1, 1]


c3_matrix <- filter(as.data.frame(TempMatrix), number == Matrix_timeMax[1, 1])
c3_matrix <- c3_matrix[, -1]
c3_matrix <- as.matrix(t(c3_matrix))
c3 <- c3_matrix
timeMax_matrix <- filter(as.data.frame(Matrix_timeMax), numbertimeMax == Matrix_timeMax[1,
    1])
timeMax_matrix <- timeMax_matrix[, -(number_in_square + 2)]
timeMax_matrix <- timeMax_matrix[, -1]

p <- c3 %*% data8
plog <- p > 0

timeMax <- t(timeMax_matrix)

colnames(timeMax) <- c()
timeMax <- as.matrix(timeMax)

ActMax <- select(ActInfo, 2)
ActMax <- as.matrix(ActMax)
colnames(ActMax) <- c()

# constraints on activity design2*n<=ActMax and -design2*n<=0*ActMax

ZActMax <- as.matrix(0 * ActMax)  # 0,0,0...

# coefficients of the linear constrains

lin_cond_max <- rbind(timeMax, ActMax, ZActMax)
c3_design3 <- c3 %*% design3
lin_mat <- rbind(c3_design3, design2, -design2)
lin_mat <- as.matrix(lin_mat)
rownames(lin_mat) <- c()

# construction of the Hessian matrix: HessianMatrix

# as much as possible spread of the destinations per activity Ni is a vector (10 x
# 1) constructed by design2 (10 x 51) * data3b(number_of_locations) (51 x 1)
Ni <- design2 %*% as.matrix(data3b[, 4])
rownames(Ni) <- c()

# number of the number of locations vector loc: (51 x 1) reciprocal of of the number
# of locations vector loc_1: (51 x 1)
loc <- as.matrix(data3b[, 4])  # 51
loc_1 <- 1/loc  # eg. 1/5 = 0.2
m1 <- as.matrix(diag(c(loc_1)))
ncoldesign2 <- dim(design2)[2]
m2 <- as.matrix(t(-design2) %*% diag(c(1/Ni)) %*% design2)
design0 <- as.matrix(ones(ncoldesign2, 1))

# as much as possible spread of the visits per activity_pattern_type construction of
# the design matrix: design1 (10 x 51)

fgroupsRecr_at <- factor(as.matrix(data3b[, 1]))
design1 <- model.matrix(~fgroupsRecr_at - 1)
rownames(design1) <- c()
design1 <- t(design1)
sumdesign1 <- rowSums(design1)
sumdesign1 <- as.matrix(sumdesign1)
colnames(sumdesign1) <- c()
design1 <- design1[, ]/sumdesign1[, ]
rownames(design1) <- c()
# the proportion of the number of locations per level groupsRecr_at
Li <- design1 %*% as.matrix(data3b[, 4])
design1 <- design1 > 0
m3 <- as.matrix(t(design1) %*% diag(c(1/Li)) %*% design1)
nTot <- sum(Li)
# number of destination visits
m4 <- as.matrix(-design0 %*% (1/nTot) %*% t(design0))

# percentage of the maximum visits

sumMi <- sum(ActMax)
SizeActMax <- length(ActMax)
dia5 <- diag(c(ones(SizeActMax, 1)))/sumMi/sumMi
m5 <- as.matrix(t(design2) %*% dia5 %*% design2)
g11 <- t((ActMax) %*% (1/(sumMi^2)))
l1 <- 2 %*% g11 %*% design2
m6 <- as.matrix(design0 %*% g11 %*% ActMax %*% t(design0))

## maximum visits

maxVisits <- sumMi
m7 <- as.matrix(design0 %*% t(design0))
# l2 = (1 x 1) x (51 x 1) = 51 x 1
l2 <- -2 * sumMi * design0
c1 <- sumMi^2

# Quadratic optimisation: implimentation of the dual method of Goldfarb and Idnani
# (1982-83) for solving quadratic problems of the form: min(-dvec'*x +
# 1/2*(x'Dmat*x) such that Amat'*x >= bvec the matrix Amat and the vector bvec are
# the coefficients of the linear constraints

# HessianMatrix (Dmat) is symmetric (51 x 51)

HessianMatrix <- as.matrix((m1 + m3 + m5 + m6 + m7)/2)

Dmat <- HessianMatrix  # Hessian matrix to be minimized (51 x 51)
sizeH <- dim(Dmat)
Adiag <- diag(-1, sizeH[2])  # diagonal matrix of -1 for the matrix extension Amat (51 x 51)

f10 <- as.matrix((t(l1)) * 1/2 + l2)
f10L <- length(f10)
bvec0 <- c(rep(0, f10L))  # vector of length 51 with zeros for the extension of the vector of linear constraints

# vector dvec is the set of coefficients of the quadratic objective function to be
# minimized
dvec <- -f10
dvec <- t(dvec)  # matrix (1 x 51)
lb <- as.matrix(0 * t(design0))  ## b
# the matrix Amat and the vector bvec are the coefficients of the linear constraints
Amat <- lin_mat  # matrix Amat contains the coefficients of the linear constrains (35 x 51)
# extension of the matrix Amat (matrix defining the constraints + transpose of the
# diagonal matrix)

Amat <- rbind(Amat, Adiag)  # matrix Amat (35+51=86 x 51)
Amat <- -Amat  # matrix Amat = -Amat
Amat <- t(Amat)  # transpose -Amat  (51 x 86)

## The vector bvec
bvec <- as.vector(lin_cond_max, mode = "any")  ## vector of the coefficients of the linear constraints
bvec <- c(bvec, bvec0)  ## Extension of the vector with the vector bvec0 (51 x 1) with zeros (86 x 1)
bvec <- -bvec  # bvec is -bvec

# determine the distribution over time segments by using solve.QP
qp <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec)
n <- qp$solution
value <- qp$value
uncontrained_solution <- qp$uncontrained.solution
iterations <- qp$iterations
Lagrangian <- qp$Langrangian
iact <- qp$iact
n <- matrix(n)


groupsRecr_act_ts_presence_absence <- list1[[3]]
ndim <- dim(groupsRecr_act_ts_presence_absence)
# number of rows = 48 * 4 = 192
numberofrows <- ndim[1] * ndim[2]
d1 <- matrix(t(groupsRecr_act_ts_presence_absence), nrow = numberofrows, ncol = 1)
h1 <- data.frame(x = 1:ndim[1])  # 1:48
h2 <- data.frame(y = 1:ndim[2])  # 1:4
# function expand.grid: create a data frame from all combinations of the supplied
# vectors or factors
d2 <- expand.grid(t(h2), t(h1))  # 4 x 48 = 192
d3 <- cbind(d2, d1)
d3 <- cbind(d3[2], d3[1], d1)
hd3 <- tbl_df(d3)
amatrix <- hd3
class(amatrix)
amatrix <- filter(amatrix, d1 > 0)
dimamatrix <- dim(amatrix)
fa <- factor(as.matrix(amatrix[, 1]))
fb <- factor(as.matrix(amatrix[, 2]))
z11 <- model.matrix(~fa - 1)
z11 <- data.frame(z11)
z11 <- t(z11)
z12 <- model.matrix(~fb - 1)
z12 <- data.frame(z12)
z12 <- t(as.matrix(z12))
cvec <- c(rep(1, dimamatrix[1]))  # array of ones (135 x 1)
linm <- rbind(z11, z12)  # Aeq size: 55 x 127
linmax <- as.matrix(c(n, TimeAvail[1], TimeAvail[2], TimeAvail[3], TimeAvail[4]))  # size: 52 x 1
# (n + (15,10,5,3)) (52 x 1)

# solution of qp plus TimeAvail

bvecL <- linmax

# Type of problem: 1 = maximization problem c = cvec coeeficients of the objective
# function bM = bvecL The vector corresponding to the right hand constants (with > =
# ) M = linm the coefficient of the problem constraints matrix (with > =)

solve5 <- interior_point(t = -1, c = cvec, bM = bvecL, M = linm, e = 1e-08)

sol5 <- solve5[2]
sol5 <- data.frame(sol5)



Rmatrix <- as.matrix(x = amatrix[, 1])
Cmatrix <- as.matrix(x = amatrix[, 2])
C1 <- as.matrix(sol5)
list15 <- crosstab_E(Rmatrix, Cmatrix, C1)
nt <- list15[[3]]
colnames(nt)[1:4] = c(1, 2, 3, 4)
nt1 <- t(nt)

result1a <- melt(nt1, measured = nt)
colnames(result1a) <- c("Time_segment", "RecrID", "number_of_visits")

rep.row <- function(x, n) {
    matrix(rep(x, each = n), nrow = n)
}
rep.col <- function(x, n) {
    matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}

x3 <- c(1:dim(data3b)[1])
data9 <- as.data.frame(matrix(c(rep(0, dim(data3b)[1])), nrow = dim(data3b)[1]))
mm2 <- matrix(data = NA, ncol = dim(data3b)[2])
for (i in x3) {
    mm2 <- as.data.frame(rep.row(data3b[i, ], dim(data3b)[2]))
    if (i == 1) {
        data9 <- mm2
    } else {
        data9 <- rbind(data9, mm2)
    }
}
result <- cbind(data9, result1a[, 1], result1a[, 3])
colnames(result) <- c("groupsRecr_at", "activity_pattern", "groupsRecr_act_ts", "nrOfLocations",
    "Time_segment", "number_of_visits")
# result <- as.matrix(result)
result <- filter(as.data.frame(result), result[, 6] > 1e-09)
# result <- as.matrix(result)
number_per_location <- as.numeric(result[, 6])/as.numeric(result[, 4])
number_per_location <- matrix(number_per_location)
result3 <- cbind(result, number_per_location)
# result3 <- as.matrix(result3)
result3 <- as.data.frame(result3)
result1 <- merge(as.data.frame(cdata), as.data.frame(result3), by = intersect(c("groupsRecr_at",
    "activity_pattern", "Time_segment"), c("groupsRecr_at", "activity_pattern", "Time_segment")))
result1[, 10] <- as.numeric(result1[, 8])/as.numeric(result1[, 5])
result1 <- data.frame(result1)
names(result1)[10] <- c("number_per_transport_ID")
names(result1)[9] <- c("number_per_location")
# result1 <- as.matrix(result1) result1 <- as.data.frame(result1) result1 <-
# as.matrix(result1)


# with the personal_costs and minimum and maximum time per activitity we can
# estimate the expected visit duration per activity see excel workbook:
# Wag_documentation_English.xlsx, excel sheet: Wag_results_Z0-2018 and
# Wageningen_solution_2017_0

personal_costs <- Final_table$total_activities_costs
min_duration_abs <- Final_table$to_spend_minimum_time_per_activity
max_duration_abs <- Final_table$to_spend_maximum_time_per_activity
costs_per_min <- personal_costs/max_duration_abs
available_time_for_activity <- Final_table$Time_available_per_time_segment - (2 * Final_table$transport_travel_time_total)
maximum_duration_as_a_result_of_costs <- rep(NaN, length(available_time_for_activity))
x <- c(1:length(available_time_for_activity))
for (i in x) {
    if (personal_costs[i] == 0) {
        maximum_duration_as_a_result_of_costs[i] <- available_time_for_activity[i]
    } else {
        maximum_duration_as_a_result_of_costs[i] <- ((60 * (Final_table$maximum_budget_per_day[i] -
            Final_table$travel_costs[i]))/(personal_costs[i]))
    }
}
step1 <- pmin(max_duration_abs, maximum_duration_as_a_result_of_costs)
minimum_duration <- pmax(as.matrix(Final_table$maxduration), as.matrix(min_duration_abs))
maximum_duration <- pmin(as.matrix(available_time_for_activity), as.matrix(step1))
dMinDurationAbs <- min_duration_abs
dMaxDurationAbs <- max_duration_abs
dMinDuration <- minimum_duration
dMaxDuration <- maximum_duration
list1a <- expected_visit_duration(dMinDurationAbs, dMaxDurationAbs, dMinDuration, dMaxDuration)
expected_visiting_duration <- list1a[[1]]
expected_costs <- Final_table$travel_costs + (costs_per_min * expected_visiting_duration)
remnant_time <- available_time_for_activity - expected_visiting_duration

Ndata <- cbind(Final_table, personal_costs, min_duration_abs, max_duration_abs, costs_per_min,
    available_time_for_activity, maximum_duration_as_a_result_of_costs, step1, minimum_duration,
    maximum_duration, expected_visiting_duration, expected_costs, remnant_time)

Ndata10 <- merge(as.data.frame(Ndata), as.data.frame(result1), by = intersect(c("RecrID",
    "activity_pattern", "Time_segment"), c("RecrID", "activity_pattern", "Time_segment")))

Ndata10 <- tbl_df(Ndata10)
class(Ndata10)
Ndata11 <- select(Ndata10, hgpID, zip_code, RecrID, transport_type_ID, transport_type_definition,
    activity_pattern, activity_pattern_definition, Time_segment, zip_code, recreation_areas,
    household_types, household_types_definition, group_types, group_types_definition,
    number_of_persons, number_of_adults, number_of_children, to_spend_minimum_time_per_activity,
    maximum_number_of_times_per_activity, distances_km, transport_travel_time_sec, transport_travel_time_min,
    maximum_of_total_time, transport_travel_time_total, percent, maxduration, mintime,
    variable_costs, private_costs_per_transport_type, travel_costs, activities_extra_costs,
    activities_costs, maximum_budget_per_day, maximum_budget_per_year, total_activities_costs,
    number_available_per_time_segment, Time_available_per_time_segment, min_duration_abs,
    max_duration_abs, activities_costs_per_hour, fixed_costs, minute_time_costs, personal_costs,
    available_time_for_activity, step1, maximum_duration_as_a_result_of_costs, costs_per_min,
    minimum_duration, maximum_duration, expected_visiting_duration, expected_costs,
    remnant_time, number_of_visits)
Ndata11 <- tbl_df(Ndata11)
Ndata11 <- Ndata11[with(Ndata11, order(hgpID, zip_code, RecrID, transport_type_ID, activity_pattern,
    Time_segment)), ]
fdata15 <- filter(Ndata11, expected_visiting_duration > 0)
fdata16 <- filter(Ndata11, expected_visiting_duration == 0)

rows <- c(1:9)
c3a <- cbind(rows, c3)
rownames(c3a)<- c()
colnames(c3a)<- c("rows","col1","col2","col3")

rows <- c(1:9)
pa <- cbind(rows, plog)
rownames(pa)<- c()
rownames(pa)<- c()
colnames(pa)<- c("rows","col1","col2","col3","col4")




rows <- c(1:9)
timeMaxa <- cbind(rows, timeMax)
rownames(timeMaxa)<- c()
colnames(timeMaxa)<- c("rows","col1")

rows <- c(1:3)
data7a <- cbind(rows, data7)
rownames(data7a)<- c()
colnames(data7a)<- c("rows","groupsRecr_act_ts","col1","col2","col3","col4")


rows <- c(1:4)
TimeAvaila <- cbind(rows, TimeAvail)
rownames(TimeAvaila)<- c()
colnames(TimeAvaila)<- c("rows","col1")

rows <- c(1:29)
lin_cond_maxa <- cbind(rows, lin_cond_max)
rownames(lin_cond_maxa)<- c()
colnames(lin_cond_maxa)<- c("rows","col1")

activity_pattern <- Table_5_activities_definition_costs[, 1]
activity_pattern <- matrix(activity_pattern)
l11 <- 2 %*% g11
l11 <- matrix(l11)
g11 <- matrix(g11)

Table_l1 <- cbind(activity_pattern, ActMax, g11, l11)
Table_l1a <- data.frame(Table_l1)
names(Table_l1a)[1:4] <- c("activity_pattern", "ActMax", "g11", "l1")

dataset3 <- list(ActInfo, TimeInfo, data7a, c3a, pa, TimeAvaila, timeMaxa, lin_cond_maxa,Table_l1a)
saveRDS(dataset3,"vignettes/dataset3.Rds")

rm(c3a)
rm(data7a)
rm(pa)
rm(TimeAvaila)
rm(timeMaxa)
rm(lin_cond_maxa)
rm(Table_l1a)
rm(dataset3)

# represent Basic_Table for the number of persons
Basic_Table <- select(Basic_TableB, Time_segment, RecrID, recreation_areas, zip_code,
    hgpID, household_types, household_types_definition, group_types_definition, transport_type_ID,
    transport_type_definition, activity_pattern, activity_pattern_definition, distances_km,
    number_of_adults, number_of_children, number_of_persons)

Data_set_Final_table <- select(Final_table, recreation_areas, zip_code, transport_type_definition,
    distances_km, maximum_distance_used, household_types_definition, group_types_definition,
    activity_pattern_definition, transport_speed_km_per_hour, maximum_budget_per_day,
    Time_segment, number_of_persons, total_activities_costs, difference_in_max_time)


Basic_Table_2018 <- createWorkbook()
addWorksheet(Basic_Table_2018, "Basic_Table_persons")
hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD", halign = "center", valign = "center",
    textDecoration = "Bold", border = "TopBottomLeftRight", textRotation = 45)
modifyBaseFont(Basic_Table_2018, fontSize = 14, fontColour = "#FF0000", fontName = "Monaco")
setColWidths(Basic_Table_2018, "Basic_Table_persons", cols = 1:20, widths = "auto")
writeDataTable(Basic_Table_2018, "Basic_Table_persons", startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE, headerStyle = hs, x = Basic_Table)
saveWorkbook(Basic_Table_2018, "Wageningen_Tables_Results_excel/Basic_Table_2018.xlsx", overwrite = TRUE)

Wag_Final_table_2018 <- createWorkbook()
addWorksheet(Wag_Final_table_2018, "Final_table")
modifyBaseFont(Wag_Final_table_2018, fontSize = 14, fontColour = "#FF0000", fontName = "Monaco")
setColWidths(Wag_Final_table_2018, "Final_table", cols = 1:100, widths = "auto")
writeDataTable(Wag_Final_table_2018, "Final_table", startCol = 1, startRow = 1, colNames = TRUE,
    rowNames = FALSE, headerStyle = hs, x = Final_table)
saveWorkbook(Wag_Final_table_2018, "Wageningen_Tables_Results_excel/Wag_Final_table_2018.xlsx", overwrite = TRUE)

Sub_set_of_Final_Tables_2018 <- createWorkbook()
addWorksheet(Sub_set_of_Final_Tables_2018, "Basic_Results")
modifyBaseFont(Sub_set_of_Final_Tables_2018, fontSize = 14, fontColour = "#FF0000",
    fontName = "Monaco")
setColWidths(Sub_set_of_Final_Tables_2018, "Basic_Results", cols = 1:100, widths = "auto")
writeDataTable(Sub_set_of_Final_Tables_2018, "Basic_Results", startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE, headerStyle = hs, x = Data_set_Final_table)
saveWorkbook(Sub_set_of_Final_Tables_2018, "Wageningen_Tables_Results_excel/Sub_set_of_Final_Tables_2018.xlsx", overwrite = TRUE)

All_Wageningen_results_2018 <- createWorkbook()
addWorksheet(All_Wageningen_results_2018, "All_Results")
modifyBaseFont(All_Wageningen_results_2018, fontSize = 14, fontColour = "#FF0000", fontName = "Monaco")
setColWidths(All_Wageningen_results_2018, "All_Results", cols = 1:100, widths = "auto")
writeDataTable(All_Wageningen_results_2018, "All_Results", startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE, headerStyle = hs, x = Ndata11)
saveWorkbook(All_Wageningen_results_2018, "Wageningen_Tables_Results_excel/All_Wageningen_results_2018.xlsx", overwrite = TRUE)

Wageningen_results_Z0_2018 <- createWorkbook()
addWorksheet(Wageningen_results_Z0_2018, "Results_without_0")
modifyBaseFont(Wageningen_results_Z0_2018, fontSize = 14, fontColour = "#FF0000", fontName = "Monaco")
setColWidths(Wageningen_results_Z0_2018, "Results_without_0", cols = 1:100, widths = "auto")
writeDataTable(Wageningen_results_Z0_2018, "Results_without_0", startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE, headerStyle = hs, x = fdata15)
saveWorkbook(Wageningen_results_Z0_2018, "Wageningen_Tables_Results_excel/Wageningen_results_Z0_2018.xlsx", overwrite = TRUE)

Wageningen_results_0_2018 <- createWorkbook()
addWorksheet(Wageningen_results_0_2018, "Results_with_0")
modifyBaseFont(Wageningen_results_0_2018, fontSize = 14, fontColour = "#FF0000", fontName = "Monaco")
setColWidths(Wageningen_results_0_2018, "Results_with_0", cols = 1:100, widths = "auto")
writeDataTable(Wageningen_results_0_2018, "Results_with_0", startCol = 1, startRow = 1,
    colNames = TRUE, rowNames = FALSE, headerStyle = hs, x = fdata16)
saveWorkbook(Wageningen_results_0_2018, "Wageningen_Tables_Results_excel/Wageningen_results_0_2018.xlsx", overwrite = TRUE)



Basic_Table_persons <- rpivotTable(Basic_Table, rows = "Time_segment", cols = "zip_code",
    vals = "Freq", rendererName = "Table", aggregatorName = "Sum", "Average", "Sum")
Basic_Table_persons


PData_set_Final_table <- rpivotTable(Final_table, rows = "recreation_areas", cols = "zip_code",
    vals = "Freq", rendererName = "Table", aggregatorName = "Sum", "Average", "Sum")
PData_set_Final_table


FRecrID <- Ndata11$RecrID
Frecreation_areas <- Ndata11$recreation_areas
Fzip_code <- Ndata11$zip_code
Factivity_pattern <- Ndata11$activity_pattern
Ftransport_type_ID <- Ndata11$transport_type_ID
Factivity_pattern_definition <- Ndata11$activity_pattern_definition
Ftransport_type_definition <- Ndata11$transport_type_definition
Fhousehold_types <- Ndata11$household_types_definition
Fgroup_types <- Ndata11$group_types_definition
FTime_segment <- Ndata11$Time_segment
Fdistances_km <- Ndata11$distances_km
Fexpected_costs <- Ndata11$expected_costs
Fnumber_of_persons <- Ndata11$number_of_persons
Fnumber_of_adults <- Ndata11$number_of_adults
Fnumber_of_children <- Ndata11$number_of_children
Fexpected_visiting_duration <- Ndata11$expected_visiting_duration
Fnumber_of_visits <- as.matrix(Ndata11$number_of_visits)

fdata30 <- data.frame(FRecrID, Frecreation_areas, Fzip_code, Fdistances_km, Fhousehold_types,
    Fgroup_types, Factivity_pattern, Factivity_pattern_definition, Ftransport_type_ID,
    Ftransport_type_definition, FTime_segment, Fnumber_of_persons, Fnumber_of_adults,
    Fnumber_of_children, Fexpected_visiting_duration, Fexpected_costs, Fnumber_of_visits)

colnames(fdata30)[1] <- c("RecrID")
colnames(fdata30)[2] <- c("recreation_areas")
colnames(fdata30)[3] <- c("zip_code")
colnames(fdata30)[4] <- c("distances_km")
colnames(fdata30)[5] <- c("household_types_definition")
colnames(fdata30)[6] <- c("group_types_definition")
colnames(fdata30)[8] <- c("activity_pattern_definition")
colnames(fdata30)[10] <- c("transport_type_definition")
colnames(fdata30)[11] <- c("Time_segment")
colnames(fdata30)[12] <- c("number_of_persons")
colnames(fdata30)[13] <- c("number_of_adults")
colnames(fdata30)[14] <- c("number_of_children")
colnames(fdata30)[15] <- c("expected_visiting_duration")
colnames(fdata30)[16] <- c("expected_costs")
colnames(fdata30)[17] <- c("number_of_visits")


All_Results <- rpivotTable(fdata30, rows = "RecrID", cols = "zip_code", vals = "Freq",
    rendererName = "Table", aggregatorName = "Count", "Average", "Sum")
All_Results


FRecrID <- fdata15$RecrID
Frecreation_areas <- fdata15$recreation_areas
Fzip_code <- fdata15$zip_code
Factivity_pattern_definition <- fdata15$activity_pattern_definition
Ftransport_type_definition <- fdata15$transport_type_definition
Fdistances_km <- fdata15$distances_km
Fhousehold_types <- fdata15$household_types_definition
Fgroup_types <- fdata15$group_types_definition
FTime_segment <- fdata15$Time_segment
Fexpected_visiting_duration <- fdata15$expected_visiting_duration
Fexpected_costs <- fdata15$expected_costs
Fnumber_of_persons <- fdata15$number_of_persons
Fnumber_of_adults <- fdata15$number_of_adults
Fnumber_of_children <- fdata15$number_of_children
Fnumber_of_visits <- fdata15[50]
fdata25 <- data.frame(FRecrID, Frecreation_areas, Fzip_code, Fhousehold_types, Fgroup_types,
    Factivity_pattern_definition, Ftransport_type_definition, FTime_segment, Fdistances_km,
    Fnumber_of_persons, Fnumber_of_adults, Fnumber_of_children, Fexpected_visiting_duration,
    Fexpected_costs, Fnumber_of_visits)

colnames(fdata25)[1] <- c("RecrID")
colnames(fdata25)[2] <- c("recreation_areas")
colnames(fdata25)[3] <- c("zip_code")
colnames(fdata25)[4] <- c("household_types_definition")
colnames(fdata25)[5] <- c("group_types_definition")
colnames(fdata25)[6] <- c("activity_pattern_definition")
colnames(fdata25)[7] <- c("transport_type_definition")
colnames(fdata25)[8] <- c("Time_segment")
colnames(fdata25)[9] <- c("distances_km")
colnames(fdata25)[10] <- c("number_of_persons")
colnames(fdata25)[11] <- c("number_of_adults")
colnames(fdata25)[12] <- c("number_of_children")
colnames(fdata25)[13] <- c("expected_visiting_duration")
colnames(fdata25)[14] <- c("expected_costs")
colnames(fdata25)[15] <- c("number_of_visits")

Results_without_0 <- rpivotTable(fdata25, rows = "RecrID", cols = "zip_code", vals = "Freq",
    rendererName = "Table", aggregatorName = "Count", "Average", "Sum")
Results_without_0

choicenr <- c(1, 2, 3, 4)
choicetxt <- c("Pivot table for the number of persons", "Pivot tables for the expected visiting duration",
    "Save tables in csv format files", "Exit")
menuchoices <- cbind(choicenr, choicetxt)
print(menuchoices)
Text_menuchoices <- dlg_list(menuchoices[, 1], title = "menuchoices", multiple = TRUE,
    )$res
choices <- as.numeric(Text_menuchoices)


for (choice in choices) {
    ## Display menu choice <- displayMenu(menuItems)



        if (choice == 1) {

          RecrID <- Table_2_RecrID_type_coordinates_prices[, 1]
          print(Table_2_RecrID_type_coordinates_prices[, 1:2])
          Text_Choice_RecrID <- dlg_list(RecrID, title = "Choice_RecrID", multiple = TRUE,
          )$res
          Choice_RecrID <- as.numeric(Text_Choice_RecrID)


          transport_type_ID <- Table_15_Transport_Type_definitions[, 1]
          print(Table_15_Transport_Type_definitions)
          Text_Choice_transport_type_ID <- dlg_list(transport_type_ID, title = "Choice_transport_type_ID",
            multiple = TRUE, )$res
          Choice_transport_type_ID <- as.numeric(Text_Choice_transport_type_ID)

          # Pivot table for the number of persons

            start_time_number_of_persons <- Sys.time()

            for (j in Choice_transport_type_ID) {

                transport <- data.frame(fdata15)
                Full_Table <- filter(transport, transport_type_ID == j)
                groupRecrID <- group_by(Full_Table, RecrID) %>% summarise(c = n())
                groupRecrID <- groupRecrID[, 1, -1]  ## levels of RecrID vector
                lgroupRecrID <- length(groupRecrID)
                Basic_Table <- data.frame(Basic_TableB)
                TBasic_Table <- filter(Basic_TableB, transport_type_ID == j)

                if (j == 1) {
                  # on_foot
                  SelectBasic_Table <- select(TBasic_Table, RecrID, recreation_areas,
                    zip_code, hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, number_of_persons)
                  Select_on_Recr <- select(Full_Table, RecrID, recreation_areas, zip_code,
                    hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, number_of_persons)

                  Excel_on_foot <- createWorkbook()
                  for (i in Choice_RecrID) {
                    Select_on_Recr1 <- filter(Select_on_Recr, RecrID == i)
                    SelectBasic_Table1 <- filter(SelectBasic_Table, RecrID == i)
                    pt1 <- PivotTable$new(evaluationMode = "batch")
                    pt1$addData(Select_on_Recr1)
                    pt1$addData(SelectBasic_Table1)
                    pt1$addColumnDataGroups("zip_code", addTotal = TRUE)
                    pt1$addRowDataGroups("Time_segment", addTotal = TRUE)
                    pt1$addRowDataGroups("RecrID", addTotal = TRUE)
                    pt1$addRowDataGroups("recreation_areas", addTotal = FALSE)
                    pt1$addRowDataGroups("hgpID", addTotal = TRUE)
                    pt1$addRowDataGroups("household_types_definition", addTotal = FALSE)
                    pt1$addRowDataGroups("group_types_definition", addTotal = FALSE)
                    pt1$defineCalculation(calculationName = "Total_base", dataName = "SelectBasic_Table1",
                      caption = "Base_number_of_persons", summariseExpression = "sum(number_of_persons, na.rm=TRUE)",
                      format = "%.0f")
                    pt1$defineCalculation(calculationName = "TotalonRecr", dataName = "Select_on_Recr1",
                      caption = "N of persons on Recr1", summariseExpression = "sum(number_of_persons, na.rm = TRUE)",
                      format = "%.0f")
                    pt1$defineCalculation(calculationName = "percent", caption = "% of the base table",
                      type = "calculation", basedOn = c("Total_base", "TotalonRecr"),
                      format = "%.1f %%", calculationExpression = "values$TotalonRecr/values$Total_base * 100")
                    pt1$evaluatePivot()
                    s999 <- i
                    s999 <- as.character(s999)
                    s100 <- c("Select_on_Recr")
                    s101 <- paste(s100, s999, sep = "")
                    addWorksheet(Excel_on_foot, s101)
                    pt1$writeToExcelWorksheet(Excel_on_foot, wsName = s101, topRowNumber = 1,
                      leftMostColumnNumber = 1, applyStyles = TRUE, mapStylesFromCSS = TRUE)
                    saveWorkbook(Excel_on_foot, file = "Wageningen_Tables_Results_excel/Excel_on_foot.xlsx", overwrite = TRUE)
                    rm(pt1, Select_on_Recr1, SelectBasic_Table1)
                  }
                } else if (j == 2) {
                  # by bicycle
                  SelectBasic_Table <- select(TBasic_Table, RecrID, recreation_areas,
                    zip_code, hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, number_of_persons)
                  Select_on_Recr <- select(Full_Table, RecrID, recreation_areas, zip_code,
                    hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, number_of_persons)
                  Excel_on_bicycle <- createWorkbook()
                  for (i in Choice_RecrID) {
                    Select_on_Recr1 <- filter(Select_on_Recr, RecrID == i)
                    SelectBasic_Table1 <- filter(SelectBasic_Table, RecrID == i)
                    pt1 <- PivotTable$new(evaluationMode = "batch")
                    pt1$addData(Select_on_Recr1)
                    pt1$addData(SelectBasic_Table1)
                    pt1$addColumnDataGroups("zip_code", addTotal = TRUE)
                    pt1$addRowDataGroups("Time_segment", addTotal = TRUE)
                    pt1$addRowDataGroups("RecrID", addTotal = TRUE)
                    pt1$addRowDataGroups("recreation_areas", addTotal = FALSE)
                    pt1$addRowDataGroups("hgpID", addTotal = TRUE)
                    pt1$addRowDataGroups("household_types_definition", addTotal = FALSE)
                    pt1$addRowDataGroups("group_types_definition", addTotal = FALSE)
                    pt1$defineCalculation(calculationName = "Total_base", dataName = "SelectBasic_Table1",
                      caption = "Base_number_of_persons", summariseExpression = "sum(number_of_persons, na.rm=TRUE)",
                      format = "%.0f")
                    pt1$defineCalculation(calculationName = "TotalonRecr", dataName = "Select_on_Recr1",
                      caption = "N of persons on Recr1", summariseExpression = "sum(number_of_persons, na.rm = TRUE)",
                      format = "%.0f")
                    pt1$defineCalculation(calculationName = "percent", caption = "% of the base table",
                      type = "calculation", basedOn = c("Total_base", "TotalonRecr"),
                      format = "%.1f %%", calculationExpression = "values$TotalonRecr/values$Total_base * 100")
                    pt1$evaluatePivot()
                    s999 <- i
                    s999 <- as.character(s999)
                    s100 <- c("Select_on_Recr")
                    s101 <- paste(s100, s999, sep = "")
                    addWorksheet(Excel_on_bicycle, s101)
                    pt1$writeToExcelWorksheet(Excel_on_bicycle, wsName = s101, topRowNumber = 1,
                      leftMostColumnNumber = 1, applyStyles = TRUE, mapStylesFromCSS = TRUE)
                    saveWorkbook(Excel_on_bicycle, file = "Wageningen_Tables_Results_excel/Excel_on_bicycle.xlsx", overwrite = TRUE)
                    rm(pt1, Select_on_Recr1, SelectBasic_Table1)
                  }
                } else {
                  # by car
                  SelectBasic_Table <- select(TBasic_Table, RecrID, recreation_areas,
                    zip_code, hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, number_of_persons)
                  Select_on_Recr <- select(Full_Table, RecrID, recreation_areas, zip_code,
                    hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, number_of_persons)
                  Excel_on_car <- createWorkbook()
                  for (i in Choice_RecrID) {
                    Select_on_Recr1 <- filter(Select_on_Recr, RecrID == i)
                    SelectBasic_Table1 <- filter(SelectBasic_Table, RecrID == i)
                    pt1 <- PivotTable$new(evaluationMode = "batch")
                    pt1$addData(Select_on_Recr1)
                    pt1$addData(SelectBasic_Table1)
                    pt1$addColumnDataGroups("zip_code", addTotal = TRUE)
                    pt1$addRowDataGroups("Time_segment", addTotal = TRUE)
                    pt1$addRowDataGroups("RecrID", addTotal = TRUE)
                    pt1$addRowDataGroups("recreation_areas", addTotal = FALSE)
                    pt1$addRowDataGroups("hgpID", addTotal = TRUE)
                    pt1$addRowDataGroups("household_types_definition", addTotal = FALSE)
                    pt1$addRowDataGroups("group_types_definition", addTotal = FALSE)
                    pt1$defineCalculation(calculationName = "Total_base", dataName = "SelectBasic_Table1",
                      caption = "Base_number_of_persons", summariseExpression = "sum(number_of_persons, na.rm=TRUE)",
                      format = "%.0f")
                    pt1$defineCalculation(calculationName = "TotalonRecr", dataName = "Select_on_Recr1",
                      caption = "N of persons on Recr1", summariseExpression = "sum(number_of_persons, na.rm = TRUE)",
                      format = "%.0f")
                    pt1$defineCalculation(calculationName = "percent", caption = "% of the base table",
                      type = "calculation", basedOn = c("Total_base", "TotalonRecr"),
                      format = "%.1f %%", calculationExpression = "values$TotalonRecr/values$Total_base * 100")
                    pt1$evaluatePivot()
                    s999 <- i
                    s999 <- as.character(s999)
                    s100 <- c("Select_on_Recr")
                    s101 <- paste(s100, s999, sep = "")
                    addWorksheet(Excel_on_car, s101)
                    pt1$writeToExcelWorksheet(Excel_on_car, wsName = s101, topRowNumber = 1,
                      leftMostColumnNumber = 1, applyStyles = TRUE, mapStylesFromCSS = TRUE)
                    saveWorkbook(Excel_on_car, file = "Wageningen_Tables_Results_excel/Excel_on_car.xlsx", overwrite = TRUE)
                    rm(pt1, Select_on_Recr1, SelectBasic_Table1)
                  }
                }
            }
            end_time_number_of_persons <- Sys.time()
            Time_difference_number_of_persons <- end_time_number_of_persons - start_time_number_of_persons

        } else if (choice == 2) {

          RecrID <- Table_2_RecrID_type_coordinates_prices[, 1]
          print(Table_2_RecrID_type_coordinates_prices[, 1:2])
          Text_Choice_RecrID <- dlg_list(RecrID, title = "Choice_RecrID", multiple = TRUE,
          )$res
          Choice_RecrID <- as.numeric(Text_Choice_RecrID)


          transport_type_ID <- Table_15_Transport_Type_definitions[, 1]
          print(Table_15_Transport_Type_definitions)
          Text_Choice_transport_type_ID <- dlg_list(transport_type_ID, title = "Choice_transport_type_ID",
            multiple = TRUE, )$res
          Choice_transport_type_ID <- as.numeric(Text_Choice_transport_type_ID)


            # Pivot tables for the expected visiting duration, expected costs and number of
            # visits
            start_time_expected_number <- Sys.time()

            for (j in Choice_transport_type_ID) {

                transport <- data.frame(fdata15)
                Full_Table_rest <- filter(transport, transport_type_ID == j)
                groupRecrID <- group_by(Full_Table_rest, RecrID) %>% summarise(c = n())
                groupRecrID <- groupRecrID[, 1, -1]  ## levels of RecrID vector
                lgroupRecrID <- length(groupRecrID)

                if (j == 1) {
                  # on_foot
                  Select_on_Recr <- select(Full_Table_rest, RecrID, recreation_areas,
                    zip_code, hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, expected_visiting_duration,
                    expected_costs, number_of_visits)
                  Excel_on_foot_rest <- createWorkbook()
                  for (i in Choice_RecrID) {
                    Select_on_Recr1 <- filter(Select_on_Recr, RecrID == i)
                    pt <- PivotTable$new()
                    pt$addData(Select_on_Recr1)
                    pt$addColumnDataGroups("zip_code", addTotal = TRUE)
                    pt$addRowDataGroups("Time_segment", addTotal = TRUE)
                    pt$addRowDataGroups("RecrID", addTotal = TRUE)
                    pt$addRowDataGroups("recreation_areas", addTotal = FALSE)
                    pt$addRowDataGroups("hgpID", addTotal = TRUE)
                    pt$addRowDataGroups("household_types_definition", addTotal = FALSE)
                    pt$addRowDataGroups("group_types_definition", addTotal = FALSE)
                    pt$defineCalculation(calculationName = "Expected_visit", dataName = "Select_on_Recr1",
                      caption = "expected visiting duration", summariseExpression = "mean(expected_visiting_duration, na.rm = TRUE)",
                      format = "%.2f")
                    pt$defineCalculation(calculationName = "Expected_costs", dataName = "Select_on_Recr1",
                      caption = "expected costs", summariseExpression = "mean(expected_costs, na.rm = TRUE)",
                      format = "%.2f")
                    pt$defineCalculation(calculationName = "Number_of_visits", dataName = "Select_on_Recr1",
                      caption = "number of visits", summariseExpression = "mean(number_of_visits, na.rm = TRUE)",
                      format = "%.2f")
                    pt$renderPivot()
                    s999 <- i
                    s999 <- as.character(s999)
                    s100 <- c("Select_on_Recr")
                    s101 <- paste(s100, s999, sep = "")
                    addWorksheet(Excel_on_foot_rest, s101)
                    pt$writeToExcelWorksheet(Excel_on_foot_rest, wsName = s101, topRowNumber = 1,
                      leftMostColumnNumber = 1, applyStyles = TRUE, mapStylesFromCSS = TRUE)
                    saveWorkbook(Excel_on_foot_rest, file = "Wageningen_Tables_Results_excel/Excel_on_foot_rest.xlsx",
                      overwrite = TRUE)
                    rm(pt, Select_on_Recr1)
                  }
                } else if (j == 2) {
                  # by bicycle
                  Select_on_Recr <- select(Full_Table_rest, RecrID, recreation_areas,
                    zip_code, hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, expected_visiting_duration,
                    expected_costs, number_of_visits)
                  Excel_on_bicycle_rest <- createWorkbook()
                  for (i in Choice_RecrID) {
                    Select_on_Recr1 <- filter(Select_on_Recr, RecrID == i)
                    pt <- PivotTable$new()
                    pt$addData(Select_on_Recr1)
                    pt$addColumnDataGroups("zip_code", addTotal = TRUE)
                    pt$addRowDataGroups("Time_segment", addTotal = TRUE)
                    pt$addRowDataGroups("RecrID", addTotal = TRUE)
                    pt$addRowDataGroups("recreation_areas", addTotal = FALSE)
                    pt$addRowDataGroups("hgpID", addTotal = TRUE)
                    pt$addRowDataGroups("household_types_definition", addTotal = FALSE)
                    pt$addRowDataGroups("group_types_definition", addTotal = FALSE)
                    pt$defineCalculation(calculationName = "Expected_visit", dataName = "Select_on_Recr1",
                      caption = "expected visiting duration", summariseExpression = "mean(expected_visiting_duration, na.rm = TRUE)",
                      format = "%.2f")
                    pt$defineCalculation(calculationName = "Expected_costs", dataName = "Select_on_Recr1",
                      caption = "expected costs", summariseExpression = "mean(expected_costs, na.rm = TRUE)",
                      format = "%.2f")
                    pt$defineCalculation(calculationName = "Number_of_visits", dataName = "Select_on_Recr1",
                      caption = "number of visits", summariseExpression = "mean(number_of_visits, na.rm = TRUE)",
                      format = "%.2f")
                    pt$renderPivot()
                    s999 <- i
                    s999 <- as.character(s999)
                    s100 <- c("Select_on_Recr")
                    s101 <- paste(s100, s999, sep = "")
                    addWorksheet(Excel_on_bicycle_rest, s101)
                    pt$writeToExcelWorksheet(Excel_on_bicycle_rest, wsName = s101, topRowNumber = 1,
                      leftMostColumnNumber = 1, applyStyles = TRUE, mapStylesFromCSS = TRUE)
                    saveWorkbook(Excel_on_bicycle_rest, file = "Wageningen_Tables_Results_excel/Excel_on_bicycle_rest.xlsx",
                      overwrite = TRUE)
                    rm(pt, Select_on_Recr1)
                  }
                } else {
                  # by car
                  Select_on_Recr <- select(Full_Table_rest, RecrID, recreation_areas,
                    zip_code, hgpID, Time_segment, household_types, household_types_definition,
                    group_types, group_types_definition, distances_km, expected_visiting_duration,
                    expected_costs, number_of_visits)
                  Excel_on_car_rest <- createWorkbook()
                  for (i in Choice_RecrID) {
                    Select_on_Recr1 <- filter(Select_on_Recr, RecrID == i)
                    pt <- PivotTable$new()
                    pt$addData(Select_on_Recr1)
                    pt$addColumnDataGroups("zip_code", addTotal = TRUE)
                    pt$addRowDataGroups("Time_segment", addTotal = TRUE)
                    pt$addRowDataGroups("RecrID", addTotal = TRUE)
                    pt$addRowDataGroups("recreation_areas", addTotal = FALSE)
                    pt$addRowDataGroups("hgpID", addTotal = TRUE)
                    pt$addRowDataGroups("household_types_definition", addTotal = FALSE)
                    pt$addRowDataGroups("group_types_definition", addTotal = FALSE)
                    pt$defineCalculation(calculationName = "Expected_visit", caption = "expected visiting duration",
                      summariseExpression = "mean(expected_visiting_duration, na.rm = TRUE)",
                      format = "%.2f")
                    pt$defineCalculation(calculationName = "Expected_costs", caption = "expected costs",
                      summariseExpression = "mean(expected_costs, na.rm = TRUE)", format = "%.2f")
                    pt$defineCalculation(calculationName = "Number_of_visits", caption = "number of visits",
                      summariseExpression = "mean(number_of_visits, na.rm = TRUE)",
                      format = "%.2f")
                    pt$renderPivot()
                    s999 <- i
                    s999 <- as.character(s999)
                    s100 <- c("Select_on_Recr")
                    s101 <- paste(s100, s999, sep = "")
                    addWorksheet(Excel_on_car_rest, s101)
                    pt$writeToExcelWorksheet(Excel_on_car_rest, wsName = s101, topRowNumber = 1,
                      leftMostColumnNumber = 1, applyStyles = TRUE, mapStylesFromCSS = TRUE)
                    saveWorkbook(Excel_on_car_rest, file = "Wageningen_Tables_Results_excel/Excel_on_car_rest.xlsx",
                      overwrite = TRUE)
                    rm(pt, Select_on_Recr1)
                  }
                }
            }

            end_time_expected_number <- Sys.time()
            Time_difference_expected_number <- end_time_expected_number - start_time_expected_number


        } else if (choice == 3) {
            fdata30 <- as.matrix(fdata30)
            Ndata15 <- as.matrix(fdata15)
            Ndata16 <- as.matrix(fdata16)

            # represent Final_table of the sql results in csv file

            write.table(Final_table, file = "Wag_Final_table_2018.csv", append = FALSE,
                quote = FALSE, sep = " , ", eol = "\r", na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")


            # save All_Wageningen_results_2018 as csv file

            write.table(fdata30, file = "All_Wageningen_results_2018.csv", append = FALSE,
                quote = FALSE, sep = " , ", eol = "\r", na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")

            # save Sub_set_of_Final_Tables as csv file

            write.table(Data_set_Final_table, file = "Sub_set_of_Final_Tables_2018.csv",
                append = FALSE, quote = FALSE, sep = " , ", eol = "\r", na = "NA", dec = ".",
                row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"),
                fileEncoding = "UTF-8")

            # save Wageningen results without 0_expected_duration as csv file

            write.table(Ndata15, file = "Wageningen_results_Z0_2018.csv", append = FALSE,
                quote = FALSE, sep = " , ", eol = "\r", na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")

            # save Wageningen results with 0_expected_duration as csv file

            write.table(Ndata16, file = "Wageningen_results_0_2018.csv", append = FALSE,
                quote = FALSE, sep = " , ", eol = "\r", na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "UTF-8")

            dbDisconnect(conn)
        } else {
            dbDisconnect(conn)
            break
        }
}
