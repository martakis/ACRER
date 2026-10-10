#' Run Queries
#'
#' @param dataset1 List of basic tables
#' @param conn Optional database connection
#' @return List of query results
#' @importFrom DBI dbReadTable
#' @export
Run_Queries <- function(dataset1, conn = NULL) {
    # 1 'hpgID_household_group_types': 10 household types in 52 records
    # Field Names: hpgID,household_types,group_types,household_types_definition,group_types_definition,number_of_adults,number_of_children,
    # number_of_groups,number_of_persons,age_of_adults,age_of_the_child,dog
    # Primary key: household_typesID

    # 2 'hgpID_budget' 52 records of househod_types-groups_types (hgpID) with their corresponding budget per year and the
    # amount of money paid out per day and year
    # Field names: hgpID,household_types,household_types_definition,group_types,group_types_definition,
    # number_of_adults,number_of_children,number_of_groups,dog,number_of_persons,age_of_child,age_of_adults,
    # household_types_budget_per_year,maximum_budget_per_day,maximum_budget_per_year,Year
    # Primary key: hgpID


    # 3 'Table_15_Transport_Type_costs' Transport type definitions (3 records with 11 variables)
    # Field names: transport_type_ID,transport_type_definition,maximum_of_total_time,wait_time_per_full_travel,
    # minimum_distance_used,maximum_distance_used,speed_of_transport_type,fixed_costs_per_transport_type,
    # per_km_costs_per_transport_type,per_person_costs_per_transport_type,
    # per_km_en_per_person_costs_per_transport_type
    # Primary key: transport_type_ID

    # 4 'hgpID_transportID' 124 records of combination hgpID and transportID
    # Field names: hgpID,transport_type_ID
    # Primary keys: hgpID,transport_type_ID

    # 5 'Table_5_activities_definition_costs' Activities pattern definitions and  costs per activity 10 records
    # Field names: activity_pattern,activity_pattern_definition,fixed_costs_per_activity,costs_per_hour_per_activity,
    # costs_per_person_per_activity,costs_per_hour_and_person_per_activity
    # Primary keys: activity_pattern

    # 6 'Table_2_RecrID_type_coordinates'. Recreation area definitions. 33 records
    # Field Names: RecrID, recreation_area, Latitude_in_degrees,
    # Longitude_in_degrees, type_of_recreation_areas,
    # type_of_recreation_areas_definition,price_of_sights_or_park_per_adult,
    # price_of_sights_or_park_per_child,price_of_sights_or_park_per_dog,
    # price_of_parking
    #
    # Primary key: RecrID

    # 7 'zip_code_coordinates' The zip codes coordinates 9 records
    # Field Names:zip_code, Latitude, Longitude
    # Primary key:zip_code

    # 8 'Table_3_RecrID_activity_pattern_costs' Type of activities per recreation  area  221 records
    # Field Names: RecrID, activity_pattern, activity_pattern_definition, recreation_areas,type_of_recreation_areas,
    # type_of_recreation_areas_definition,period,fixed_costs_per_activity,costs_per_hour_per_activity,
    # costs_per_person_per_activity,costs_per_hour_and_person_per_activity,Latitude_in_degrees,Longitude_in_degrees,
    # price_of_sights_or_park_per_adult,price_of_sights_or_park_per_child,price_of_sights_or_park_per_dog,price_of_parking
    # Primary keys: RecrID,activity_pattern
    # Table3_RecrID_type_activity_pattern = RcrID_activity_pattern'

    # 9 'Table_7_zip_hgpID_budget' 468 records with 12 variables
    # Field Names: zip_code,hgpID,ID_origin_area,household_types,household_types_definition,group_types,group_types_definition,
    # number_of_adults,number_of_children,number_of_persons,dog,number_of_groups
    # Primary key:hgpID

    # 10 'Table_9_zip_code_RecrID_distances' Combinations of zip codes, recreation
    # areas and their corresponding transport types distances (in km) from zip codes to
    # recreation areas, transport travel time in sec and transport speed in km/hour. 891 records
    # Field Names: RecrID,zip_code,transport_type_ID,ID_origin_area,recreation_areas,distances_km,transport_travel_time_sec,
    # transport_speed_km_per_hour
    # Primary keys: RecrID,, "zip_code", "transport_type_ID"

    # 11 'Table_10_hgpID_activity_time'. Combinations of household_types, group_types
    # with their corresponding activity pattern, the minimum and maximum number of times
    # per activity and finaly the minimum and maximum amount of time to spend per activity.260 records
    # Field Names: hgpID, activity_pattern,household_types,household_types_definition,
    # group_types, group_types_definition,minimum_number_of_times_per_activity, maximum_number_of_times_per_activity,
    # to_spend_minimum_time_per_activity, to_spend_maximum_time_per_activity
    # Primary keys: hgpID,activity_pattern

    # 12 'Time_segment_selected'. 4 time segments are chosen with their corresponding
    # time available per segment (in min) and the number of visits per time segment 80 records'
    # Field Names: RecrID, Time_segment,number_available_per_time_segment,Circle_segment,Time_available_per_time_segment
    # Primary keys:RecrID,Time_segment


    # 13 'Table_14_hgpID_groups_types'. Household_types with their corresponding
    # groups_types (recreation person(s) with or without a dog) 52 records'
    # Field Names:hgpID, household_types, household_types_definition, group_types,
    # group_types_definition, number_of_adults, age_of_adults, number_of_children,
    # age_of_the_child, dog, number_of_groups, number_of_persons
    # Primary key: hgpID

    #14 'Table 16 hgpID with min_max_budgets'. Number of records 52
    # Field Names:hgpID,household_types,household_types_definition,group_types,group_types_definition,
    # number_of_children,number_of_persons,household_types_budget_per_year,maximum_budget_per_day,maximum_budget_per_year,Year
    # Primary key: hgpID


  logical_tables <- c("FALSE")
  if (is.list(dataset1) && length(dataset1) >= 14 && !inherits(dataset1, "DBIConnection")) {
    Table_10_hgpId_activity_time <- dataset1[[1]]
    Table_14_hgpID_groups_types <- dataset1[[2]]
    Table_15_Transport_Type_costs <- dataset1[[3]]
    Table_16_hgpID_min_max_budgets <- dataset1[[4]]
    Table_2_RecrID_type_coordinates_prices <- dataset1[[5]]
    Table_3_RecrID_activity_pattern <- dataset1[[6]]
    Table_5_activities_definition_costs <- dataset1[[7]]
    Table_7_zip_hgpID_budget <- dataset1[[8]]
    Table_9_zip_code_RecrID_distances <- dataset1[[9]]
    Time_segment_selected <- dataset1[[10]]
    hgpID_budget <- dataset1[[11]]
    hgpID_household_group_types <- dataset1[[12]]
    hgpID_transportID <- dataset1[[13]]
    zip_code_coordinates <- dataset1[[14]]
  } else {
    if (is.null(conn) && inherits(dataset1, "DBIConnection")) conn <- dataset1
    Table_10_hgpId_activity_time <- DBI::dbReadTable(conn, "Table_10_hgpId_activity_time")
    Table_14_hgpID_groups_types <- DBI::dbReadTable(conn, "Table_14_hgpID_groups_types")
    Table_15_Transport_Type_costs <- DBI::dbReadTable(conn, "Table_15_Transport_Type_costs")
    Table_16_hgpID_min_max_budgets <- DBI::dbReadTable(conn, "Table_16_hgpID_min_max_budgets")
    Table_2_RecrID_type_coordinates_prices <- DBI::dbReadTable(conn, "Table_2_RecrID_type_coordinates_prices")
    Table_3_RecrID_activity_pattern <- DBI::dbReadTable(conn, "Table_3_RecrID_activity_pattern")
    Table_5_activities_definition_costs <- DBI::dbReadTable(conn, "Table_5_activities_definition_costs")
    Table_7_zip_hgpID_budget <- DBI::dbReadTable(conn, "Table_7_zip_hgpID_budget")
    Table_9_zip_code_RecrID_distances <- DBI::dbReadTable(conn, "Table_9_zip_code_RecrID_distances")
    Time_segment_selected <- DBI::dbReadTable(conn, "Time_segment_selected")
    hgpID_budget <- DBI::dbReadTable(conn, "hgpID_budget")
    hgpID_household_group_types <- DBI::dbReadTable(conn, "hgpID_household_group_types")
    hgpID_transportID <- DBI::dbReadTable(conn, "hgpID_transportID")
    zip_code_coordinates <- DBI::dbReadTable(conn, "zip_code_coordinates")
  }

    # 1.Table_8_hgpID_transportID_costs
    # Table_8_hgpID_transportID_costs' = Table_14_hgpID_groups_types + Table_15_Transport_Type_costs + hgpID_transportID
    # Combinations of household_types, group_types with their corresponding transport type, the percentage of the maximum time of the total time,
    # the wait time per full   travel, the minimum and maximum distance per  transport type, the speed per transport type
    # and finaly the correspondings costs per transport type,fixed costs, per km costs per person costs and per km and  persons costs  119 records
    # Primary key: hgpID
    # Secondary keys:transport_type_ID

    Table_8_hgpID_transportID_costs <- sqldf::sqldf("SELECT hgpID_transportID.hgpID,
  hgpID_transportID.transport_type_ID,
        Table_15_Transport_Type_costs.transport_type_definition,
	Table_15_Transport_Type_costs.maximum_of_total_time,
	Table_15_Transport_Type_costs.minimum_distance_used,
	Table_15_Transport_Type_costs.maximum_distance_used,
	Table_15_Transport_Type_costs.wait_time_per_full_travel,
	Table_15_Transport_Type_costs.speed_of_transport_type,
	Table_15_Transport_Type_costs.fixed_costs_per_transport_type,
	Table_15_Transport_Type_costs.per_km_costs_per_transport_type,
	Table_15_Transport_Type_costs.per_person_costs_per_transport_type,
	Table_15_Transport_Type_costs.per_km_en_per_person_costs_per_transport_type,
	Table_14_hgpID_groups_types.household_types,
	Table_14_hgpID_groups_types.household_types_definition, Table_14_hgpID_groups_types.group_types,
	Table_14_hgpID_groups_types.group_types_definition, Table_14_hgpID_groups_types.number_of_groups,
	Table_14_hgpID_groups_types.age_of_adults, Table_14_hgpID_groups_types.number_of_adults,
	Table_14_hgpID_groups_types.number_of_children, Table_14_hgpID_groups_types.age_of_the_child,
	Table_14_hgpID_groups_types.number_of_persons, Table_14_hgpID_groups_types.dog
FROM Table_15_Transport_Type_costs
INNER JOIN (hgpID_transportID
INNER JOIN Table_14_hgpID_groups_types ON hgpID_transportID.hgpID = Table_14_hgpID_groups_types.hgpID) ON
	Table_15_Transport_Type_costs.transport_type_ID = hgpID_transportID.transport_type_ID
	GROUP BY hgpID_transportID.hgpID, hgpID_transportID.transport_type_ID,
	Table_15_Transport_Type_costs.transport_type_definition,
	Table_15_Transport_Type_costs.maximum_of_total_time,
	Table_15_Transport_Type_costs.minimum_distance_used,
	Table_15_Transport_Type_costs.maximum_distance_used,
	Table_15_Transport_Type_costs.wait_time_per_full_travel,
	Table_15_Transport_Type_costs.speed_of_transport_type,
	Table_15_Transport_Type_costs.fixed_costs_per_transport_type,
	Table_15_Transport_Type_costs.per_km_costs_per_transport_type,
	Table_15_Transport_Type_costs.per_person_costs_per_transport_type,
	Table_15_Transport_Type_costs.per_km_en_per_person_costs_per_transport_type,
	Table_14_hgpID_groups_types.household_types,
	Table_14_hgpID_groups_types.household_types_definition, Table_14_hgpID_groups_types.group_types,
	Table_14_hgpID_groups_types.group_types_definition, Table_14_hgpID_groups_types.number_of_groups,
	Table_14_hgpID_groups_types.age_of_adults, Table_14_hgpID_groups_types.number_of_adults,
	Table_14_hgpID_groups_types.number_of_children, Table_14_hgpID_groups_types.age_of_the_child,
	Table_14_hgpID_groups_types.number_of_persons, Table_14_hgpID_groups_types.dog;")

    # 2.  Table: 'q' 221 records and 17 columns
    # Required Basic Tables: Table_2_RecrID_type_coordinates (33 records),
    # Table_3_RecrID_activity_pattern_costs (213 records), Table_5_activities_definition_costs
    # (10 records) Join properties: RecrID, activity_pattern

    RecrID_activities_costs_prices <- sqldf::sqldf("SELECT Table_2_RecrID_type_coordinates_prices.RecrID,
    Table_3_RecrID_activity_pattern.activity_pattern,
    Table_2_RecrID_type_coordinates_prices.recreation_areas,
    Table_3_RecrID_activity_pattern.activity_pattern_definition,
    Table_3_RecrID_activity_pattern.period,
    Table_2_RecrID_type_coordinates_prices.Latitude_in_degrees,
    Table_2_RecrID_type_coordinates_prices.Longitude_in_degrees,
    Table_2_RecrID_type_coordinates_prices.type_of_recreation_areas,
    Table_2_RecrID_type_coordinates_prices.type_of_recreation_areas_definition,
    Table_2_RecrID_type_coordinates_prices.price_of_sights_or_park_per_adult,
    Table_2_RecrID_type_coordinates_prices.price_of_sights_or_park_per_child,
    Table_2_RecrID_type_coordinates_prices.price_of_sights_or_park_per_dog,
    Table_2_RecrID_type_coordinates_prices.price_of_parking,
    Table_5_activities_definition_costs.fixed_costs_per_activity,
    Table_5_activities_definition_costs.costs_per_hour_per_activity,
    Table_5_activities_definition_costs.costs_per_person_per_activity,
    Table_5_activities_definition_costs.costs_per_hour_and_person_per_activity
FROM Table_5_activities_definition_costs
INNER JOIN (Table_2_RecrID_type_coordinates_prices
INNER JOIN Table_3_RecrID_activity_pattern ON Table_2_RecrID_type_coordinates_prices.RecrID = Table_3_RecrID_activity_pattern.RecrID) ON Table_5_activities_definition_costs.activity_pattern = Table_3_RecrID_activity_pattern.activity_pattern
    GROUP BY Table_2_RecrID_type_coordinates_prices.RecrID,
    Table_3_RecrID_activity_pattern.activity_pattern,
    Table_2_RecrID_type_coordinates_prices.recreation_areas,
    Table_3_RecrID_activity_pattern.activity_pattern_definition,
    Table_3_RecrID_activity_pattern.period,
    Table_2_RecrID_type_coordinates_prices.Latitude_in_degrees,
    Table_2_RecrID_type_coordinates_prices.Longitude_in_degrees,
    Table_2_RecrID_type_coordinates_prices.type_of_recreation_areas,
    Table_2_RecrID_type_coordinates_prices.type_of_recreation_areas_definition,
    Table_2_RecrID_type_coordinates_prices.price_of_sights_or_park_per_adult,
    Table_2_RecrID_type_coordinates_prices.price_of_sights_or_park_per_child,
    Table_2_RecrID_type_coordinates_prices.price_of_sights_or_park_per_dog,
    Table_2_RecrID_type_coordinates_prices.price_of_parking,
    Table_5_activities_definition_costs.fixed_costs_per_activity,
    Table_5_activities_definition_costs.costs_per_hour_per_activity,
    Table_5_activities_definition_costs.costs_per_person_per_activity,
    Table_5_activities_definition_costs.costs_per_hour_and_person_per_activity
    ORDER BY Table_2_RecrID_type_coordinates_prices.RecrID,
    Table_3_RecrID_activity_pattern.activity_pattern;")

    # 3. Table: 'zip_code_hgpID_budget' (468 records,18 columns)
    # Required Basic Tables: hgpID_zip_code, hgpID_budget (468 records) Join properties: hgpID

    zip_code_hgpID_budget <- sqldf::sqldf("SELECT Table_7_zip_hgpID_budget.hgpID,
    Table_7_zip_hgpID_budget.zip_code,
    Table_7_zip_hgpID_budget.ID_origin_area,
    Table_7_zip_hgpID_budget.household_types,
    Table_7_zip_hgpID_budget.household_types_definition,
    Table_7_zip_hgpID_budget.group_types,
    Table_7_zip_hgpID_budget.group_types_definition,
    Table_7_zip_hgpID_budget.number_of_groups,
    Table_7_zip_hgpID_budget.number_of_adults,
    Table_7_zip_hgpID_budget.number_of_children,
    Table_7_zip_hgpID_budget.number_of_persons,
    Table_7_zip_hgpID_budget.dog,
    Table_14_hgpID_groups_types.age_of_adults,
    Table_14_hgpID_groups_types.age_of_the_child,
    Table_16_hgpID_min_max_budgets.household_types_budget_per_year,
    Table_16_hgpID_min_max_budgets.maximum_budget_per_day,
    Table_16_hgpID_min_max_budgets.maximum_budget_per_year, Table_16_hgpID_min_max_budgets.Year
    FROM (Table_14_hgpID_groups_types INNER JOIN Table_7_zip_hgpID_budget ON
    Table_14_hgpID_groups_types.hgpID = Table_7_zip_hgpID_budget.hgpID) INNER JOIN
    Table_16_hgpID_min_max_budgets ON (Table_16_hgpID_min_max_budgets.hgpID =
    Table_7_zip_hgpID_budget.hgpID) AND (Table_14_hgpID_groups_types.hgpID =
    Table_16_hgpID_min_max_budgets.hgpID)
    GROUP BY Table_7_zip_hgpID_budget.hgpID, Table_7_zip_hgpID_budget.zip_code,
    Table_7_zip_hgpID_budget.ID_origin_area, Table_7_zip_hgpID_budget.household_types,
    Table_7_zip_hgpID_budget.household_types_definition, Table_7_zip_hgpID_budget.group_types,
    Table_7_zip_hgpID_budget.group_types_definition, Table_7_zip_hgpID_budget.number_of_groups,
    Table_7_zip_hgpID_budget.number_of_adults, Table_7_zip_hgpID_budget.number_of_children,
    Table_7_zip_hgpID_budget.number_of_persons, Table_7_zip_hgpID_budget.dog,
    Table_14_hgpID_groups_types.age_of_adults, Table_14_hgpID_groups_types.age_of_the_child,
    Table_16_hgpID_min_max_budgets.household_types_budget_per_year,
    Table_16_hgpID_min_max_budgets.maximum_budget_per_day,
    Table_16_hgpID_min_max_budgets.maximum_budget_per_year, Table_16_hgpID_min_max_budgets.Year
    ORDER BY Table_7_zip_hgpID_budget.hgpID, Table_7_zip_hgpID_budget.zip_code;")


    # 4. Table: 'zip_code_Recr_hgpID' (18824 records,33 columns) Required Basic Tables:
    # Table_9_zip_code_RecrID_distances (891 records),
    # Table_8_hgpID_transportID_costs (468 records)
    # zip_code_hgpID_budget (468 records)
    # hgpID_transportID

    zip_code_Recr_hgpID <- sqldf::sqldf("SELECT zip_code_hgpID_budget.hgpID,
    Table_9_zip_code_RecrID_distances.zip_code,
    Table_9_zip_code_RecrID_distances.RecrID,
    Table_8_hgpID_transportID_costs.transport_type_ID,
    Table_9_zip_code_RecrID_distances.recreation_areas,
    Table_8_hgpID_transportID_costs.transport_type_definition,
    zip_code_hgpID_budget.household_types,
    zip_code_hgpID_budget.household_types_definition,
    zip_code_hgpID_budget.group_types,
    zip_code_hgpID_budget.group_types_definition,
    Table_8_hgpID_transportID_costs.minimum_distance_used,
    Table_8_hgpID_transportID_costs.maximum_distance_used,
    Table_9_zip_code_RecrID_distances.distances_km,
    zip_code_hgpID_budget.number_of_adults,
    zip_code_hgpID_budget.number_of_children,
    zip_code_hgpID_budget.number_of_persons,
    zip_code_hgpID_budget.number_of_groups,
    zip_code_hgpID_budget.household_types_budget_per_year,
    zip_code_hgpID_budget.maximum_budget_per_day,
    zip_code_hgpID_budget.maximum_budget_per_year,
    zip_code_hgpID_budget.dog,
    zip_code_hgpID_budget.Year,
    Table_9_zip_code_RecrID_distances.transport_travel_time_sec,
    Table_9_zip_code_RecrID_distances.transport_speed_km_per_hour,
    Table_8_hgpID_transportID_costs.maximum_of_total_time,
    Table_8_hgpID_transportID_costs.wait_time_per_full_travel,
    Table_8_hgpID_transportID_costs.speed_of_transport_type,
    Table_8_hgpID_transportID_costs.fixed_costs_per_transport_type,
    Table_8_hgpID_transportID_costs.per_km_costs_per_transport_type,
    Table_8_hgpID_transportID_costs.per_person_costs_per_transport_type,
    Table_8_hgpID_transportID_costs.per_km_en_per_person_costs_per_transport_type,
    [distances_km]*2*[per_km_costs_per_transport_type] AS fuel_cost_total
    FROM (Table_8_hgpID_transportID_costs INNER JOIN (Table_9_zip_code_RecrID_distances INNER JOIN zip_code_hgpID_budget ON Table_9_zip_code_RecrID_distances.zip_code = zip_code_hgpID_budget.zip_code) ON (Table_8_hgpID_transportID_costs.hgpID = zip_code_hgpID_budget.hgpID) AND (Table_8_hgpID_transportID_costs.transport_type_ID = Table_9_zip_code_RecrID_distances.transport_type_ID) AND (Table_8_hgpID_transportID_costs.transport_type_ID = Table_9_zip_code_RecrID_distances.transport_type_ID) AND (Table_8_hgpID_transportID_costs.transport_type_ID = Table_9_zip_code_RecrID_distances.transport_type_ID)) INNER JOIN hgpID_transportID ON (Table_8_hgpID_transportID_costs.transport_type_ID = hgpID_transportID.transport_type_ID) AND (zip_code_hgpID_budget.hgpID = hgpID_transportID.hgpID) AND (Table_8_hgpID_transportID_costs.hgpID = hgpID_transportID.hgpID) AND (Table_9_zip_code_RecrID_distances.transport_type_ID = hgpID_transportID.transport_type_ID)
    GROUP BY zip_code_hgpID_budget.hgpID,
    Table_9_zip_code_RecrID_distances.zip_code,
    Table_9_zip_code_RecrID_distances.RecrID,
    Table_8_hgpID_transportID_costs.transport_type_ID,
    Table_9_zip_code_RecrID_distances.recreation_areas,
    Table_8_hgpID_transportID_costs.transport_type_definition,
    zip_code_hgpID_budget.household_types,
    zip_code_hgpID_budget.household_types_definition,
    zip_code_hgpID_budget.group_types,
    zip_code_hgpID_budget.group_types_definition,
    Table_8_hgpID_transportID_costs.minimum_distance_used,
    Table_8_hgpID_transportID_costs.maximum_distance_used,
    Table_9_zip_code_RecrID_distances.distances_km,
    zip_code_hgpID_budget.number_of_adults,
    zip_code_hgpID_budget.number_of_children,
    zip_code_hgpID_budget.number_of_persons,
    zip_code_hgpID_budget.number_of_groups,
    zip_code_hgpID_budget.household_types_budget_per_year,
    zip_code_hgpID_budget.maximum_budget_per_day,
    zip_code_hgpID_budget.maximum_budget_per_year,
    zip_code_hgpID_budget.dog, zip_code_hgpID_budget.Year,
    Table_9_zip_code_RecrID_distances.transport_travel_time_sec,
    Table_9_zip_code_RecrID_distances.transport_speed_km_per_hour,
    Table_8_hgpID_transportID_costs.maximum_of_total_time,
    Table_8_hgpID_transportID_costs.wait_time_per_full_travel,
    Table_8_hgpID_transportID_costs.speed_of_transport_type,
    Table_8_hgpID_transportID_costs.fixed_costs_per_transport_type,
    Table_8_hgpID_transportID_costs.per_km_costs_per_transport_type,
    Table_8_hgpID_transportID_costs.per_person_costs_per_transport_type,
    Table_8_hgpID_transportID_costs.per_km_en_per_person_costs_per_transport_type
    HAVING (((Table_9_zip_code_RecrID_distances.distances_km)>=[minimum_distance_used] And (Table_9_zip_code_RecrID_distances.distances_km)<=[maximum_distance_used]))
    ORDER BY zip_code_hgpID_budget.hgpID,
    Table_9_zip_code_RecrID_distances.zip_code,
    Table_9_zip_code_RecrID_distances.RecrID,
    Table_8_hgpID_transportID_costs.transport_type_ID;")



    # 5.  Table: 'hgpID_Recr_activities_costs' (13426 records, 29 columns) Required
    # Basic Tables: Table_10_hgpID_activity_time (250 records)
    # Time_segment_selected (80 records)
    # RecrID_activities_costs_prices (221 records) join properties: RecrID, activity_pattern

    hgpID_Recr_activities_costs <- sqldf::sqldf("SELECT Table_10_hgpId_activity_time.hgpID,
    RecrID_activities_costs_prices.RecrID,
    Table_10_hgpId_activity_time.activity_pattern,
    Time_segment_selected.Time_segment,
    RecrID_activities_costs_prices.activity_pattern_definition,
    Table_10_hgpId_activity_time.household_types,
    Table_10_hgpId_activity_time.household_types_definition,
    Table_10_hgpId_activity_time.group_types,
    Table_10_hgpId_activity_time.group_types_definition,
    Table_10_hgpId_activity_time.minimum_number_of_times_per_activity,
    Table_10_hgpId_activity_time.maximum_number_of_times_per_activity,
    Table_10_hgpId_activity_time.to_spend_minimum_time_per_activity,
    Table_10_hgpId_activity_time.to_spend_maximum_time_per_activity,
    RecrID_activities_costs_prices.recreation_areas,
    RecrID_activities_costs_prices.period,
    RecrID_activities_costs_prices.Latitude_in_degrees,
    RecrID_activities_costs_prices.Longitude_in_degrees,
    RecrID_activities_costs_prices.type_of_recreation_areas,
    RecrID_activities_costs_prices.type_of_recreation_areas_definition,
    RecrID_activities_costs_prices.price_of_sights_or_park_per_adult,
    RecrID_activities_costs_prices.price_of_sights_or_park_per_child,
    RecrID_activities_costs_prices.price_of_sights_or_park_per_dog,
    RecrID_activities_costs_prices.price_of_parking,
    RecrID_activities_costs_prices.fixed_costs_per_activity,
    RecrID_activities_costs_prices.costs_per_hour_per_activity,
    RecrID_activities_costs_prices.costs_per_person_per_activity,
    RecrID_activities_costs_prices.costs_per_hour_and_person_per_activity,
    Time_segment_selected.number_available_per_time_segment,
    Time_segment_selected.Time_available_per_time_segment
FROM (Table_10_hgpId_activity_time
INNER JOIN RecrID_activities_costs_prices ON Table_10_hgpId_activity_time.activity_pattern = RecrID_activities_costs_prices.activity_pattern) INNER JOIN Time_segment_selected ON RecrID_activities_costs_prices.RecrID = Time_segment_selected.RecrID
    GROUP BY Table_10_hgpId_activity_time.hgpID,
    RecrID_activities_costs_prices.RecrID,
    Table_10_hgpId_activity_time.activity_pattern,
    Time_segment_selected.Time_segment,
    RecrID_activities_costs_prices.activity_pattern_definition,
    Table_10_hgpId_activity_time.household_types,
    Table_10_hgpId_activity_time.household_types_definition,
    Table_10_hgpId_activity_time.group_types,
    Table_10_hgpId_activity_time.group_types_definition,
    Table_10_hgpId_activity_time.minimum_number_of_times_per_activity,
    Table_10_hgpId_activity_time.maximum_number_of_times_per_activity,
    Table_10_hgpId_activity_time.to_spend_minimum_time_per_activity,
    Table_10_hgpId_activity_time.to_spend_maximum_time_per_activity,
    RecrID_activities_costs_prices.recreation_areas,
    RecrID_activities_costs_prices.period,
    RecrID_activities_costs_prices.Latitude_in_degrees,
    RecrID_activities_costs_prices.Longitude_in_degrees,
    RecrID_activities_costs_prices.type_of_recreation_areas,
    RecrID_activities_costs_prices.type_of_recreation_areas_definition,
    RecrID_activities_costs_prices.price_of_sights_or_park_per_adult,
    RecrID_activities_costs_prices.price_of_sights_or_park_per_child,
    RecrID_activities_costs_prices.price_of_sights_or_park_per_dog,
    RecrID_activities_costs_prices.price_of_parking,
    RecrID_activities_costs_prices.fixed_costs_per_activity,
    RecrID_activities_costs_prices.costs_per_hour_per_activity,
    RecrID_activities_costs_prices.costs_per_person_per_activity,
    RecrID_activities_costs_prices.costs_per_hour_and_person_per_activity,
    Time_segment_selected.number_available_per_time_segment,
    Time_segment_selected.Time_available_per_time_segment
    ORDER BY Table_10_hgpId_activity_time.hgpID,
    RecrID_activities_costs_prices.RecrID,
    Table_10_hgpId_activity_time.activity_pattern,
    Time_segment_selected.Time_segment")



    # 6. Table: 'zip_code_Recr_hgpID_transport_costs' (19824 records, 30 columns)
    # Required Basi zip_code_Recr_hgpID (19551 records, 33 columns)

    zip_code_Recr_hgpID_transport_costs <- sqldf::sqldf("SELECT zip_code_Recr_hgpID.hgpID,
    zip_code_Recr_hgpID.RecrID,
    zip_code_Recr_hgpID.zip_code,
    zip_code_Recr_hgpID.transport_type_ID,
    zip_code_Recr_hgpID.transport_type_definition,
    zip_code_Recr_hgpID.recreation_areas,
    zip_code_Recr_hgpID.household_types,
    zip_code_Recr_hgpID.household_types_definition,
    zip_code_Recr_hgpID.group_types,
    zip_code_Recr_hgpID.group_types_definition,
    zip_code_Recr_hgpID.dog,
    zip_code_Recr_hgpID.Year,
    zip_code_Recr_hgpID.distances_km,
    zip_code_Recr_hgpID.transport_travel_time_sec,
    zip_code_Recr_hgpID.transport_speed_km_per_hour,
    zip_code_Recr_hgpID.household_types_budget_per_year,
    zip_code_Recr_hgpID.maximum_budget_per_day,
    zip_code_Recr_hgpID.maximum_budget_per_year,
    zip_code_Recr_hgpID.number_of_adults,
    zip_code_Recr_hgpID.number_of_children,
    zip_code_Recr_hgpID.number_of_persons,
    zip_code_Recr_hgpID.number_of_groups,
    zip_code_Recr_hgpID.maximum_of_total_time,
    zip_code_Recr_hgpID.wait_time_per_full_travel,
    zip_code_Recr_hgpID.minimum_distance_used,
    zip_code_Recr_hgpID.maximum_distance_used,
    zip_code_Recr_hgpID.fixed_costs_per_transport_type,
    zip_code_Recr_hgpID.per_km_costs_per_transport_type,
    zip_code_Recr_hgpID.per_person_costs_per_transport_type,
    zip_code_Recr_hgpID.per_km_en_per_person_costs_per_transport_type,
    zip_code_Recr_hgpID.fuel_cost_total
    FROM zip_code_Recr_hgpID
    ORDER BY zip_code_Recr_hgpID.hgpID,
    zip_code_Recr_hgpID.RecrID,
    zip_code_Recr_hgpID.zip_code,
    zip_code_Recr_hgpID.transport_type_ID,
    zip_code_Recr_hgpID.household_types,
    zip_code_Recr_hgpID.group_types;")


    # 7. Table: 'zip_code_Recr_hgpID_transport_activities_costs'. 157820 records,49
    # columns Required Basic Tables:
    # hgpID_Recr_activities_costs
    # zip_code_Recr_hgpID_transport_costs (127319 records)
    # join properties: RecrID, hgpID

zip_code_Recr_hgpID_transport_activities_costs <- sqldf::sqldf("SELECT hgpID_Recr_activities_costs.hgpID,
    hgpID_Recr_activities_costs.RecrID,
    zip_code_Recr_hgpID_transport_costs.zip_code,
    hgpID_Recr_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_costs.transport_type_ID,
    hgpID_Recr_activities_costs.Time_segment,
    zip_code_Recr_hgpID_transport_costs.transport_type_definition,
    hgpID_Recr_activities_costs.activity_pattern_definition,
    zip_code_Recr_hgpID_transport_costs.household_types,
    zip_code_Recr_hgpID_transport_costs.household_types_definition,
    zip_code_Recr_hgpID_transport_costs.group_types,
    zip_code_Recr_hgpID_transport_costs.group_types_definition,
    hgpID_Recr_activities_costs.recreation_areas,
    hgpID_Recr_activities_costs.type_of_recreation_areas,
    hgpID_Recr_activities_costs.type_of_recreation_areas_definition,
    hgpID_Recr_activities_costs.period,
    hgpID_Recr_activities_costs.minimum_number_of_times_per_activity,
    hgpID_Recr_activities_costs.maximum_number_of_times_per_activity,
    hgpID_Recr_activities_costs.to_spend_minimum_time_per_activity,
    hgpID_Recr_activities_costs.to_spend_maximum_time_per_activity,
    hgpID_Recr_activities_costs.fixed_costs_per_activity,
    hgpID_Recr_activities_costs.costs_per_hour_per_activity,
    hgpID_Recr_activities_costs.costs_per_person_per_activity,
    hgpID_Recr_activities_costs.costs_per_hour_and_person_per_activity,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_adult,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_child,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_dog,
    hgpID_Recr_activities_costs.price_of_parking,
        CASE
        WHEN zip_code_Recr_hgpID_transport_costs.transport_type_ID = 1
OR zip_code_Recr_hgpID_transport_costs.transport_type_ID = 2 THEN 0 ELSE  hgpID_Recr_activities_costs.price_of_parking END AS price_of_parking_transport,
    zip_code_Recr_hgpID_transport_costs.transport_travel_time_sec,
    zip_code_Recr_hgpID_transport_costs.transport_speed_km_per_hour,
    zip_code_Recr_hgpID_transport_costs.Year,
    zip_code_Recr_hgpID_transport_costs.household_types_budget_per_year,
    zip_code_Recr_hgpID_transport_costs.maximum_budget_per_day,
    zip_code_Recr_hgpID_transport_costs.maximum_budget_per_year,
    zip_code_Recr_hgpID_transport_costs.number_of_adults,
    zip_code_Recr_hgpID_transport_costs.number_of_children,
    zip_code_Recr_hgpID_transport_costs.number_of_groups,
    zip_code_Recr_hgpID_transport_costs.number_of_persons,
    zip_code_Recr_hgpID_transport_costs.maximum_of_total_time,
    zip_code_Recr_hgpID_transport_costs.wait_time_per_full_travel,
    zip_code_Recr_hgpID_transport_costs.minimum_distance_used,
    zip_code_Recr_hgpID_transport_costs.maximum_distance_used,
    zip_code_Recr_hgpID_transport_costs.fixed_costs_per_transport_type,
    zip_code_Recr_hgpID_transport_costs.per_km_costs_per_transport_type,
    zip_code_Recr_hgpID_transport_costs.per_person_costs_per_transport_type,
    zip_code_Recr_hgpID_transport_costs.per_km_en_per_person_costs_per_transport_type,
    zip_code_Recr_hgpID_transport_costs.fuel_cost_total,
    hgpID_Recr_activities_costs.number_available_per_time_segment,
    hgpID_Recr_activities_costs.Time_available_per_time_segment
FROM hgpID_Recr_activities_costs
INNER JOIN zip_code_Recr_hgpID_transport_costs ON (hgpID_Recr_activities_costs.RecrID = zip_code_Recr_hgpID_transport_costs.RecrID) AND (hgpID_Recr_activities_costs.hgpID = zip_code_Recr_hgpID_transport_costs.hgpID)
    GROUP BY hgpID_Recr_activities_costs.hgpID,
    hgpID_Recr_activities_costs.RecrID,
    zip_code_Recr_hgpID_transport_costs.zip_code,
    hgpID_Recr_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_costs.transport_type_ID,
    hgpID_Recr_activities_costs.Time_segment,
    zip_code_Recr_hgpID_transport_costs.transport_type_definition,
    hgpID_Recr_activities_costs.activity_pattern_definition,
    zip_code_Recr_hgpID_transport_costs.household_types,
    zip_code_Recr_hgpID_transport_costs.household_types_definition,
    zip_code_Recr_hgpID_transport_costs.group_types,
    zip_code_Recr_hgpID_transport_costs.group_types_definition,
    hgpID_Recr_activities_costs.recreation_areas,
    hgpID_Recr_activities_costs.type_of_recreation_areas,
    hgpID_Recr_activities_costs.type_of_recreation_areas_definition,
    hgpID_Recr_activities_costs.period,
    hgpID_Recr_activities_costs.minimum_number_of_times_per_activity,
    hgpID_Recr_activities_costs.maximum_number_of_times_per_activity,
    hgpID_Recr_activities_costs.to_spend_minimum_time_per_activity,
    hgpID_Recr_activities_costs.to_spend_maximum_time_per_activity,
    hgpID_Recr_activities_costs.fixed_costs_per_activity,
    hgpID_Recr_activities_costs.costs_per_hour_per_activity,
    hgpID_Recr_activities_costs.costs_per_person_per_activity,
    hgpID_Recr_activities_costs.costs_per_hour_and_person_per_activity,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_adult,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_child,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_dog,
    hgpID_Recr_activities_costs.price_of_parking,
    zip_code_Recr_hgpID_transport_costs.distances_km,
    zip_code_Recr_hgpID_transport_costs.fuel_cost_total,
    zip_code_Recr_hgpID_transport_costs.transport_travel_time_sec,
    zip_code_Recr_hgpID_transport_costs.transport_speed_km_per_hour,
    zip_code_Recr_hgpID_transport_costs.Year,
    zip_code_Recr_hgpID_transport_costs.household_types_budget_per_year,
    zip_code_Recr_hgpID_transport_costs.maximum_budget_per_day,
    zip_code_Recr_hgpID_transport_costs.maximum_budget_per_year,
    zip_code_Recr_hgpID_transport_costs.number_of_adults,
    zip_code_Recr_hgpID_transport_costs.number_of_children,
    zip_code_Recr_hgpID_transport_costs.number_of_groups,
    zip_code_Recr_hgpID_transport_costs.number_of_persons,
    zip_code_Recr_hgpID_transport_costs.maximum_of_total_time,
    zip_code_Recr_hgpID_transport_costs.wait_time_per_full_travel,
    zip_code_Recr_hgpID_transport_costs.minimum_distance_used,
    zip_code_Recr_hgpID_transport_costs.maximum_distance_used,
    zip_code_Recr_hgpID_transport_costs.fixed_costs_per_transport_type,
    zip_code_Recr_hgpID_transport_costs.per_km_costs_per_transport_type,
    zip_code_Recr_hgpID_transport_costs.per_person_costs_per_transport_type,
    zip_code_Recr_hgpID_transport_costs.per_km_en_per_person_costs_per_transport_type,
    hgpID_Recr_activities_costs.number_available_per_time_segment,
    hgpID_Recr_activities_costs.Time_available_per_time_segment
    ORDER BY hgpID_Recr_activities_costs.hgpID,
    hgpID_Recr_activities_costs.RecrID,
    zip_code_Recr_hgpID_transport_costs.zip_code,
    hgpID_Recr_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_costs.transport_type_ID,
    hgpID_Recr_activities_costs.Time_segment;")



    ## 8. Table:z_c_R_hgpID_transport_activities_costs_A (157820 records,59 columns)
    ## Required Basic Tables: zip_code_Recr_hgpID_transport_costs (16448 records)
    # zip_code_Recr_hgpID_transport_activities_costs (127319 records)
    # join properties: RecrID, hgpID, ID_origin_area, transport_type_ID, zip_code


    z_c_R_hgpID_transport_activities_costs_A <- sqldf::sqldf("SELECT zip_code_Recr_hgpID_transport_costs.RecrID,
    zip_code_Recr_hgpID_transport_costs.hgpID,
	zip_code_Recr_hgpID_transport_costs.zip_code,
	zip_code_Recr_hgpID_transport_activities_costs.Time_segment,
	zip_code_Recr_hgpID_transport_costs.transport_type_ID,
	zip_code_Recr_hgpID_transport_activities_costs.activity_pattern,
	zip_code_Recr_hgpID_transport_activities_costs.activity_pattern_definition,
	zip_code_Recr_hgpID_transport_costs.transport_type_definition,
	zip_code_Recr_hgpID_transport_costs.household_types,
	zip_code_Recr_hgpID_transport_costs.household_types_definition,
	zip_code_Recr_hgpID_transport_costs.group_types,
	zip_code_Recr_hgpID_transport_costs.dog,
	zip_code_Recr_hgpID_transport_costs.group_types_definition,
	zip_code_Recr_hgpID_transport_activities_costs.recreation_areas,
	zip_code_Recr_hgpID_transport_costs.distances_km,
	zip_code_Recr_hgpID_transport_costs.fuel_cost_total,
	zip_code_Recr_hgpID_transport_costs.transport_travel_time_sec,
	zip_code_Recr_hgpID_transport_costs.transport_speed_km_per_hour,
	zip_code_Recr_hgpID_transport_costs.Year,
	zip_code_Recr_hgpID_transport_costs.household_types_budget_per_year,
	zip_code_Recr_hgpID_transport_costs.maximum_budget_per_day,
	zip_code_Recr_hgpID_transport_costs.maximum_budget_per_year,
	zip_code_Recr_hgpID_transport_costs.number_of_groups,
	zip_code_Recr_hgpID_transport_costs.number_of_persons,
	zip_code_Recr_hgpID_transport_costs.number_of_adults,
	zip_code_Recr_hgpID_transport_costs.number_of_children,
	zip_code_Recr_hgpID_transport_costs.maximum_of_total_time,
	zip_code_Recr_hgpID_transport_costs.wait_time_per_full_travel,
	zip_code_Recr_hgpID_transport_costs.minimum_distance_used,
	zip_code_Recr_hgpID_transport_costs.maximum_distance_used,
	zip_code_Recr_hgpID_transport_costs.fixed_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_costs.per_km_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_costs.per_person_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_costs.per_km_en_per_person_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_activities_costs.type_of_recreation_areas,
	zip_code_Recr_hgpID_transport_activities_costs.type_of_recreation_areas_definition,
	zip_code_Recr_hgpID_transport_activities_costs.period,
	zip_code_Recr_hgpID_transport_activities_costs.minimum_number_of_times_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.maximum_number_of_times_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.to_spend_minimum_time_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.to_spend_maximum_time_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.fixed_costs_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.costs_per_hour_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.costs_per_person_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.costs_per_hour_and_person_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.number_available_per_time_segment,
	zip_code_Recr_hgpID_transport_activities_costs.Time_available_per_time_segment,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_adult,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_child,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_dog,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_parking,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_parking_transport,
	(zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_adult+
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_child+
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_dog+
	zip_code_Recr_hgpID_transport_activities_costs.price_of_parking_transport) AS activities_extra_costs
	FROM zip_code_Recr_hgpID_transport_costs
    INNER JOIN zip_code_Recr_hgpID_transport_activities_costs ON (zip_code_Recr_hgpID_transport_costs.transport_type_ID =
	zip_code_Recr_hgpID_transport_activities_costs.transport_type_ID) AND
	(zip_code_Recr_hgpID_transport_costs.RecrID = 	zip_code_Recr_hgpID_transport_activities_costs.RecrID)
	AND (zip_code_Recr_hgpID_transport_costs.hgpID =
	zip_code_Recr_hgpID_transport_activities_costs.hgpID) AND
	(zip_code_Recr_hgpID_transport_costs.zip_code =
	zip_code_Recr_hgpID_transport_activities_costs.zip_code)
    GROUP BY zip_code_Recr_hgpID_transport_costs.RecrID, 	zip_code_Recr_hgpID_transport_costs.hgpID,
	zip_code_Recr_hgpID_transport_costs.zip_code,
	zip_code_Recr_hgpID_transport_activities_costs.Time_segment,
	zip_code_Recr_hgpID_transport_costs.transport_type_ID,
	zip_code_Recr_hgpID_transport_activities_costs.activity_pattern,
	zip_code_Recr_hgpID_transport_activities_costs.activity_pattern_definition,
	zip_code_Recr_hgpID_transport_costs.transport_type_definition,
	zip_code_Recr_hgpID_transport_costs.household_types,
	zip_code_Recr_hgpID_transport_costs.household_types_definition,
	zip_code_Recr_hgpID_transport_costs.group_types,
	zip_code_Recr_hgpID_transport_costs.dog,
	zip_code_Recr_hgpID_transport_costs.group_types_definition,
	zip_code_Recr_hgpID_transport_activities_costs.recreation_areas,
	zip_code_Recr_hgpID_transport_costs.distances_km,
	zip_code_Recr_hgpID_transport_costs.fuel_cost_total,
	zip_code_Recr_hgpID_transport_costs.transport_travel_time_sec,
	zip_code_Recr_hgpID_transport_costs.transport_speed_km_per_hour,
	zip_code_Recr_hgpID_transport_costs.Year,
	zip_code_Recr_hgpID_transport_costs.household_types_budget_per_year,
	zip_code_Recr_hgpID_transport_costs.maximum_budget_per_day,
	zip_code_Recr_hgpID_transport_costs.maximum_budget_per_year,
	zip_code_Recr_hgpID_transport_costs.number_of_groups,
	zip_code_Recr_hgpID_transport_costs.number_of_persons,
	zip_code_Recr_hgpID_transport_costs.number_of_adults,
	zip_code_Recr_hgpID_transport_costs.number_of_children,
	zip_code_Recr_hgpID_transport_costs.maximum_of_total_time,
	zip_code_Recr_hgpID_transport_costs.wait_time_per_full_travel,
	zip_code_Recr_hgpID_transport_costs.minimum_distance_used,
	zip_code_Recr_hgpID_transport_costs.maximum_distance_used,
	zip_code_Recr_hgpID_transport_costs.fixed_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_costs.per_km_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_costs.per_person_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_costs.per_km_en_per_person_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_activities_costs.type_of_recreation_areas,
	zip_code_Recr_hgpID_transport_activities_costs.type_of_recreation_areas_definition,
	zip_code_Recr_hgpID_transport_activities_costs.period,
	zip_code_Recr_hgpID_transport_activities_costs.minimum_number_of_times_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.maximum_number_of_times_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.to_spend_minimum_time_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.to_spend_maximum_time_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.fixed_costs_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.costs_per_hour_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.costs_per_person_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.costs_per_hour_and_person_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs.number_available_per_time_segment,
	zip_code_Recr_hgpID_transport_activities_costs.Time_available_per_time_segment,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_adult,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_child,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_dog,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_parking,
	zip_code_Recr_hgpID_transport_activities_costs.price_of_parking_transport
    ORDER BY zip_code_Recr_hgpID_transport_activities_costs.RecrID,
     zip_code_Recr_hgpID_transport_activities_costs.hgpID,
     zip_code_Recr_hgpID_transport_activities_costs.zip_code,
     zip_code_Recr_hgpID_transport_activities_costs.transport_type_ID,
     zip_code_Recr_hgpID_transport_activities_costs.activity_pattern,
     zip_code_Recr_hgpID_transport_activities_costs.Time_segment;")


    # 9. Table: zip_code_Recr_hgpID_transport_activities_costs_B
    #  (157820  records, 70columns) Required Basic Tables: 0 SQL Tables:
    # z_c_R_hgpID_transport_activities_costs_A (145772 records) join properties: 0

    zip_code_Recr_hgpID_transport_activities_costs_B <- sqldf::sqldf("SELECT
	z_c_R_hgpID_transport_activities_costs_A.hgpID,
	z_c_R_hgpID_transport_activities_costs_A.RecrID,
	z_c_R_hgpID_transport_activities_costs_A.zip_code,
	z_c_R_hgpID_transport_activities_costs_A.transport_type_ID,
	z_c_R_hgpID_transport_activities_costs_A.activity_pattern,
	z_c_R_hgpID_transport_activities_costs_A.Time_segment,
	z_c_R_hgpID_transport_activities_costs_A.transport_type_definition,
	z_c_R_hgpID_transport_activities_costs_A.activity_pattern_definition,
	z_c_R_hgpID_transport_activities_costs_A.household_types,
	z_c_R_hgpID_transport_activities_costs_A.household_types_definition,
	z_c_R_hgpID_transport_activities_costs_A.group_types,
	z_c_R_hgpID_transport_activities_costs_A.group_types_definition,
	z_c_R_hgpID_transport_activities_costs_A.recreation_areas,
	z_c_R_hgpID_transport_activities_costs_A.distances_km,
	z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec,
	z_c_R_hgpID_transport_activities_costs_A.transport_speed_km_per_hour,
	z_c_R_hgpID_transport_activities_costs_A.Year,
	z_c_R_hgpID_transport_activities_costs_A.household_types_budget_per_year,
	z_c_R_hgpID_transport_activities_costs_A.maximum_budget_per_day,
	z_c_R_hgpID_transport_activities_costs_A.maximum_budget_per_year,
	z_c_R_hgpID_transport_activities_costs_A.number_of_groups,
	z_c_R_hgpID_transport_activities_costs_A.number_of_persons,
	z_c_R_hgpID_transport_activities_costs_A.number_of_adults,
	z_c_R_hgpID_transport_activities_costs_A.number_of_children,
	z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time,
	z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel,
	z_c_R_hgpID_transport_activities_costs_A.minimum_distance_used,
	z_c_R_hgpID_transport_activities_costs_A.maximum_distance_used,
	z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.type_of_recreation_areas,
	z_c_R_hgpID_transport_activities_costs_A.type_of_recreation_areas_definition,
	z_c_R_hgpID_transport_activities_costs_A.period,
	z_c_R_hgpID_transport_activities_costs_A.minimum_number_of_times_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.maximum_number_of_times_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.to_spend_maximum_time_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.number_available_per_time_segment,
	z_c_R_hgpID_transport_activities_costs_A.Time_available_per_time_segment,
	z_c_R_hgpID_transport_activities_costs_A.price_of_sights_or_park_per_adult,
	z_c_R_hgpID_transport_activities_costs_A.price_of_sights_or_park_per_child,
	z_c_R_hgpID_transport_activities_costs_A.price_of_parking,
       z_c_R_hgpID_transport_activities_costs_A.activities_extra_costs,
	z_c_R_hgpID_transport_activities_costs_A.fuel_cost_total,
  (z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec)/60 AS transport_travel_time_min,
  (z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60) AS transport_travel_time_total,
2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time AS percent,
    (CASE
WHEN (z_c_R_hgpID_transport_activities_costs_A.minimum_number_of_times_per_activity >= (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec)/60)) * ((100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) THEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity ELSE ((2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec)/60)) * ((100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) END) AS maxduration,
    (2 * (z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)) +
    (CASE
    WHEN
    (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)+ 	z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time) THEN
    z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity ELSE 2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time END) AS mintime,
   (z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type +
	(z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type)) AS variable_costs,
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type +
z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type AS private_costs_per_transport_type,
2*((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60))*
(z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type))+
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+
z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type AS travel_costs,
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity +
z_c_R_hgpID_transport_activities_costs_A.activities_extra_costs+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity) AS activities_costs,
z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity +
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity) AS activities_costs_per_hour,
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity+
z_c_R_hgpID_transport_activities_costs_A.activities_extra_costs+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity)+
2*((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60))*
(z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type))+
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+
z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type AS fixed_costs,
((2 * (z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)) +
    (CASE
    WHEN
    (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)+ 	z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time) THEN
    z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity ELSE 2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time END)) *
(z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity +
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity)) AS minute_time_costs,
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity+
z_c_R_hgpID_transport_activities_costs_A.activities_extra_costs+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity)+
2*((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60))*
(z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type))+
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+
z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type +
((2 * (z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)) +
    (CASE
    WHEN
    (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)+ 	z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time) THEN
    z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity ELSE 2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time END)) *
(z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity +
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity)) AS total_activities_costs,
z_c_R_hgpID_transport_activities_costs_A.maximum_budget_per_day -
(z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity+
z_c_R_hgpID_transport_activities_costs_A.activities_extra_costs+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity)+
2*((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60))*
(z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type+
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type))+
z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+
z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type +
((2 * (z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)) +
    (CASE
    WHEN
    (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + (z_c_R_hgpID_transport_activities_costs_A. transport_travel_time_sec)/60)+ 	z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time) THEN
    z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity ELSE 2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel+
z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*(100-z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time END)) *
(z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity +
(z_c_R_hgpID_transport_activities_costs_A.number_of_persons*
z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity))) AS difference_in_costs
FROM
	z_c_R_hgpID_transport_activities_costs_A
GROUP BY
	z_c_R_hgpID_transport_activities_costs_A.hgpID,
	z_c_R_hgpID_transport_activities_costs_A.RecrID,
	z_c_R_hgpID_transport_activities_costs_A.zip_code,
	z_c_R_hgpID_transport_activities_costs_A.transport_type_ID,
	z_c_R_hgpID_transport_activities_costs_A.activity_pattern,
	z_c_R_hgpID_transport_activities_costs_A.Time_segment,
	z_c_R_hgpID_transport_activities_costs_A.transport_type_definition,
	z_c_R_hgpID_transport_activities_costs_A.activity_pattern_definition,
	z_c_R_hgpID_transport_activities_costs_A.household_types,
	z_c_R_hgpID_transport_activities_costs_A.household_types_definition,
	z_c_R_hgpID_transport_activities_costs_A.group_types,
	z_c_R_hgpID_transport_activities_costs_A.group_types_definition,
	z_c_R_hgpID_transport_activities_costs_A.recreation_areas,
	z_c_R_hgpID_transport_activities_costs_A.distances_km,
	z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec,
	z_c_R_hgpID_transport_activities_costs_A.transport_speed_km_per_hour,
	z_c_R_hgpID_transport_activities_costs_A.Year,
	z_c_R_hgpID_transport_activities_costs_A.household_types_budget_per_year,
	z_c_R_hgpID_transport_activities_costs_A.maximum_budget_per_day,
	z_c_R_hgpID_transport_activities_costs_A.maximum_budget_per_year,
	z_c_R_hgpID_transport_activities_costs_A.number_of_groups,
	z_c_R_hgpID_transport_activities_costs_A.number_of_persons,
	z_c_R_hgpID_transport_activities_costs_A.number_of_adults,
	z_c_R_hgpID_transport_activities_costs_A.number_of_children,
	z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time,
	z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel,
	z_c_R_hgpID_transport_activities_costs_A.minimum_distance_used,
	z_c_R_hgpID_transport_activities_costs_A.maximum_distance_used,
	z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type,
	z_c_R_hgpID_transport_activities_costs_A.type_of_recreation_areas,
	z_c_R_hgpID_transport_activities_costs_A.type_of_recreation_areas_definition,
	z_c_R_hgpID_transport_activities_costs_A.period,
	z_c_R_hgpID_transport_activities_costs_A.minimum_number_of_times_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.maximum_number_of_times_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.to_spend_maximum_time_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity,
	z_c_R_hgpID_transport_activities_costs_A.number_available_per_time_segment,
	z_c_R_hgpID_transport_activities_costs_A.Time_available_per_time_segment,
	z_c_R_hgpID_transport_activities_costs_A.price_of_sights_or_park_per_adult,
	z_c_R_hgpID_transport_activities_costs_A.price_of_sights_or_park_per_child,
	z_c_R_hgpID_transport_activities_costs_A.price_of_parking,
	z_c_R_hgpID_transport_activities_costs_A.fuel_cost_total,
	z_c_R_hgpID_transport_activities_costs_A.activities_extra_costs
ORDER BY
	z_c_R_hgpID_transport_activities_costs_A.hgpID,
	z_c_R_hgpID_transport_activities_costs_A.RecrID,
	z_c_R_hgpID_transport_activities_costs_A.zip_code,
	z_c_R_hgpID_transport_activities_costs_A.transport_type_ID,
	z_c_R_hgpID_transport_activities_costs_A.activity_pattern,
	z_c_R_hgpID_transport_activities_costs_A.Time_segment,
	z_c_R_hgpID_transport_activities_costs_A.household_types,
	z_c_R_hgpID_transport_activities_costs_A.group_types;")


    # 10. Table: Final_Table (157820 records, 70 columns) Required Basic Tables: 0 SQL
    # Tables: z_c_R_hgpID_transport_activities_costs_A (145772 records, 70 columns) join
    # properties: 0

    Final_table <- sqldf::sqldf("SELECT 	zip_code_Recr_hgpID_transport_activities_costs_B.hgpID,
	zip_code_Recr_hgpID_transport_activities_costs_B.RecrID,
	zip_code_Recr_hgpID_transport_activities_costs_B.zip_code,
	zip_code_Recr_hgpID_transport_activities_costs_B.transport_type_ID,
	zip_code_Recr_hgpID_transport_activities_costs_B.activity_pattern,
	zip_code_Recr_hgpID_transport_activities_costs_B.Time_segment,
	zip_code_Recr_hgpID_transport_activities_costs_B.transport_type_definition,
	zip_code_Recr_hgpID_transport_activities_costs_B.activity_pattern_definition,
	zip_code_Recr_hgpID_transport_activities_costs_B.household_types,
	zip_code_Recr_hgpID_transport_activities_costs_B.household_types_definition,
	zip_code_Recr_hgpID_transport_activities_costs_B.group_types,
	zip_code_Recr_hgpID_transport_activities_costs_B.group_types_definition,
	zip_code_Recr_hgpID_transport_activities_costs_B.recreation_areas,
	zip_code_Recr_hgpID_transport_activities_costs_B.distances_km,
	zip_code_Recr_hgpID_transport_activities_costs_B.transport_travel_time_sec,
	zip_code_Recr_hgpID_transport_activities_costs_B.transport_speed_km_per_hour,
	zip_code_Recr_hgpID_transport_activities_costs_B.Year,
	zip_code_Recr_hgpID_transport_activities_costs_B.maximum_budget_per_day,
	zip_code_Recr_hgpID_transport_activities_costs_B.maximum_budget_per_year,
	zip_code_Recr_hgpID_transport_activities_costs_B.number_of_groups,
	zip_code_Recr_hgpID_transport_activities_costs_B.number_of_children,
	zip_code_Recr_hgpID_transport_activities_costs_B.number_of_adults,
	zip_code_Recr_hgpID_transport_activities_costs_B.number_of_persons,
	zip_code_Recr_hgpID_transport_activities_costs_B.maximum_of_total_time,
	zip_code_Recr_hgpID_transport_activities_costs_B.wait_time_per_full_travel,
	zip_code_Recr_hgpID_transport_activities_costs_B.minimum_distance_used,
	zip_code_Recr_hgpID_transport_activities_costs_B.maximum_distance_used,
	zip_code_Recr_hgpID_transport_activities_costs_B.fixed_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_activities_costs_B.per_km_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_activities_costs_B.per_person_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_activities_costs_B.per_km_en_per_person_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_activities_costs_B.type_of_recreation_areas,
	zip_code_Recr_hgpID_transport_activities_costs_B.type_of_recreation_areas_definition,
	zip_code_Recr_hgpID_transport_activities_costs_B.period,
	zip_code_Recr_hgpID_transport_activities_costs_B.minimum_number_of_times_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.maximum_number_of_times_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.to_spend_minimum_time_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.to_spend_maximum_time_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.fixed_costs_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.costs_per_hour_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.costs_per_person_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.costs_per_hour_and_person_per_activity,
	zip_code_Recr_hgpID_transport_activities_costs_B.number_available_per_time_segment,
	zip_code_Recr_hgpID_transport_activities_costs_B.Time_available_per_time_segment,
	zip_code_Recr_hgpID_transport_activities_costs_B.price_of_sights_or_park_per_adult,
	zip_code_Recr_hgpID_transport_activities_costs_B.price_of_sights_or_park_per_child,
	zip_code_Recr_hgpID_transport_activities_costs_B.price_of_parking,
	zip_code_Recr_hgpID_transport_activities_costs_B.activities_extra_costs,
	zip_code_Recr_hgpID_transport_activities_costs_B.transport_travel_time_min,
	zip_code_Recr_hgpID_transport_activities_costs_B.transport_travel_time_total,
	zip_code_Recr_hgpID_transport_activities_costs_B.percent,
	zip_code_Recr_hgpID_transport_activities_costs_B.maxduration,
	zip_code_Recr_hgpID_transport_activities_costs_B.mintime,
	zip_code_Recr_hgpID_transport_activities_costs_B.variable_costs,
	zip_code_Recr_hgpID_transport_activities_costs_B.private_costs_per_transport_type,
	zip_code_Recr_hgpID_transport_activities_costs_B.travel_costs,
	zip_code_Recr_hgpID_transport_activities_costs_B.activities_costs,
	zip_code_Recr_hgpID_transport_activities_costs_B.activities_costs_per_hour,
	zip_code_Recr_hgpID_transport_activities_costs_B.fixed_costs,
	zip_code_Recr_hgpID_transport_activities_costs_B.minute_time_costs,
	zip_code_Recr_hgpID_transport_activities_costs_B.total_activities_costs,
	zip_code_Recr_hgpID_transport_activities_costs_B.fuel_cost_total
FROM
	zip_code_Recr_hgpID_transport_activities_costs_B
ORDER BY
	zip_code_Recr_hgpID_transport_activities_costs_B.hgpID,
	zip_code_Recr_hgpID_transport_activities_costs_B.RecrID,
	zip_code_Recr_hgpID_transport_activities_costs_B.zip_code,
	zip_code_Recr_hgpID_transport_activities_costs_B.transport_type_ID,
	zip_code_Recr_hgpID_transport_activities_costs_B.activity_pattern,
	zip_code_Recr_hgpID_transport_activities_costs_B.Time_segment,
	zip_code_Recr_hgpID_transport_activities_costs_B.household_types,
	zip_code_Recr_hgpID_transport_activities_costs_B.group_types;")


    # 11. Table:data (1074 records)
    data <- sqldf::sqldf("SELECT Final_table.RecrID, Final_table.activity_pattern, Final_table.transport_type_ID, Final_table.Time_segment
    FROM Final_table
    GROUP BY Final_table.RecrID, Final_table.activity_pattern, Final_table.transport_type_ID, Final_table.Time_segment
    ORDER BY Final_table.RecrID, Final_table.activity_pattern, Final_table.transport_type_ID, Final_table.Time_segment;")



    # 12. Table: ActInfo 10 records Required Basic Tables: 0 SQL Tables: Final_Table
    # (127319 records, 70 columns) join properties: 0

    ActInfo <- sqldf::sqldf("SELECT Final_table.activity_pattern,
    Avg(Final_table.maximum_number_of_times_per_activity) AS maximum_number_of_times_per_activity
    FROM Final_table
    GROUP BY Final_table.activity_pattern
    ORDER BY Final_table.activity_pattern, Avg(Final_table.maximum_number_of_times_per_activity);")



    # 13. Table: TimeInfo 4 records Required Basic Tables: 0 SQL Tables: Final_Table

    TimeInfo <- sqldf::sqldf("SELECT Final_table.Time_segment,
    Avg(Final_table.number_available_per_time_segment) AS AvgOfnumber_available_per_time_segment
    FROM Final_table
    GROUP BY Final_table.Time_segment;")


    list5 <- list(Final_table, data, ActInfo, TimeInfo)
    return(list5)
}

