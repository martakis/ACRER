#' function to RUN the queries from the tables included from the SQLite database
#'
#' dataset1 List includes the Basic Tables
#'
#' Run_Queries(dataset1)
#'
#' '1 Table_2_RecrID_type_coordinates_prices' Recreation area definitions. 33 records Field Names: RecrID, recreation_area, Latitude_in_degrees,Longitude_in_degrees, type_of_recreation_areas,type_of_recreation_areas_definition, price_of_sights_or_park_per_child, price_of_parking Primary key: RecrID
#' '2 Table_3_RecrID_type_activity_pattern' Type of activities per recreation  area  213 records Field Names: RecrID, activity_pattern, recreation_areas,type_of_recreation_areas, type_of_recreation_areas_definition Primary key: RecrID Secondary key: activity_pattern
#' '3 Table_5_activities_definition_costs' Activities pattern definitions and  costs per activity 10 records Field Names: activity_pattern,  activity_pattern_definition, fixed_costs_per_activity, costs_per_hour_per_activity,  costs_per_person_per_activity,costs_per_hour_and_person_per_activity Primary key:activity_pattern
#' '4 Table_14_hgpID_groups_types' Houshold_types with their corresponding groups_types (recreation person) 50 records Field Names: hgpID, household_types,household_types_definition, group_types, group_types_definition, number_of_adults,  age_of_adults, number_of_children, age_of_the_child, dog, number_of_groups,number_of_persons Primary key: hgpID
#' '5 Table_16_hgpID_min_max_budgets' househod_types-groups_types (hgpID) with their corresponding budget per year and the amount of money paid out per day and year 50 records Field Names: hgpID, household_types_budget_per_year,number_of_groups, number_of_persons, maximum_budget_per_day,maximum_budget_per_year, Year Primary key: hgpID
#' '6 Table_7_zip_hgpID_budget' Combinations of Houshold_types with their corresponding groups_types (recreation person) per zip code 450 records Field
#' Names: hgpID, ID_origin_area, zip_code, household_types,household_types_definition, group_types, group_types_definition, Year,number_of_adults, number_of_children, number_of_groups, number_of_persons,maximum_budget_per_day, maximum_budget_per_year Primary key: hgpID Secondary key:ID_origin_area
#' '7 Table_9_zip_code_RecrID_distances' Combinations of zip codes, recreation areas and their corresponding transport types distances (in km) from zip codes to recreation areas, transport travel time in sec and transport speed inkm/hour 891 records Field Names: ID_origin_area, RecrID, transport_type_ID, zip_code, recreation_areas, distances_km, transport_travel_time_sec,transport_speed_km_per_hour Primary key: ID_origin_area Secondary keys: RecrID, transport_type_ID
#' '8 Table_8_hgpID_distance_costs' Combinations of household_types, group_types with their corresponding transport type, the percentage of the maximum time of the total time, the wait time per full travel, the minimum and maximum distance per  transport type, the speed per transport type and finaly the correspondings costs per transport type, fixed costs, per km costs per person costs and per km and  persons costs  119 records
#' Field Names: hgpID, transport_type_ID,  household_types, group_types, household_types_definition, group_types_definition,  maximum_of_total_time, wait_time_per_full_travel, minimum_distance_used,maximum_distance_used, speed_of_transport_type, fixed_costs_per_transport_type,per_km_costs_per_transport_type, per_person_costs_per_transport_type,  per_km_en_per_person_costs_per_transport_type Primary key: hgpID Secondary key:transport_type_ID
#' '9 Table_10_hgpId_activity_budget' Combinations of household_types, group_typeswith their corresponding activity pattern, the minimum and maximum number of times per activity and finaly the minimum and maximum amount of time to spend per   activity 250 records
#' Field Names: hgpID, activity_pattern, activity_pattern, household_types, household_types_definition, group_types,group_types_definition,  minimum_number_of_times_per_activity, maximum_number_of_times_per_activity,to_spend_minimum_time_per_activity, to_spend_maximum_time_per_activity Primary key: hgpID Secondary key: activity_pattern
#' '10 Time_segment_selected' 4 time segments are chosen with their corresponding time available per segment (in min) and the number of visits per time segment 79 records'
#' Field Names: RecrID, Time_segment, number_available_per_time_segment,Circle_segment, Time_available_per_time_segment Primary key: RecrID Secondary key:Time_segment
#' '11 zip_code_coordinates' The zip codes coordinates 9 records Field Names:zip_code, Latitude, Longitude Primary key:zip_code
#' Field Names:transport_type_ID,transport_type_definitions,transport_speed_km_per_hour,minimum_distance_per_transport_used,maximum_distance_per_transport_used   Primary key: transport_type_ID.
#' @param dataset1 List of tables included in the SQLite database
#' @return list5 Final_table, data, ActInfo, TimeInfo, Basic_TableB.



Run_Queries <- function(dataset1) {
##.dataset1 list includes the Basic Tables
##
##
##
  ## . 1 'Table_2_RecrID_type_coordinates_prices'. Recreation area definitions. 33
  ## records Field Names: RecrID, recreation_area, Latitude_in_degrees,
  ## Longitude_in_degrees, type_of_recreation_areas,
  ## type_of_recreation_areas_definition, type_of_recreation_areas_definition,
  ## price_of_sights_or_park_per_child, price_of_parking Primary key: RecrID
  ##
  ##
  ##

    Table_2_RecrID_type_coordinates_prices <- dataset1[[1]]

    ## . 2 'Table_3_RecrID_type_activity_pattern'. Type of activities per recreation
    ## area.  213 records Field Names: RecrID, activity_pattern, recreation_areas,
    ## type_of_recreation_areas, type_of_recreation_areas_definition Primary key: RecrID
    ## Secondary key: activity_pattern
    ##
    ##
    ##

    Table3_RecrID_activity_pattern <- dataset1[[2]]

    ## . 3 'Table_5_activities_definition_costs'. Activities pattern definitions and
    ## costs per activity 10 records Field Names: activity_pattern,
    ## activity_pattern_definition, fixed_costs_per_activity,
    ## costs_per_hour_per_activity,
    ## costs_per_person_per_activity,costs_per_hour_and_person_per_activity Primary key:
    ## activity_pattern

    Table_5_activities_definition_costs <- dataset1[[3]]

    ## . 4 'Table_14_hgpID_groups_types'. Houshold_types with their corresponding
    ## groups_types (recreation person) 50 records Field Names: hgpID, household_types,
    ## household_types_definition, group_types, group_types_definition, number_of_adults,
    ## age_of_adults, number_of_children, age_of_the_child, dog, number_of_groups,
    ## number_of_persons Primary key: hgpID

    Table_14_hgpID_groups_types <- dataset1[[4]]

    ## . 5 'Table_16_hgpID_min_max_budgets'. househod_types-groups_types (hgpID) with
    ## their corresponding budget per year and the amount of money paid out per day and
    ## year 50 records Field Names: hgpID, household_types_budget_per_year,
    ## number_of_groups, number_of_persons, maximum_budget_per_day,
    ## maximum_budget_per_year, Year Primary key: hgpID
    ##
    ##
    ##

    Table_16_hgpID_min_max_budgets <- dataset1[[5]]

    ## . 6 'Table_7_zip_hgpID_budget' Combinations of Houshold_types with their
    ## corresponding groups_types (recreation person) per zip code 450 records Field
    ## Names: hgpID, ID_origin_area, zip_code, household_types,
    ## household_types_definition, group_types, group_types_definition, Year,
    ## number_of_adults, number_of_children, number_of_groups, number_of_persons,
    ## maximum_budget_per_day, maximum_budget_per_year Primary key: hgpID Secondary key:
    ## ID_origin_area
    ##
    ##
    ##

    Table7_zip_hgpID_budget <- dataset1[[6]]

    ## . 7 'Table_9_zip_code_RecrID_distances' Combinations of zip codes, recreation
    ## areas and their corresponding transport types distances (in km) from zip codes to
    ## recreation areas, transport travel time in sec and transport speed inkm/hour. 891
    ## records Field Names: ID_origin_area, RecrID, transport_type_ID, zip_code,
    ## recreation_areas, distances_km, transport_travel_time_sec,
    ## transport_speed_km_per_hour Primary key: ID_origin_area Secondary keys: RecrID,
    ## transport_type_ID
    ##
    ##
    ##

    Table_9_zip_code_RecrID_distances <- dataset1[[7]]

    ## . 8 'Table_8_hgpID_distance_costs'. Combinations of household_types, group_types
    ## with their corresponding transport type, the percentage of the maximum time of the
    ## total time, the wait time per full travel, the minimum and maximum distance per
    ## transport type, the speed per transport type and finaly the correspondings costs
    ## per transport type, fixed costs, per km costs per person costs and per km and
    ## persons costs.  119 records Field Names: hgpID, transport_type_ID,
    ## household_types, group_types, household_types_definition, group_types_definition,
    ## maximum_of_total_time, wait_time_per_full_travel, minimum_distance_used,
    ## maximum_distance_used, speed_of_transport_type, fixed_costs_per_transport_type,
    ## per_km_costs_per_transport_type, per_person_costs_per_transport_type,
    ## per_km_en_per_person_costs_per_transport_type Primary key: hgpID Secondary key:
    ## transport_type_ID

    Table_8_hgpID_distance_costs <- dataset1[[8]]

    ## . 9 'Table_10_hgpId_activity_budget'. Combinations of household_types, group_types
    ## with their corresponding activity pattern, the minimum and maximum number of times
    ## per activity and finaly the minimum and maximum amount of time to spend per
    ## activity. 250 records Field Names: hgpID, activity_pattern, activity_pattern,
    ## household_types, household_types_definition, group_types, group_types_definition,
    ## minimum_number_of_times_per_activity, maximum_number_of_times_per_activity,
    ## to_spend_minimum_time_per_activity, to_spend_maximum_time_per_activity Primary
    ## key: hgpID Secondary key: activity_pattern

    Table_10_hgpID_activity_budget <- dataset1[[9]]

    ## . 10 'Time_segment_selected. 4 time segments are chosen with their corresponding
    ## time available per segment (in min) and the number of visits per time segment 79
    ## records' Field Names: RecrID, Time_segment, number_available_per_time_segment,
    ### Circle_segment, Time_available_per_time_segment Primary key: RecrID Secondary key:
    ## Time_segment
    ##
    ##
    ##


    Time_segment_selected <- dataset1[[10]]

    ## . 11 'zip_code_coordinates'. The zip codes coordinates 9 records Field Names:
    ## zip_code, Latitude, Longitude Primary key: zip_code
    ##
    ##
    ##

    Table_15_Transport_Type_definitions <- dataset1[[11]]

    ## Field Names:
    ## transport_type_ID,transport_type_definitions,transport_speed_km_per_hour,minimum_distance_per_transport_used,maximum_distance_per_transport_used
    ## Primary key: transport_type_ID
    ##
    ##
    ##
    zip_code_coordinates <- dataset1[[12]]



    # 1.  Table: 'RecrID_activities_costs_prices' 213 records and 17 columns Required
    # Basic Tables: Table_2_RecrID_type_coordinates_prices (33 records),
    # Table3_RecrID_activity_pattern (213 records), Table_5_activities_definition_costs
    # (10 records) Join properties: RecrID, activity_pattern

    RecrID_activities_costs_prices <- sqldf::sqldf("SELECT Table_2_RecrID_type_coordinates_prices.RecrID,
    Table_2_RecrID_type_coordinates_prices.recreation_areas,
    Table3_RecrID_activity_pattern.activity_pattern,
    Table3_RecrID_activity_pattern.activity_pattern_definition,
    Table3_RecrID_activity_pattern.period,
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
    FROM Table_5_activities_definition_costs INNER JOIN (Table_2_RecrID_type_coordinates_prices INNER JOIN
    Table3_RecrID_activity_pattern ON Table_2_RecrID_type_coordinates_prices.RecrID =
    Table3_RecrID_activity_pattern.RecrID) ON Table_5_activities_definition_costs.activity_pattern =
    Table3_RecrID_activity_pattern.activity_pattern
    ORDER BY Table_2_RecrID_type_coordinates_prices.RecrID,
    Table3_RecrID_activity_pattern.activity_pattern;")

    # 2. Table: 'zip_code_hgpID_budget' (468 records,18 columns) Required Basic Tables:
    # Table_14_hgpID_groups_types (52 records), Table_16_hgpID_min_max_budgets (52
    # records), Table7_zip_hgpID_budget (468 records) Join properties: hgpID

    zip_code_hgpID_budget <- sqldf::sqldf("SELECT Table7_zip_hgpID_budget.zip_code,
    Table_14_hgpID_groups_types.hgpID,
    Table7_zip_hgpID_budget.ID_origin_area,
    Table_14_hgpID_groups_types.household_types,
    Table_14_hgpID_groups_types.household_types_definition,
    Table_14_hgpID_groups_types.number_of_adults,
    Table_14_hgpID_groups_types.age_of_adults,
    Table_14_hgpID_groups_types.group_types,
    Table_14_hgpID_groups_types.group_types_definition,
    Table_14_hgpID_groups_types.number_of_children,
    Table_14_hgpID_groups_types.dog,
    Table_14_hgpID_groups_types.number_of_groups,
    Table_14_hgpID_groups_types.number_of_persons,
    Table_14_hgpID_groups_types.age_of_the_child,
    Table_16_hgpID_min_max_budgets.household_types_budget_per_year,
    Table_16_hgpID_min_max_budgets.maximum_budget_per_day,
    Table_16_hgpID_min_max_budgets.maximum_budget_per_year,
    Table_16_hgpID_min_max_budgets.Year
    FROM Table_16_hgpID_min_max_budgets INNER JOIN (Table_14_hgpID_groups_types INNER JOIN
    Table7_zip_hgpID_budget ON Table_14_hgpID_groups_types.hgpID = Table7_zip_hgpID_budget.hgpID) ON
    (Table_14_hgpID_groups_types.hgpID = Table_16_hgpID_min_max_budgets.hgpID) AND
    (Table_16_hgpID_min_max_budgets.hgpID = Table7_zip_hgpID_budget.hgpID);
    ORDER BY Table_14_hgpID_groups_types.hgpID,
    Table7_zip_hgpID_budget.ID_origin_area;")


    # 3. Table: 'zip_code_Recr_hgpID' (19551 records,33 columns) Required Basic Tables:
    # Table_9_zip_code_RecrID_distances (891 records), Table_8_hgpID_distance_costs (468
    # records), SQL Tables: zip_code_hgpID_budget (468
    # records),Table_15_Transport_Type_definitions (3 records) Join properties: hgpID,
    # transport_type_ID, household_types, group_types, ID_origin_area, RecrID, zip_code

    zip_code_Recr_hgpID <- sqldf::sqldf("SELECT Table_9_zip_code_RecrID_distances.RecrID,
    Table_9_zip_code_RecrID_distances.zip_code,
    Table_8_hgpID_distance_costs.hgpID,
    Table_15_Transport_Type_definitions.transport_type_ID,
    Table_15_Transport_Type_definitions.transport_type_definition,
    Table_8_hgpID_distance_costs.household_types,
    Table_8_hgpID_distance_costs.household_types_definition,
    Table_8_hgpID_distance_costs.group_types,
    Table_8_hgpID_distance_costs.group_types_definition,
    Table_9_zip_code_RecrID_distances.recreation_areas,
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
    Table_8_hgpID_distance_costs.maximum_of_total_time,
    Table_8_hgpID_distance_costs.wait_time_per_full_travel,
    Table_8_hgpID_distance_costs.minimum_distance_used,
    Table_8_hgpID_distance_costs.maximum_distance_used,
    Table_8_hgpID_distance_costs.speed_of_transport_type,
    Table_8_hgpID_distance_costs.fixed_costs_per_transport_type,
    Table_8_hgpID_distance_costs.per_km_costs_per_transport_type,
    Table_8_hgpID_distance_costs.per_person_costs_per_transport_type,
    Table_8_hgpID_distance_costs.per_km_en_per_person_costs_per_transport_type,
    Table_8_hgpID_distance_costs.Met_Auto
    FROM Table_15_Transport_Type_definitions INNER JOIN ((Table_9_zip_code_RecrID_distances INNER JOIN
    Table_8_hgpID_distance_costs ON Table_9_zip_code_RecrID_distances.transport_type_ID =
    Table_8_hgpID_distance_costs.transport_type_ID) INNER JOIN zip_code_hgpID_budget ON
    (Table_8_hgpID_distance_costs.hgpID = zip_code_hgpID_budget.hgpID) AND
    (Table_9_zip_code_RecrID_distances.zip_code = zip_code_hgpID_budget.zip_code)) ON
    Table_15_Transport_Type_definitions.transport_type_ID = Table_8_hgpID_distance_costs.transport_type_ID
    WHERE (((Table_9_zip_code_RecrID_distances.distances_km)>=[minimum_distance_used] And
    (Table_9_zip_code_RecrID_distances.distances_km)<=[maximum_distance_used]))
    ORDER BY zip_code_hgpID_budget.hgpID,
    zip_code_hgpID_budget.zip_code,
    Table_9_zip_code_RecrID_distances.RecrID,
    Table_8_hgpID_distance_costs.transport_type_ID,
    zip_code_hgpID_budget.household_types,
    zip_code_hgpID_budget.group_types;")

    # 3a. Table: 'Basic_TableA' (36828 records, 14 columns) Required Basic Tables:
    # Table_9_zip_code_RecrID_distances (891 records),
    # Table_8_hgpID_distance_costs,Table_15_Transport_Type_definitions,Table_9_zip_code_RecrID_distances
    # SQL Tables: zip_code_hgpID_budget (468
    # records),Table_15_Transport_Type_definitions (3 records) Join properties: hgpID,
    # transport_type_ID, household_types, group_types, ID_origin_area, RecrID, zip_code
    Basic_TableA <- sqldf::sqldf("SELECT Table_8_hgpID_distance_costs.hgpID,
    Table_9_zip_code_RecrID_distances.RecrID,
\t  Table_9_zip_code_RecrID_distances.recreation_areas,
    Table_9_zip_code_RecrID_distances.zip_code,
    Table_15_Transport_Type_definitions.transport_type_ID,
    Table_15_Transport_Type_definitions.transport_type_definition,
    Table_8_hgpID_distance_costs.household_types,
    Table_8_hgpID_distance_costs.household_types_definition,
    Table_8_hgpID_distance_costs.group_types,
    Table_8_hgpID_distance_costs.group_types_definition,
    Table_9_zip_code_RecrID_distances.distances_km,
    zip_code_hgpID_budget.number_of_adults,
    zip_code_hgpID_budget.number_of_children,
    zip_code_hgpID_budget.number_of_persons
    FROM ((Table_9_zip_code_RecrID_distances INNER JOIN Table_8_hgpID_distance_costs ON
    Table_9_zip_code_RecrID_distances.transport_type_ID = Table_8_hgpID_distance_costs.transport_type_ID)
    INNER JOIN zip_code_hgpID_budget ON (Table_8_hgpID_distance_costs.hgpID = zip_code_hgpID_budget.hgpID)
    AND (Table_9_zip_code_RecrID_distances.zip_code = zip_code_hgpID_budget.zip_code)) INNER JOIN
    Table_15_Transport_Type_definitions ON (Table_15_Transport_Type_definitions.transport_type_ID =
    Table_8_hgpID_distance_costs.transport_type_ID) AND (Table_9_zip_code_RecrID_distances.transport_type_ID
    = Table_15_Transport_Type_definitions.transport_type_ID)
    GROUP BY Table_8_hgpID_distance_costs.hgpID, Table_9_zip_code_RecrID_distances.RecrID,
    Table_9_zip_code_RecrID_distances.recreation_areas, Table_9_zip_code_RecrID_distances.zip_code,
    Table_15_Transport_Type_definitions.transport_type_ID,
    Table_15_Transport_Type_definitions.transport_type_definition, Table_8_hgpID_distance_costs.household_types,
    Table_8_hgpID_distance_costs.household_types_definition, Table_8_hgpID_distance_costs.group_types,
    Table_8_hgpID_distance_costs.group_types_definition, Table_9_zip_code_RecrID_distances.distances_km,
    zip_code_hgpID_budget.number_of_adults, zip_code_hgpID_budget.number_of_children,
    zip_code_hgpID_budget.number_of_persons
    ORDER BY Table_8_hgpID_distance_costs.hgpID, Table_9_zip_code_RecrID_distances.RecrID,
    Table_9_zip_code_RecrID_distances.zip_code, Table_15_Transport_Type_definitions.transport_type_ID;")

    # 3b. Table: 'Basic_TableB' Table required for the number of persons in the
    # zip_codes of Wageningen (574740 records, 19 columns) Required Basic Tables:
    # Basic_TableA,Time_segment_selected SQL Tables: RecrID_activities_costs_prices Join
    # properties: hgpID, transport_type_ID, household_types, group_types,
    # ID_origin_area, RecrID, zip_code

    Basic_TableB <- sqldf::sqldf("SELECT Basic_TableA.RecrID, Basic_TableA.zip_code,
      Basic_TableA.hgpID,
      Basic_TableA.transport_type_ID,
      RecrID_activities_costs_prices.activity_pattern,
      Time_segment_selected.Time_segment,
      RecrID_activities_costs_prices.activity_pattern_definition,
      Basic_TableA.recreation_areas,
      Basic_TableA.transport_type_definition,
      Basic_TableA.household_types,
      Basic_TableA.household_types_definition,
      Basic_TableA.group_types,
      Basic_TableA.group_types_definition,
      Time_segment_selected.number_available_per_time_segment,
      Time_segment_selected.Time_available_per_time_segment,
      Basic_TableA.distances_km,
      Basic_TableA.number_of_adults,
      Basic_TableA.number_of_children, Basic_TableA.number_of_persons
      FROM (RecrID_activities_costs_prices INNER JOIN Time_segment_selected ON
      RecrID_activities_costs_prices.RecrID = Time_segment_selected.RecrID) INNER JOIN Basic_TableA ON
      Time_segment_selected.RecrID = Basic_TableA.RecrID
      GROUP BY Basic_TableA.RecrID, Basic_TableA.zip_code, Basic_TableA.hgpID, Basic_TableA.transport_type_ID,
      RecrID_activities_costs_prices.activity_pattern, Time_segment_selected.Time_segment,
      RecrID_activities_costs_prices.activity_pattern_definition, Basic_TableA.recreation_areas,
      Basic_TableA.transport_type_definition, Basic_TableA.household_types,
      Basic_TableA.household_types_definition, Basic_TableA.group_types, Basic_TableA.group_types_definition,
      Time_segment_selected.number_available_per_time_segment,
      Time_segment_selected.Time_available_per_time_segment, Basic_TableA.distances_km,
      Basic_TableA.number_of_adults, Basic_TableA.number_of_children, Basic_TableA.number_of_persons
      ORDER BY Basic_TableA.RecrID, Basic_TableA.zip_code, Basic_TableA.hgpID, Basic_TableA.transport_type_ID,
      RecrID_activities_costs_prices.activity_pattern, Time_segment_selected.Time_segment;")


    # . 4 Table: 'hgpID_Recr_activities_costs' (12625 records, 29 columns) Required
    # Basic Tables: Table_10_hgpID_activity_budget (250 records), Time_segment_selected
    # (79 records) SQL Tables: RecrID_activities_costs_prices (213 records) join
    # properties: RecrID, activity_pattern

    hgpID_Recr_activities_costs <- sqldf::sqldf("SELECT RecrID_activities_costs_prices.RecrID,
    Table_10_hgpID_activity_budget.hgpID,
    Table_10_hgpID_activity_budget.activity_pattern,
    Time_segment_selected.Time_segment,
    RecrID_activities_costs_prices.activity_pattern_definition,
    Table_10_hgpID_activity_budget.household_types,
    Table_10_hgpID_activity_budget.household_types_definition,
    Table_10_hgpID_activity_budget.group_types,
    Table_10_hgpID_activity_budget.group_types_definition,
    Table_10_hgpID_activity_budget.minimum_number_of_times_per_activity,
    Table_10_hgpID_activity_budget.maximum_number_of_times_per_activity,
    Table_10_hgpID_activity_budget.to_spend_minimum_time_per_activity,
    Table_10_hgpID_activity_budget.to_spend_maximum_time_per_activity,
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
    FROM (RecrID_activities_costs_prices INNER JOIN Table_10_hgpID_activity_budget ON
    RecrID_activities_costs_prices.activity_pattern = Table_10_hgpID_activity_budget.activity_pattern) INNER JOIN
    Time_segment_selected ON RecrID_activities_costs_prices.RecrID = Time_segment_selected.RecrID
    GROUP BY RecrID_activities_costs_prices.RecrID,
    Table_10_hgpID_activity_budget.hgpID,
    Table_10_hgpID_activity_budget.activity_pattern,
    Time_segment_selected.Time_segment,
    RecrID_activities_costs_prices.activity_pattern_definition,
    Table_10_hgpID_activity_budget.household_types,
    Table_10_hgpID_activity_budget.household_types_definition,
    Table_10_hgpID_activity_budget.group_types,
    Table_10_hgpID_activity_budget.group_types_definition,
    Table_10_hgpID_activity_budget.minimum_number_of_times_per_activity,
    Table_10_hgpID_activity_budget.maximum_number_of_times_per_activity,
    Table_10_hgpID_activity_budget.to_spend_minimum_time_per_activity,
    Table_10_hgpID_activity_budget.to_spend_maximum_time_per_activity,
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
    Time_segment_selected.Time_available_per_time_segment;")

    # 5. Table: 'zip_code_Recr_hgpID_transport_costs' (19551 records, 30 columns)
    # Required Basic Tables: 0 SQL Tables: zip_code_Recr_hgpID (19551 records, 33
    # columns, speed_of_transport_type is not required) join properties: 0

    zip_code_Recr_hgpID_transport_costs <- sqldf::sqldf("SELECT zip_code_Recr_hgpID.hgpID,
    zip_code_Recr_hgpID.zip_code,
    zip_code_Recr_hgpID.RecrID,
    zip_code_Recr_hgpID.transport_type_ID,
    zip_code_Recr_hgpID.transport_type_definition,
    zip_code_Recr_hgpID.recreation_areas,
    zip_code_Recr_hgpID.household_types,
    zip_code_Recr_hgpID.household_types_definition,
    zip_code_Recr_hgpID.group_types,
    zip_code_Recr_hgpID.group_types_definition,
    zip_code_Recr_hgpID.dog, zip_code_Recr_hgpID.Year,
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
    zip_code_Recr_hgpID.Met_Auto
    FROM zip_code_Recr_hgpID
    ORDER BY zip_code_Recr_hgpID.hgpID,
    zip_code_Recr_hgpID.RecrID,
    zip_code_Recr_hgpID.transport_type_ID;")

    # 6. Table: 'zip_code_Recr_hgpID_transport_activities_costs'. 148631 records,49
    # columns Required Basic Tables: 0 SQL Tables: hgpID_Recr_activities_costs (12175
    # records), zip_code_Recr_hgpID_transport_costs (127319 records) join properties:
    # RecrID, hgpID

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
    WHEN zip_code_Recr_hgpID_transport_costs.transport_type_ID = 1 OR zip_code_Recr_hgpID_transport_costs.transport_type_ID = 3 THEN 0 ELSE hgpID_Recr_activities_costs.price_of_parking END AS price_of_parking_transport,

    zip_code_Recr_hgpID_transport_costs.distances_km,
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
    FROM hgpID_Recr_activities_costs INNER JOIN zip_code_Recr_hgpID_transport_costs ON
\t  (hgpID_Recr_activities_costs.RecrID = zip_code_Recr_hgpID_transport_costs.RecrID) AND
    (hgpID_Recr_activities_costs.hgpID = zip_code_Recr_hgpID_transport_costs.hgpID)
    GROUP BY hgpID_Recr_activities_costs.hgpID, hgpID_Recr_activities_costs.RecrID,
    zip_code_Recr_hgpID_transport_costs.zip_code, hgpID_Recr_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_costs.transport_type_ID, hgpID_Recr_activities_costs.Time_segment,
    zip_code_Recr_hgpID_transport_costs.transport_type_definition,
    hgpID_Recr_activities_costs.activity_pattern_definition, zip_code_Recr_hgpID_transport_costs.household_types,
    zip_code_Recr_hgpID_transport_costs.household_types_definition,
    zip_code_Recr_hgpID_transport_costs.group_types,
    zip_code_Recr_hgpID_transport_costs.group_types_definition, hgpID_Recr_activities_costs.recreation_areas,
    hgpID_Recr_activities_costs.type_of_recreation_areas,
    hgpID_Recr_activities_costs.type_of_recreation_areas_definition, hgpID_Recr_activities_costs.period,
    hgpID_Recr_activities_costs.minimum_number_of_times_per_activity,
    hgpID_Recr_activities_costs.maximum_number_of_times_per_activity,
    hgpID_Recr_activities_costs.to_spend_minimum_time_per_activity,
    hgpID_Recr_activities_costs.to_spend_maximum_time_per_activity,
    hgpID_Recr_activities_costs.fixed_costs_per_activity, hgpID_Recr_activities_costs.costs_per_hour_per_activity,
    hgpID_Recr_activities_costs.costs_per_person_per_activity,
    hgpID_Recr_activities_costs.costs_per_hour_and_person_per_activity,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_adult,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_child,
    hgpID_Recr_activities_costs.price_of_sights_or_park_per_dog, hgpID_Recr_activities_costs.price_of_parking,
    zip_code_Recr_hgpID_transport_costs.distances_km,
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
    ORDER BY hgpID_Recr_activities_costs.hgpID, hgpID_Recr_activities_costs.RecrID,
    zip_code_Recr_hgpID_transport_costs.zip_code, hgpID_Recr_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_costs.transport_type_ID, hgpID_Recr_activities_costs.Time_segment;")


    ## 7. Table:z_c_R_hgpID_transport_activities_costs_A (127319 records,59 columns)
    ## Required Basic Tables: 0 SQL Tables: zip_code_Recr_hgpID_transport_costs (16448
    ## records), zip_code_Recr_hgpID_transport_activities_costs (127319 records) join
    ## properties: RecrID, hgpID, ID_origin_area, transport_type_ID, zip_code,
    ## household_types, group_types

    z_c_R_hgpID_transport_activities_costs_A <- sqldf::sqldf("SELECT zip_code_Recr_hgpID_transport_costs.RecrID,
    zip_code_Recr_hgpID_transport_costs.hgpID,
    zip_code_Recr_hgpID_transport_costs.zip_code,
    zip_code_Recr_hgpID_transport_costs.transport_type_ID,
    zip_code_Recr_hgpID_transport_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_activities_costs.Time_segment,
    zip_code_Recr_hgpID_transport_activities_costs.activity_pattern_definition,
    zip_code_Recr_hgpID_transport_activities_costs.transport_type_definition,
    zip_code_Recr_hgpID_transport_costs.household_types,
    zip_code_Recr_hgpID_transport_costs.household_types_definition,
    zip_code_Recr_hgpID_transport_costs.group_types,
    zip_code_Recr_hgpID_transport_costs.dog,
    zip_code_Recr_hgpID_transport_costs.group_types_definition,
    zip_code_Recr_hgpID_transport_activities_costs.recreation_areas,
    zip_code_Recr_hgpID_transport_costs.distances_km,
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
    zip_code_Recr_hgpID_transport_activities_costs.price_of_parking,
    zip_code_Recr_hgpID_transport_activities_costs.price_of_parking_transport,
    zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_dog,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=1 And ((zip_code_Recr_hgpID_transport_costs.group_types=1 Or zip_code_Recr_hgpID_transport_costs.group_types=3)) THEN ([price_of_sights_or_park_per_adult]+[price_of_parking_transport]) ELSE 0 END AS ticket_1_adult_without_children_without_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=1 And ((zip_code_Recr_hgpID_transport_costs.group_types=2 Or zip_code_Recr_hgpID_transport_costs.group_types=4)) THEN ([price_of_sights_or_park_per_adult]+[price_of_parking_transport]) ELSE 0 END AS ticket_2_adult_without_children_with_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=1 And ((zip_code_Recr_hgpID_transport_costs.group_types=9 Or zip_code_Recr_hgpID_transport_costs.group_types=10)) THEN ([price_of_sights_or_park_per_adult]+[price_of_parking_transport]) ELSE 0 END AS ticket_3_single_child,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=2 And ((zip_code_Recr_hgpID_transport_costs.group_types=5 Or zip_code_Recr_hgpID_transport_costs.group_types=6)) THEN (2*[price_of_sights_or_park_per_adult]+[price_of_parking_transport]) ELSE 0 END AS ticket_4_adults_without_children_without_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=2 And ((zip_code_Recr_hgpID_transport_costs.group_types=7 Or zip_code_Recr_hgpID_transport_costs.group_types=8)) THEN (2*[price_of_sights_or_park_per_adult]+[price_of_parking_transport]) ELSE 0 END AS ticket_5_adults_without_children_with_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=3 And ((zip_code_Recr_hgpID_transport_costs.group_types=11 Or zip_code_Recr_hgpID_transport_costs.group_types=13)) THEN (2*[price_of_sights_or_park_per_adult]+[price_of_sights_or_park_per_child]+[price_of_parking_transport]) ELSE 0 END AS ticket_6_adult_with_children_without_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=3 And ((zip_code_Recr_hgpID_transport_costs.group_types=12 Or zip_code_Recr_hgpID_transport_costs.group_types=14)) THEN (2*[price_of_sights_or_park_per_adult]+[price_of_sights_or_park_per_child]+[price_of_parking_transport]) ELSE 0 END AS ticket_7_adult_with_children_with_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=4 And ((zip_code_Recr_hgpID_transport_costs.group_types=15 Or zip_code_Recr_hgpID_transport_costs.group_types=17)) THEN (2*[price_of_sights_or_park_per_adult]+2*[price_of_sights_or_park_per_child]+[price_of_parking_transport]) ELSE 0 END AS ticket_8_adults_with_children_without_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.number_of_persons=4 And ((zip_code_Recr_hgpID_transport_costs.group_types=16 Or zip_code_Recr_hgpID_transport_costs.group_types=18)) THEN (2*[price_of_sights_or_park_per_adult]+2*[price_of_sights_or_park_per_child]+[price_of_parking_transport]) ELSE 0 END AS ticket_9_adults_with_children_with_car,

    CASE
    WHEN zip_code_Recr_hgpID_transport_costs.dog = 1 THEN [price_of_sights_or_park_per_dog] ELSE 0 END AS dog_price

    FROM zip_code_Recr_hgpID_transport_costs INNER JOIN zip_code_Recr_hgpID_transport_activities_costs ON
\t(zip_code_Recr_hgpID_transport_costs.zip_code = zip_code_Recr_hgpID_transport_activities_costs.zip_code)
    AND (zip_code_Recr_hgpID_transport_costs.hgpID = zip_code_Recr_hgpID_transport_activities_costs.hgpID)
    AND (zip_code_Recr_hgpID_transport_costs.RecrID = zip_code_Recr_hgpID_transport_activities_costs.RecrID)
    AND (zip_code_Recr_hgpID_transport_costs.transport_type_ID =
    zip_code_Recr_hgpID_transport_activities_costs.transport_type_ID)
    GROUP BY zip_code_Recr_hgpID_transport_costs.RecrID, zip_code_Recr_hgpID_transport_costs.hgpID,
    zip_code_Recr_hgpID_transport_costs.zip_code, zip_code_Recr_hgpID_transport_costs.transport_type_ID,
    zip_code_Recr_hgpID_transport_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_activities_costs.Time_segment,
    zip_code_Recr_hgpID_transport_activities_costs.activity_pattern_definition,
    zip_code_Recr_hgpID_transport_activities_costs.transport_type_definition,
    zip_code_Recr_hgpID_transport_costs.household_types,
    zip_code_Recr_hgpID_transport_costs.household_types_definition,
    zip_code_Recr_hgpID_transport_costs.group_types, zip_code_Recr_hgpID_transport_costs.dog,
    zip_code_Recr_hgpID_transport_costs.group_types_definition,
    zip_code_Recr_hgpID_transport_activities_costs.recreation_areas,
    zip_code_Recr_hgpID_transport_costs.distances_km,
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
    zip_code_Recr_hgpID_transport_activities_costs.price_of_parking,
    zip_code_Recr_hgpID_transport_activities_costs.price_of_parking_transport,
    zip_code_Recr_hgpID_transport_activities_costs.price_of_sights_or_park_per_dog
    ORDER BY zip_code_Recr_hgpID_transport_costs.RecrID, zip_code_Recr_hgpID_transport_costs.hgpID,
    zip_code_Recr_hgpID_transport_costs.zip_code, zip_code_Recr_hgpID_transport_costs.transport_type_ID,
    zip_code_Recr_hgpID_transport_activities_costs.activity_pattern,
    zip_code_Recr_hgpID_transport_activities_costs.Time_segment;")

    # 8. Table: 'zip_code_Recr_hgpID_transport_activities_costs_B' (148631 records, 70
    # columns) Required Basic Tables: 0 SQL Tables:
    # z_c_R_hgpID_transport_activities_costs_A (148631 records) join properties: 0

    zip_code_Recr_hgpID_transport_activities_costs_B <- sqldf::sqldf("SELECT z_c_R_hgpID_transport_activities_costs_A.RecrID,
z_c_R_hgpID_transport_activities_costs_A.hgpID,
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
    z_c_R_hgpID_transport_activities_costs_A.ticket_1_adult_without_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_2_adult_without_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_3_single_child,
    z_c_R_hgpID_transport_activities_costs_A.ticket_4_adults_without_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_5_adults_without_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_6_adult_with_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_7_adult_with_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_8_adults_with_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_9_adults_with_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.dog_price,

    z_c_R_hgpID_transport_activities_costs_A.ticket_1_adult_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_2_adult_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_3_single_child + z_c_R_hgpID_transport_activities_costs_A.ticket_4_adults_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_5_adults_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_6_adult_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_7_adult_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_8_adults_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_9_adults_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.dog_price AS activities_extra_costs,

    z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60 AS transport_travel_time_min,

    z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60 AS transport_travel_time_total,

    2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time) AS percent,

    CASE
    WHEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=(2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) THEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity  ELSE (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) END AS maxduration,

    2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60) +
    (CASE
    WHEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=(2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) THEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity  ELSE (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) END) AS mintime,

    z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons*z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type) AS variable_costs,

    z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type + z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type AS private_costs_per_transport_type,

    (2*(((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)/60)) * (z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type))) + (z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type) AS travel_costs,

    z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity + (z_c_R_hgpID_transport_activities_costs_A.ticket_1_adult_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_2_adult_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_3_single_child + z_c_R_hgpID_transport_activities_costs_A.ticket_4_adults_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_5_adults_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_6_adult_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_7_adult_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_8_adults_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_9_adults_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.dog_price) + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity) AS activities_costs,

    z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity) AS activities_costs_per_hour,

    z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity + ((z_c_R_hgpID_transport_activities_costs_A.ticket_1_adult_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_2_adult_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_3_single_child + z_c_R_hgpID_transport_activities_costs_A.ticket_4_adults_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_5_adults_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_6_adult_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_7_adult_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_8_adults_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_9_adults_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.dog_price) + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity)) +
    ((2*(((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)/60)) * (z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type))) + (z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type)) AS fixed_costs,

    (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60) +
    (CASE
    WHEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=(2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) THEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity  ELSE (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) END)) * ((z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity))/60) AS minute_time_costs,

(z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity + ((z_c_R_hgpID_transport_activities_costs_A.ticket_1_adult_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_2_adult_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_3_single_child + z_c_R_hgpID_transport_activities_costs_A.ticket_4_adults_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_5_adults_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_6_adult_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_7_adult_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_8_adults_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_9_adults_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.dog_price) + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity)) +
    ((2*(((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)/60)) * (z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type))) + (z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type))) +
((2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60) +
(CASE
    WHEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=(2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) THEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity  ELSE (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) END)) * ((z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity))/60)) AS total_activities_costs,

    z_c_R_hgpID_transport_activities_costs_A.maximum_budget_per_day -

((z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_activity + ((z_c_R_hgpID_transport_activities_costs_A.ticket_1_adult_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_2_adult_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_3_single_child + z_c_R_hgpID_transport_activities_costs_A.ticket_4_adults_without_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_5_adults_without_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_6_adult_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_7_adult_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.ticket_8_adults_with_children_without_car + z_c_R_hgpID_transport_activities_costs_A.ticket_9_adults_with_children_with_car + z_c_R_hgpID_transport_activities_costs_A.dog_price) + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_person_per_activity)) +
    ((2*(((z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)/60)) * (z_c_R_hgpID_transport_activities_costs_A.per_km_costs_per_transport_type + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.per_km_en_per_person_costs_per_transport_type))) + (z_c_R_hgpID_transport_activities_costs_A.fixed_costs_per_transport_type+z_c_R_hgpID_transport_activities_costs_A.per_person_costs_per_transport_type))) +
    ((2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60) +
    (CASE
    WHEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=(2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) THEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity  ELSE (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) END)) * ((z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_per_activity + (z_c_R_hgpID_transport_activities_costs_A.number_of_persons * z_c_R_hgpID_transport_activities_costs_A.costs_per_hour_and_person_per_activity))/60))) AS difference_in_costs,

    z_c_R_hgpID_transport_activities_costs_A.to_spend_maximum_time_per_activity  -
((2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60) +
    (CASE
    WHEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity>=(2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) THEN z_c_R_hgpID_transport_activities_costs_A.to_spend_minimum_time_per_activity  ELSE (2*(z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60)*((100 - z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)/z_c_R_hgpID_transport_activities_costs_A.maximum_of_total_time)) END)) -
(2 * (z_c_R_hgpID_transport_activities_costs_A.wait_time_per_full_travel + z_c_R_hgpID_transport_activities_costs_A.transport_travel_time_sec/60))) AS difference_in_max_time

    FROM z_c_R_hgpID_transport_activities_costs_A
    GROUP BY z_c_R_hgpID_transport_activities_costs_A.RecrID,
    z_c_R_hgpID_transport_activities_costs_A.hgpID,
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
    z_c_R_hgpID_transport_activities_costs_A.ticket_1_adult_without_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_2_adult_without_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_3_single_child,
    z_c_R_hgpID_transport_activities_costs_A.ticket_4_adults_without_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_5_adults_without_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_6_adult_with_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_7_adult_with_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_8_adults_with_children_without_car,
    z_c_R_hgpID_transport_activities_costs_A.ticket_9_adults_with_children_with_car,
    z_c_R_hgpID_transport_activities_costs_A.dog_price
    ORDER BY z_c_R_hgpID_transport_activities_costs_A.hgpID,
    z_c_R_hgpID_transport_activities_costs_A.zip_code,
    z_c_R_hgpID_transport_activities_costs_A.RecrID,
    z_c_R_hgpID_transport_activities_costs_A.transport_type_ID,
    z_c_R_hgpID_transport_activities_costs_A.activity_pattern,
    z_c_R_hgpID_transport_activities_costs_A.Time_segment;")




    # 9. Table: 'Final_Table' (127319 records, 70 columns) Required Basic Tables: 0 SQL
    # Tables: z_c_R_hgpID_transport_activities_costs_A (127319 records, 70 columns) join
    # properties: 0

    Final_table <- sqldf::sqldf("SELECT zip_code_Recr_hgpID_transport_activities_costs_B.hgpID,
    zip_code_Recr_hgpID_transport_activities_costs_B.RecrID,
    zip_code_Recr_hgpID_transport_activities_costs_B.zip_code,
    zip_code_Recr_hgpID_transport_activities_costs_B.transport_type_ID,
    zip_code_Recr_hgpID_transport_activities_costs_B.transport_type_definition,
    zip_code_Recr_hgpID_transport_activities_costs_B.activity_pattern,
    zip_code_Recr_hgpID_transport_activities_costs_B.activity_pattern_definition,
    zip_code_Recr_hgpID_transport_activities_costs_B.Time_segment,
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
    zip_code_Recr_hgpID_transport_activities_costs_B.number_of_adults,
    zip_code_Recr_hgpID_transport_activities_costs_B.number_of_children,
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
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_1_adult_without_children_without_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_2_adult_without_children_with_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_3_single_child,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_4_adults_without_children_without_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_5_adults_without_children_with_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_6_adult_with_children_without_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_7_adult_with_children_with_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_8_adults_with_children_without_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.ticket_9_adults_with_children_with_car,
    zip_code_Recr_hgpID_transport_activities_costs_B.dog_price,
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
    zip_code_Recr_hgpID_transport_activities_costs_B.difference_in_costs,
    zip_code_Recr_hgpID_transport_activities_costs_B.difference_in_max_time
    FROM zip_code_Recr_hgpID_transport_activities_costs_B
    ORDER BY zip_code_Recr_hgpID_transport_activities_costs_B.hgpID,
    zip_code_Recr_hgpID_transport_activities_costs_B.RecrID,
    zip_code_Recr_hgpID_transport_activities_costs_B.zip_code,
    zip_code_Recr_hgpID_transport_activities_costs_B.transport_type_ID,
    zip_code_Recr_hgpID_transport_activities_costs_B.activity_pattern,
    zip_code_Recr_hgpID_transport_activities_costs_B.Time_segment;")


    # 10. Table: 'data' 1075 records Required Basic Tables: 0 SQL Tables: Final_Table
    # (127319 records, 70 columns) join properties: 0

    data <- sqldf::sqldf("SELECT Final_table.RecrID, Final_table.activity_pattern, Final_table.transport_type_ID, Final_table.Time_segment
    FROM Final_table
    GROUP BY Final_table.RecrID, Final_table.activity_pattern, Final_table.transport_type_ID, Final_table.Time_segment
    ORDER BY Final_table.RecrID, Final_table.activity_pattern, Final_table.transport_type_ID, Final_table.Time_segment;")

    # 11. Table: 'ActInfo' 10 records Required Basic Tables: 0 SQL Tables: Final_Table
    # (127319 records, 70 columns) join properties: 0

    ActInfo <- sqldf::sqldf("SELECT Final_table.activity_pattern, Round(Final_table.maximum_number_of_times_per_activity) AS
    maximum_number_of_times_per_activity
    FROM Final_table
    GROUP BY Final_table.activity_pattern
    ORDER BY Final_table.activity_pattern, Round(Final_table.maximum_number_of_times_per_activity);")

    # 11. Table: 'TimeInfo' 10 records Required Basic Tables: 0 SQL Tables: Final_Table
    # (127319 records, 70 columns) join properties: 0

    TimeInfo <- sqldf::sqldf("SELECT Final_table.Time_segment, Round(Final_table.number_available_per_time_segment) AS
    number_available_per_time_segment
    FROM Final_table
    GROUP BY Final_table.Time_segment
    ORDER BY Final_table.Time_segment, Round(Final_table.number_available_per_time_segment);")

    list5 <- list(Final_table, data, ActInfo, TimeInfo, Basic_TableB)
    return(list5)
}

