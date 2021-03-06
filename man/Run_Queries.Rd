% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Run_Queries.R
\name{Run_Queries}
\alias{Run_Queries}
\title{function to RUN the queries from the tables included from the SQLite database}
\usage{
Run_Queries(dataset1)
}
\arguments{
\item{dataset1}{List of tables included in the SQLite database}
}
\value{
list5 Final_table, data, ActInfo, TimeInfo, Basic_TableB.
}
\description{
dataset1 List includes the Basic Tables
}
\details{
Run_Queries(dataset1)

'1 Table_2_RecrID_type_coordinates_prices' Recreation area definitions. 33 records Field Names: RecrID, recreation_area, Latitude_in_degrees,Longitude_in_degrees, type_of_recreation_areas,type_of_recreation_areas_definition, price_of_sights_or_park_per_child, price_of_parking Primary key: RecrID
'2 Table_3_RecrID_type_activity_pattern' Type of activities per recreation  area  213 records Field Names: RecrID, activity_pattern, recreation_areas,type_of_recreation_areas, type_of_recreation_areas_definition Primary key: RecrID Secondary key: activity_pattern
'3 Table_5_activities_definition_costs' Activities pattern definitions and  costs per activity 10 records Field Names: activity_pattern,  activity_pattern_definition, fixed_costs_per_activity, costs_per_hour_per_activity,  costs_per_person_per_activity,costs_per_hour_and_person_per_activity Primary key:activity_pattern
'4 Table_14_hgpID_groups_types' Houshold_types with their corresponding groups_types (recreation person) 50 records Field Names: hgpID, household_types,household_types_definition, group_types, group_types_definition, number_of_adults,  age_of_adults, number_of_children, age_of_the_child, dog, number_of_groups,number_of_persons Primary key: hgpID
'5 Table_16_hgpID_min_max_budgets' househod_types-groups_types (hgpID) with their corresponding budget per year and the amount of money paid out per day and year 50 records Field Names: hgpID, household_types_budget_per_year,number_of_groups, number_of_persons, maximum_budget_per_day,maximum_budget_per_year, Year Primary key: hgpID
'6 Table_7_zip_hgpID_budget' Combinations of Houshold_types with their corresponding groups_types (recreation person) per zip code 450 records Field
Names: hgpID, ID_origin_area, zip_code, household_types,household_types_definition, group_types, group_types_definition, Year,number_of_adults, number_of_children, number_of_groups, number_of_persons,maximum_budget_per_day, maximum_budget_per_year Primary key: hgpID Secondary key:ID_origin_area
'7 Table_9_zip_code_RecrID_distances' Combinations of zip codes, recreation areas and their corresponding transport types distances (in km) from zip codes to recreation areas, transport travel time in sec and transport speed inkm/hour 891 records Field Names: ID_origin_area, RecrID, transport_type_ID, zip_code, recreation_areas, distances_km, transport_travel_time_sec,transport_speed_km_per_hour Primary key: ID_origin_area Secondary keys: RecrID, transport_type_ID
'8 Table_8_hgpID_distance_costs' Combinations of household_types, group_types with their corresponding transport type, the percentage of the maximum time of the total time, the wait time per full travel, the minimum and maximum distance per  transport type, the speed per transport type and finaly the correspondings costs per transport type, fixed costs, per km costs per person costs and per km and  persons costs  119 records
Field Names: hgpID, transport_type_ID,  household_types, group_types, household_types_definition, group_types_definition,  maximum_of_total_time, wait_time_per_full_travel, minimum_distance_used,maximum_distance_used, speed_of_transport_type, fixed_costs_per_transport_type,per_km_costs_per_transport_type, per_person_costs_per_transport_type,  per_km_en_per_person_costs_per_transport_type Primary key: hgpID Secondary key:transport_type_ID
'9 Table_10_hgpId_activity_budget' Combinations of household_types, group_typeswith their corresponding activity pattern, the minimum and maximum number of times per activity and finaly the minimum and maximum amount of time to spend per   activity 250 records
Field Names: hgpID, activity_pattern, activity_pattern, household_types, household_types_definition, group_types,group_types_definition,  minimum_number_of_times_per_activity, maximum_number_of_times_per_activity,to_spend_minimum_time_per_activity, to_spend_maximum_time_per_activity Primary key: hgpID Secondary key: activity_pattern
'10 Time_segment_selected' 4 time segments are chosen with their corresponding time available per segment (in min) and the number of visits per time segment 79 records'
Field Names: RecrID, Time_segment, number_available_per_time_segment,Circle_segment, Time_available_per_time_segment Primary key: RecrID Secondary key:Time_segment
'11 zip_code_coordinates' The zip codes coordinates 9 records Field Names:zip_code, Latitude, Longitude Primary key:zip_code
Field Names:transport_type_ID,transport_type_definitions,transport_speed_km_per_hour,minimum_distance_per_transport_used,maximum_distance_per_transport_used   Primary key: transport_type_ID.
}
