#' Model ACRER (Wageningen_SQLite_2023)
#' Main function for the model ACRER.
#'
#' The model ACRER is a recreation model and simulates the number of persons per household and group types, the expected visiting duration as well as the number of visits from a zip code in a city to the destination area.
#'
#' @param db_choice Integer or character. 1 or "basic" for Basic tables, 2 or "complete" for Wageningen complete tables. Default is 1.
#' @param output_dir Character. Optional directory path to save outputs. Default NULL uses inst/extdata or tempdir.
#' @param interactive_ui Logical. If TRUE and in interactive session, prompts user with GUI dialogs. Default FALSE.
#' @param db_path Optional character. Direct file or directory path to SQLite DB file. Default NULL.
#' @return A list containing model results and pivot data frame.
#' @importFrom DBI dbGetQuery dbReadTable dbConnect dbListTables
#' @importFrom dplyr group_by summarise summarise_all summarize_all tally select distinct slice filter n n_distinct
#' @importFrom stats model.matrix
#' @importFrom utils head write.csv
#' @importFrom data.table setorder
#' @importFrom pracma ones
#' @importFrom quadprog solve.QP
#' @importFrom tibble as_tibble
#' @export
run_Acrer_model <- function(db_choice = 1, output_dir = NULL, interactive_ui = FALSE, db_path = NULL) {

    conn <- NULL
    complete <- "FALSE"

    choices <- if (identical(as.character(db_choice), "2") ||
                   tolower(as.character(db_choice)) %in% c("2", "complete", "wageningen_complete_tables")) 2 else 1

    if (isTRUE(interactive_ui) && interactive() && requireNamespace("svDialogs", quietly = TRUE)) {
      svDialogs::dlg_input(
          message = "Welcome to ACRER model. The first steps to run this model is:\n1. to define your working directory of ACRER package and\n2. to enter the Basic tables (15 tables) or the Wageningen_complete_tables (42 tables)\nfrom the sqlite database (with extension db ) from your data.\nIf you have only the Basic tables tables from the the sqlite database these\nwill be converted with the function Run_Queries to the complete tables.",
          default = "      Define working dir and database selection       ", rstudio = getOption("svDialogs.rstudio", TRUE), gui = .GUI)

      choicenr <- c(1, 2)
      choicetxt <- c("1. Basic tables", "2. Complete tables")
      menuchoices <- cbind(choicenr, choicetxt)
      print(menuchoices)
      Text_menuchoices <- svDialogs::dlg_list(menuchoices[, 1], preselect = as.character(choices), title = "Menu Choices",
                                              multiple = FALSE, gui = .GUI, rstudio = getOption("svDialogs.rstudio", TRUE))$res
      if (length(Text_menuchoices) > 0 && nzchar(Text_menuchoices)) {
        choices <- as.numeric(Text_menuchoices)
      }
    }

  # Resolve file helper:
  # 1. Explicit output_dir if specified
  # 2. In package development mode (if "inst/extdata" exists), write directly there
  # 3. Default to tempdir() for CRAN compliance
  out_base <- if (!is.null(output_dir) && nzchar(output_dir)) {
    output_dir
  } else if (dir.exists(file.path("inst", "extdata"))) {
    file.path("inst", "extdata")
  } else {
    tempdir()
  }

  get_out_path <- function(filename) {
    file.path(out_base, filename)
  }

  for (zx in choices) {
    if (zx == 1) { ## Basic tables choice
      conn <- wageningen_conn(db_choice = 1, db_path = db_path)
      dataset1 <- DBI::dbListTables(conn)
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

      my_basic_tables <- list(Table_10_hgpId_activity_time,Table_14_hgpID_groups_types,
        Table_15_Transport_Type_costs,Table_16_hgpID_min_max_budgets,
        Table_2_RecrID_type_coordinates_prices,Table_3_RecrID_activity_pattern,Table_5_activities_definition_costs,
        Table_7_zip_hgpID_budget,Table_9_zip_code_RecrID_distances,Time_segment_selected,hgpID_budget,
        hgpID_household_group_types,hgpID_transportID,zip_code_coordinates)
      save(my_basic_tables, file = get_out_path("dataset1.RData"))
      complete <- c("FALSE")
    } else if (zx == 2) { ## Wageningen_complete_tables choice
      conn <- wageningen_conn(db_choice = 2, db_path = db_path)
      data <- DBI::dbReadTable(conn,"data")
      Final_table <- DBI::dbReadTable(conn,"Final_table")
      ActInfo <- DBI::dbReadTable(conn,"ActInfo")
      TimeInfo <- DBI::dbReadTable(conn,"TimeInfo")

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
      save(Final_table,data,ActInfo,TimeInfo, file = get_out_path("dataset2.RData"))
      complete <- c("TRUE")
    }
  }

  if (complete == c("TRUE")) { # you don' have to run the queries
   } else {
    # household_types_definition,group_types_definition,number_of_adults,number_of_children,
    # number_of_groups,number_of_persons,age_of_adults,age_of_the_child,dog
    # Primary key: household_typesID
    hgpID_household_group_types <- dbGetQuery(conn, "SELECT * FROM hgpID_household_group_types")

    # 2 'hgpID_budget' 52 records of househod_types-groups_types (hgpID) with their corresponding budget per year and the
    # amount of money paid out per day and year
    # Field names: hgpID,household_types,household_types_definition,group_types,group_types_definition,
    # number_of_adults,number_of_children,number_of_groups,dog,number_of_persons,age_of_child,age_of_adults,
    # household_types_budget_per_year,maximum_budget_per_day,maximum_budget_per_year,Year
    # Primary key: hgpID
    hgpID_budget <- dbGetQuery(conn, "SELECT * FROM hgpID_budget")

    # 3 'Table_15_Transport_Type_costs' Transport type definitions (3 records with 11 variables)
    # Field names: transport_type_ID,transport_type_definition,maximum_of_total_time,wait_time_per_full_travel,
    # minimum_distance_used,maximum_distance_used,speed_of_transport_type,fixed_costs_per_transport_type,
    # per_km_costs_per_transport_type,per_person_costs_per_transport_type,
    # per_km_en_per_person_costs_per_transport_type
    # Primary key: transport_type_ID
    Table_15_Transport_Type_costs <- dbGetQuery(conn, "SELECT * FROM Table_15_Transport_Type_costs")

    # 4 'hgpID_transportID' 124 records of combination hgpID and transportID
    # Field names: hgpID,transport_type_ID
    # Primary keys: hgpID,transport_type_ID
    hgpID_transportID <- dbGetQuery(conn, "SELECT * FROM hgpID_transportID")

    # 5 'Table_5_activities_definition_costs' Activities pattern definitions and  costs per activity 10 records
    # Field names: activity_pattern,activity_pattern_definition,fixed_costs_per_activity,costs_per_hour_per_activity,
    # costs_per_person_per_activity,costs_per_hour_and_person_per_activity
    # Primary keys: activity_pattern
    Table_5_activities_definition_costs <- dbGetQuery(conn, "SELECT * FROM Table_5_activities_definition_costs")

    # 6 'Table_2_RecrID_type_coordinates'. Recreation area definitions. 33 records
    # Field Names: RecrID, recreation_area, Latitude_in_degrees,
    # Longitude_in_degrees, type_of_recreation_areas,
    # type_of_recreation_areas_definition,price_of_sights_or_park_per_adult,
    # price_of_sights_or_park_per_child,price_of_sights_or_park_per_dog,
    # price_of_parking
    # Primary key: RecrID
    Table_2_RecrID_type_coordinates_prices <- dbGetQuery(conn, "SELECT * FROM Table_2_RecrID_type_coordinates_prices")

    # 7 'zip_code_coordinates' The zip codes coordinates 9 records
    # Field Names:zip_code, Latitude, Longitude
    # Primary key:zip_code
    zip_code_coordinates <- dbGetQuery(conn, "SELECT * FROM zip_code_coordinates")

    # 8 'Table3_Recr_activity_pattern_costs' Type of activities per recreation  area  221 records
    # Field Names: RecrID, activity_pattern, activity_pattern_definition, recreation_areas,type_of_recreation_areas,
    # type_of_recreation_areas_definition,period,fixed_costs_per_activity,costs_per_hour_per_activity,
    # costs_per_person_per_activity,costs_per_hour_and_person_per_activity,Latitude_in_degrees,Longitude_in_degrees,
    # price_of_sights_or_park_per_adult,price_of_sights_or_park_per_child,price_of_sights_or_park_per_dog,price_of_parking
    # Primary keys: RecrID,activity_pattern
    # Table3_RecrID_type_activity_pattern = RcrID_activity_pattern'
    Table_3_RecrID_activity_pattern <- dbGetQuery(conn, "SELECT * FROM Table_3_RecrID_activity_pattern")

    # 9 'Table7_zip_hgpID_budget' 468 records with 12 variables
    # Field Names: zip_code,hgpID,ID_origin_area,household_types,household_types_definition,group_types,group_types_definition,
    # number_of_adults,number_of_children,number_of_persons,dog,number_of_groups
    # Primary key:hgpID
    Table_7_zip_hgpID_budget <- dbGetQuery(conn, "SELECT * FROM Table_7_zip_hgpID_budget")

    # 10 'Table_9_zip_code_RecrID_distances' Combinations of zip codes, recreation
    # areas and their corresponding transport types distances (in km) from zip codes to
    # recreation areas, transport travel time in sec and transport speed in km/hour. 891 records
    # Field Names: RecrID,zip_code,transport_type_ID,ID_origin_area,recreation_areas,distances_km,transport_travel_time_sec,
    # transport_speed_km_per_hour
    # Primary keys: RecrID,, "zip_code", "transport_type_ID"
    Table_9_zip_code_RecrID_distances <- dbGetQuery(conn, "SELECT * FROM Table_9_zip_code_RecrID_distances")

    # 11 'Table_10_hgpID_activity_time'. Combinations of household_types, group_types
    # with their corresponding activity pattern, the minimum and maximum number of times
    # per activity and finaly the minimum and maximum amount of time to spend per activity.260 records
    # Field Names: hgpID, activity_pattern,household_types,household_types_definition,
    # group_types, group_types_definition,minimum_number_of_times_per_activity, maximum_number_of_times_per_activity,
    # to_spend_minimum_time_per_activity, to_spend_maximum_time_per_activity
    # Primary keys: hgpID,activity_pattern
    Table_10_hgpID_activity_time <- dbGetQuery(conn, "SELECT * FROM Table_10_hgpId_activity_time")

    # 12 'Time_segment_selected'. 4 time segments are chosen with their corresponding
    # time available per segment (in min) and the number of visits per time segment 80 records'
    # Field Names: RecrID, Time_segment,number_available_per_time_segment,Circle_segment,Time_available_per_time_segment
    # Primary keys:RecrID,Time_segment
    Time_segment_selected <- dbGetQuery(conn, "SELECT * FROM Time_segment_selected")

    # 13 'Table_14_hgpID_groups_types'. Household_types with their corresponding
    # groups_types (recreation person(s) with or without a dog) 52 records'
    # Field Names:hgpID, household_types, household_types_definition, group_types,
    # group_types_definition, number_of_adults, age_of_adults, number_of_children,
    # age_of_the_child, dog, number_of_groups, number_of_persons
    # Primary key: hgpID
    Table_14_hgpID_groups_types <- dbGetQuery(conn, "SELECT * FROM Table_14_hgpID_groups_types")

    # 14 'Table 16 hgpID with min_max_budgets'. Number of records 52
    # Field Names:hgpID,household_types,household_types_definition,group_types,group_types_definition,
    # number_of_children,number_of_persons,household_types_budget_per_year,maximum_budget_per_day,maximum_budget_per_year,Year
    # Primary key: hgpID
    Table_16_hgpID_min_max_budgets <- dbGetQuery(conn, "SELECT * FROM Table_16_hgpID_min_max_budgets")

  # The Basic Tables can be displayed by Running the presentation of the
  # Basic_Tables.Rmd, in the R markdown file
  # Run the different queries from the Basic tables (dataset1 list of Basic tables) in
  # order to obtain the Finaltable by using the function 'Run_Queries'
  #    dataset1 <- readRDS("data/raw_data/dataset1.rds")

  list5 <- Run_Queries(my_basic_tables, conn = conn) # go to run the function Run_Queries which requires the list of Basic tables
  # After running the queries we get the Final_table, data, ActInfo and Time_Info
  # tables from the list5 and we save them in the file:dataset2.Rdata The SQL-created
  # Tables can be displayed by Run the presentation of the SQL_created_tables.Rmd, R
  # markdown file


  Final_table <- list5[[1]]
  data <- list5[[2]]
  ActInfo <- list5[[3]]
  TimeInfo <- list5[[4]]
  list_files <- list5
  save(Final_table,data,ActInfo,TimeInfo, file = get_out_path("dataset2.RData"))
}


# use the R Utils Package for head and str use the base package for the Object
# Classes


head(data, 10)
class(data) # (1092 x 4)

# The data set (data) is defined by the combinations of the variables: RecrID,
# activity pattern, Time_segment and transport_type_ID The transport_type_ ID
# variable is not important in the analysis.  In the analysis only the unique
# combinations of the variables: RecrID, activity_patern and Time_segment are
# needed.  However, the shorter vector used by these three variables being
# determined to expand again is the design vector of these three variables needed
# cdata: (515 x 4) (see Excel workbook: Wag_documentation_English,
# sheet:crostab_procedure) The presence of the RecrID on the combinations of
# activity_pattern and Time_segment (34 combinations) are given by the variable
# groupsRecr_at by using the function crosstab.  These groups we further used for
# the quadratic programming optimisation procedure

cdata <- data %>% group_by(RecrID, activity_pattern, Time_segment) %>%
  summarise(Counts_transport_type_ID = n()) #515 x 4
RmatrixA <- as.matrix(x = cdata[, 1], dimnames = list(NULL, "RecrID"))
CmatrixA <- as.matrix(x = cdata[, 2:3], dimnames = list(NULL, "activity_pattern", "Time_segment"))
C1C <- vector("numeric")
# the function crosstab construct a matrix of 33 rows (the number of RecrID) and 34
# columns (the combinations of activity_pattern and Time_segment) with the
# present/absence of the RecrID per combination where the sum of each row is greater
# than zero After that this function computes the groups with the same activities in
# the same time segments per destination area (see Wag_documentation_English.xlsx,
# Excel sheet: crostab_procedure)

list10 <- crosstab(RmatrixA, CmatrixA, C1C) ## RecrID,activity_pattern, Time_segment,groupsRecr_at

f9 <- as.matrix(list10[[4]]) ## groupsRecr_at
cdata <- as.matrix(cdata)
cdata <- cbind(cdata, f9) ## 515 x 5
colnames(cdata)[colnames(cdata) == ""] <- "groupsRecr_at"
data1 <- cbind(cdata[, 1], cdata[, 5]) ## RecrID, groupsRecr_at (515 x 2)
data1 <- as.data.frame(data1)
names(data1)[1:2] <- c("RecrID", "groupsRecr_at")
df <- data1 ## RecrID,groupsRecr_at

# use the library dpryl for the count/tally observations by group

z <- df %>% group_by(RecrID, groupsRecr_at) %>% summarize_all(dplyr::n_distinct)
z1 <- z %>% group_by(groupsRecr_at) %>% tally()
## 10 kategories (1,2,3,4,6,7,16,25,34,82)
names(z1)[2] <- c("number of locations")
z1 <- data.frame(z1)
data1 <- cbind(cdata[, 5], cdata[, 2], cdata[, 3])
data1 <- as.data.frame(data1)
names(data1)[1:3] <- c("groupsRecr_at", "activity_pattern", "Time_segment")
data2 <- group_by(data1, groupsRecr_at, activity_pattern, Time_segment)
data2 <- tally(data2, sort = TRUE)
names(data2)[4:4] <- c("number_of_locations")
data2 <- data.frame(data2)
data2 <- data2[order(data2$groupsRecr_at, data2$activity_pattern, data2$Time_segment),]
RmatrixB <- as.matrix(x = data2[, 1:2], dimnames = list(NULL, "groupsRecr_at", "activity_pattern"))
CmatrixB <- as.matrix(x = data2[, 3], dimnames = list(NULL, "Time_segment"))
C1B <- vector("numeric")


list1 <- crosstab(RmatrixB, CmatrixB, C1C)

groupsRecr_act_ts <- as.matrix(list1[[4]])
Table <- as.matrix(list1[[3]]) # (48 x 4) matrix (presence-absence: groupsRecr_act_ts)

# data2 groupsRecr_at, activity_pattern, TimeSegment, number_of_locations,
# groupsRecr_act_ts

data2 <- cbind(as.matrix(data2), groupsRecr_act_ts)
data2 <- as.data.frame(data2)
names(data2)[5] <- c("groupsRecr_act_ts")

# select groupsRecr_at(1), activity_pattern(2), groupsRecr_act_ts(5),
# first(NumberOfLocations(4)) from data2 groupby
# groupsRecr_at(1),activity_pattern(2),groupsRecr_act_ts(5) use the library dpryl

data3a <- data2 %>% group_by(groupsRecr_at, activity_pattern,groupsRecr_act_ts, number_of_locations)
data33 <- select(data3a,!(3))
data3b <- distinct(data33)
Gdata3 <- group_by(data3b, groupsRecr_at, number_of_locations)
Gdata3 <- dplyr::select(Gdata3, -(activity_pattern), -(groupsRecr_act_ts))
Gdata3 <- dplyr::summarise_all(Gdata3, dplyr::n_distinct)
totaal_aantal_locaties = sum(Gdata3[, 2])

NumberAvail <- TimeInfo ##15, 10, 4.9, 3
NumberAvail <- dplyr::select(NumberAvail, c(-1))
groupsRecr_act_ts <- factor(as.matrix(data3b[,4])) ## 1, 4, 37 (3 categories)
# the number of levels of the factor groupsRecr_act_ts
levels3 <- rev(as.numeric(levels(groupsRecr_act_ts)))

# Construction of the design matrices: matrix design2 with the use of the
# model.matrix function from the stats package Insert - 1 in the formula:
# ~factivity_pattern to suppress the intercept (see Wag_documentation_English.xlsx,
# Excel sheet: timeMax and quadprog)

factivity_pattern <- factor(as.matrix(data3b[, 2])) ## levels 1:10
design2 <- model.matrix(~factivity_pattern - 1) ## 48 x 10 = design matrix of activity pattern without intercept
design2 <- as.data.frame(design2)
design2 <- t(design2) ## 10 x 48

# Construction of the design matrices: matrix design3 with the use of the
# model.matrix function from the stats package Insert - 1 inthe formula:
# ~fgroupsRecr_act_ts to suppress the intercept

design3 <- model.matrix(~groupsRecr_act_ts - 1) ## 48 x 3 (1, 4, 37) = design matrix of fgroupsRecr_act_ts
design3 <- as.data.frame(design3)

design3 <- t(design3) ## 3 x 48


# constraints on available time c3*design3 <= timeMax
# select the hulp combination matrix c3 (9 x 3) for the construction of the timeMax vector
# Intialization
Matrix_timeMax <- matrix(ncol = 0, nrow = 0)
zcount <- 1
number <- vector(mode = "logical", length = 0)
groupsRecr_act_ts_levels <- length(levels3)
number_in_square <- groupsRecr_act_ts_levels * groupsRecr_act_ts_levels
total_numbers <- groupsRecr_act_ts_levels * groupsRecr_act_ts_levels
c3 <- matrix(rep(0, total_numbers), nrow = number_in_square, ncol = groupsRecr_act_ts_levels)
timeMax <- matrix(c(rep(0, number_in_square)), ncol = 1, nrow = number_in_square)
TemptimeMax <- matrix(c(rep(0, number_in_square)), ncol = 1, nrow = number_in_square)
TempMatrix <- matrix(c(rep(0, number_in_square)), nrow = number_in_square)
icount <- matrix(c(rep(1, groupsRecr_act_ts_levels)), ncol = 1)
jcount <- matrix(c(rep(1, groupsRecr_act_ts_levels)), ncol = 1)

while (zcount < 5001) {
  if (groupsRecr_act_ts_levels == 2) {
    list50 <- combinations(groupsRecr_act_ts_levels)
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
  } else if (groupsRecr_act_ts_levels == 3) {
    list50 <- combinations(groupsRecr_act_ts_levels)
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
    list50 <- combinations(groupsRecr_act_ts_levels)
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

  data6 <- cbind(Table,(data3b[,4]))
  data6 <- as.data.frame(data6[, c(5, 4, 3, 2, 1)])  ## 48 x 5 (groupsRecr_act_ts,V4,V3,V2,V1)
  data7 <- group_by(data6, groupsRecr_act_ts)
  data7 <- unique(data7) ## 3 x 4 (groupsRecr_act_ts levels,1, 2, 3, 4)
  data7 <- as.matrix(data7)
  colnames(data7) <- c()
  colnames(c3) <- c()
  data7 <- apply(data7, 2, as.numeric)
  data8 <- data7[, 2:dim(data7)[2]] #(3x4)
  NumberAvail <- as.matrix(NumberAvail) #(4x1)
  NumberAvail <- apply(NumberAvail, 2, as.numeric)
  size1 <- dim(Matrix_timeMax)
  # construction of timeMax vector

  p <- c3 %*% data8
  p <- p > 0

  tMT <- p %*% NumberAvail
  sumtMT <- sum(tMT)
  if (nrow(tMT) == number_in_square) {
    if ((size1[1] == 0) & (size1[2] == 0)) {
      Matrix_timeMax <- tMT
      zcount <- zcount + 1
    } else {
      Matrix_timeMax <- cbind(Matrix_timeMax, tMT)
      zcount <- zcount + 1
    }
  }
}

TempMatrix <- TempMatrix[, -1] # matrix of c3 generation(9 x 12)
TempMatrix <- t(TempMatrix) # matrix of c3 generation(12 x 9)
Matrix_timeMax_H <- data.frame(t(Matrix_timeMax)) %>% slice(rep(1:n(), each = groupsRecr_act_ts_levels)) #generation of the timeMaxmatrices x number_of_time_segments
Sum_timeMax <- rowSums(Matrix_timeMax_H)
Matrix_timeMax_new <- cbind(TempMatrix, Matrix_timeMax_H, Sum_timeMax)
#TempMatrix <- cbind(TempMatrix, Matrix_timeMax_new) # combination of c3 matrices with timeMax matrices
Matrix_timeMax_new <- setorder(Matrix_timeMax_new, -Sum_timeMax)
#SizeMatrix_timeMax_new <- dim(Matrix_timeMax_new)
#numberMatrix_timeMax_new <- (1:SizeMatrix_timeMax_new[[1]])
#TempMatrix <- cbind(numberMatrix_timeMax_new, numberMatrix_timeMax_new)
c3_def <- as.data.frame(Matrix_timeMax_new[1:groupsRecr_act_ts_levels, 1:(groupsRecr_act_ts_levels*groupsRecr_act_ts_levels)])
c3_def <- t(c3_def)
p <- c3_def %*% data8
plog <- p > 0
lower <- (groupsRecr_act_ts_levels*groupsRecr_act_ts_levels) + 1
upper <- lower + ((groupsRecr_act_ts_levels*groupsRecr_act_ts_levels) - 1)
timeMax <- as.data.frame(Matrix_timeMax_new[lower,lower:upper])

ActMax <- dplyr::select(ActInfo, 2)
ActMax <- as.numeric(unlist(ActMax))

# constraints on activity design2*n<=ActMax and -design2*n<=0*ActMax

ZActMax <- 0.0 * ActMax # 0,0,0...

# coefficients of the linear constrains
timeMax <- t(as.matrix(timeMax))
timeMax_1 <- as.matrix(timeMax[, 1])
ActMax <- as.matrix(ActMax)
ZActMax <- as.matrix(ZActMax)
lin_cond_max <- rbind(timeMax_1, ActMax, ZActMax)
c3_design3 <- as.matrix(c3) %*% design3 ## c3 matrix (9 x 3) * design3 matrix (3 x 48) = 9 x 48 matrix
lin_mat <- rbind(c3_design3,design2,-design2)
lin_mat <- as.matrix(lin_mat)
rownames(lin_mat) <- c()

# construction of the Hessian matrix: HessianMatrix

# as much as possible spread of the destinations per activity Ni is a vector (10 x
# 1) constructed by design2 (10 x 51) * data3b(number_of_locations) (51 x 1)
Ni <- design2 %*% as.matrix(data3b[, 4])
rownames(Ni) <- c()

# number of the number of locations vector loc: (48 x 1) reciprocal of of the number
# of locations vector loc_1: (48 x 1)
loc <- as.matrix(data3b[, 4]) # 48 (number of locations)
loc_1 <- 1 / loc # eg. 1/5 = 0.2
m1 <- as.matrix(diag(c(loc_1))) ## the vector loc_1 = 1/number of locations (Hessian matrix)
ncoldesign2 <- dim(design2)[2]
## m2 <- as.matrix(t(-design2) %*% diag(c(1 / Ni)) %*% design2)
design0 <- as.matrix(ones(ncoldesign2, 1))

# as much as possible spread of the visits per activity_pattern_type construction of
# the design matrix: design1 (10 x 48)

fgroupsRecr_at <- factor(as.matrix(data3b[, 1])) ## groupRecr_at (48)
design1 <- model.matrix(~fgroupsRecr_at - 1)
rownames(design1) <- c()
design1 <- t(design1) ## 10 x 48 matrix
sumdesign1 <- rowSums(design1)
sumdesign1 <- as.matrix(sumdesign1)
colnames(sumdesign1) <- c()
design1 <- design1[,] / sumdesign1[,] ## 10 x 48 matrix
rownames(design1) <- c()
# the proportion of the number of locations per level groupsRecr_at
Li <- design1 %*% as.matrix(data3b[, 4]) ## (10 x 48)=design1 x (48 x 1)=number of locations -> 10 x 1 vector
design1 <- design1 > 0 ## 10 x 48 matrix
m2 <- as.matrix(t(design1) %*% diag(c(1 / Li)) %*% design1) ## (48 x 10) x (10 x 10) x (10 x 48) -> (48 x48) (Hessian matrix)
nTot <- sum(Li)
# number of destination visits
## m4 <- as.matrix(-design0 %*% (1 / nTot) %*% t(design0))

# percentage of the maximum visits
sumMi <- sum(ActMax)
SizeActMax <- length(ActMax) ## = 10
dia5 <- diag(c(ones(SizeActMax, 1))) / sumMi / sumMi
m3 <- as.matrix(t(design2) %*% dia5 %*% design2) ## (Hessian matrix) (48 x48)
g11 <- t((as.matrix(ActMax)) %*% (1 / (sumMi ^ 2))) ## (1 x 10)
l1 <- 2 %*% g11 %*% design2 ## (1 x 10) x (10 x 48) -> (1 x 48)
m4 <- as.matrix(design0 %*% g11 %*% as.matrix(ActMax) %*% t(design0)) ## (48 x 1) x (1 x 10) x (10 x1) x (1 x 48) -> (48 x 48) (Hessian matrix) (48 x48)

## maximum visits

maxVisits <- sumMi
m5 <- as.matrix(design0 %*% t(design0)) ## (48 x 1) x (1 x 48) = (Hessian matrix) (48 x48)
# l2 = (1 x 1) x (48 x 1) = 48 x 1
l2 <- -2 * sumMi * design0
c1 <- sumMi ^ 2

# Quadratic optimisation: implimentation of the dual method of Goldfarb and Idnani
# (1982-83) for solving quadratic problems of the form: min(-dvec'*x +
# 1/2*(x'Dmat*x) such that Amat'*x >= bvec the matrix Amat and the vector bvec are
# the coefficients of the linear constraints

# HessianMatrix (Dmat) is symmetric (48 x 48)

HessianMatrix <- as.matrix((m1 + m2 + m3 + m4 + m5) / 2)

Dmat <- HessianMatrix # Hessian matrix to be minimized (48 x 48)
sizeH <- dim(Dmat)
Adiag <- diag(-1, sizeH[2]) # diagonal matrix of -1 for the matrix extension Amat (48 x 48)

f10 <- as.matrix((t(l1)) * 1 / 2 + l2)
f10L <- length(f10)
bvec0 <- c(rep(0, f10L)) # vector of length 51 with zeros for the extension of the vector of linear constraints

# vector dvec is the set of coefficients of the quadratic objective function to be
# minimized
dvec <- -f10
dvec <- t(dvec) # matrix (1 x 48)
lb <- as.matrix(0 * t(design0)) ## b
# the matrix Amat and the vector bvec are the coefficients of the linear constraints
Amat <- lin_mat # matrix Amat contains the coefficients of the linear constrains (29 x 48)
# extension of the matrix Amat (matrix defining the constraints + transpose of the
# diagonal matrix)

Amat <- rbind(Amat, Adiag) # matrix Amat (29 + 48 = 48 x 77)
Amat <- -Amat # matrix Amat = -Amat
Amat <- t(Amat) # transpose -Amat  (48 x 77)

## The vector bvec
bvec <- as.vector(lin_cond_max, mode = "any") ## vector of the coefficients of the linear constraints (1:29)
bvec <- c(bvec, bvec0) ## Extension of the vector with the vector bvec0 (48 x 1) with zeros (77 x 1)
bvec <- -bvec # bvec is -bvec

# determine the distribution over time segments by using solve.QP
qp <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec)
n <- qp$solution
value <- qp$value
uncontrained_solution <- qp$uncontrained.solution
iterations <- qp$iterations
Lagrangian <- qp$Langrangian
iact <- qp$iact
n <- matrix(n) # qp solution


groupsRecr_act_ts_presence_absence <- list1[[3]] ## (48 x 4) vector
ndim <- dim(groupsRecr_act_ts_presence_absence) ## (48 x 4)
# number of rows = 48 * 4 = 192
numberofrows <- ndim[1] * ndim[2]
d1 <- matrix(t(groupsRecr_act_ts_presence_absence), nrow = numberofrows, ncol = 1)
h1 <- data.frame(x = 1:ndim[1]) # 1:48
h2 <- data.frame(y = 1:ndim[2]) # 1:4
# function expand.grid: create a data frame from all combinations of the supplied
# vectors or factors
d2 <- expand.grid(t(h2), t(h1)) # 48 x 4 = 192
d3 <- cbind(d2, d1) ## 192 x 3
d3 <- cbind(d3[2], d3[1], d1) ## (192 x 3) units, time_segment, groupsRecr_act_ts_presence_absence
hd3 <- tibble::as_tibble(d3)
amatrix <- hd3 ## (192 x 3)
class(amatrix)
amatrix <- filter(amatrix, d1 > 0) ## (133 x 3) matrix (units, time_segment, groupsRecr_act_ts_presence_absence)
dimamatrix <- dim(amatrix) ## 133 x 3
fa <- factor(as.matrix(amatrix[, 1])) ## factor units (levels) (1:48)
fb <- factor(as.matrix(amatrix[, 2])) ## factor time segments (levels) (1:4)
z11 <- model.matrix(~fa - 1) ## (48 x 133) matrix
z11 <- data.frame(z11)
z11 <- t(z11) ## (48 x 133)
z12 <- model.matrix(~ fb - 1)
z12 <- data.frame(z12)
z12 <- t(as.matrix(z12))  ## (4 x 133) matrix
cvec <- c(rep(1, dimamatrix[1])) # array of ones (133 x 1)
linm <- rbind(z11, z12) # size: 52 x 133
linmax <- as.matrix(c(n, NumberAvail[1], NumberAvail[2], NumberAvail[3], NumberAvail[4])) # size: 52 x 1
# (n=48 + (15,10,5,3)) (52 x 1)

# solution of qp plus NumberAvail

bvecL <- linmax # qp solution + NumberAvail[1:4]

# Type of problem: 1 = maximization problem and -1 = minimization problem, c = cvec coeeficients of the objective
# function bM = bvecL The vector corresponding to the right hand constants (with <=)
# M = linm the coefficient of the problem constraints matrix (with > =)

solve5 <- interior_point(t = -1, c = cvec, bM = bvecL, M = linm, e = 1e-04)

sol5 <- solve5[2] ## 133 x 1
sol5 <- data.frame(sol5)  ## 133 x 1

RmatrixC <- as.matrix(x = amatrix[, 1]) #units = 133
CmatrixC <- as.matrix(x = amatrix[, 2]) # time_segment = 133
C1C <- as.matrix(sol5) # sol5 (133 x 1)
list15 <- crosstab(RmatrixC, CmatrixC, C1C)
#rm(RmatrixC, CmatrixC, C1C)
nt <- list15[[3]] ## (48 x 4)
colnames(nt)[1:4] = c(1, 2, 3, 4)
nt1 <- t(nt) ## (4 x 48)


result1a <- reshape2::melt(nt) # units,Time_segment,number_of_visits
colnames(result1a) <- c("units", "Time_segment", "number_of_visits")

result1a <- data.frame(result1a)
result1a <- result1a[order(result1a$units),]

x3 <- c(1:dim(data3b)[1]) ## 1:48
data9 <- as.data.frame(matrix(c(rep(0, dim(data3b)[1])), nrow = dim(data3b)[1])) ## groupsRecr_at dim(48 x 1)
mm2 <- matrix(data = NA, ncol = dim(data3b)[2]) ## activity_pattern dim(48 x 1)
for (i in x3) {
  mm2 <- as.data.frame(rep_row(data3b[i,], dim(data3b)[2])) ## i = 1:48
    if (i == 1) {
      data9 <- mm2
    } else {
      data9 <- rbind(data9, mm2)
    }
  }

result <- cbind(data9, result1a[,2], result1a[,3])
colnames(result) <- c("groupsRecr_at", "activity_pattern", "groupsRecr_act_ts", "nrOfLocations",
                      "Time_segment", "number_of_visits")

# result <- as.matrix(result)
## result <- filter(as.data.frame(result), result[,6] > 1e-06)
# result <- as.matrix(result)
# result3 <- as.matrix(result3)
cdata <- cdata[order(cdata[,5], decreasing = FALSE),]
cdata <- data.frame(cdata)
result <- data.frame(result)
result1 <- merge(x = cdata, y = result, by = intersect(c("groupsRecr_at", "activity_pattern","Time_segment"), c("groupsRecr_at", "activity_pattern","Time_segment")))
result1 <- data.frame(result1)
result1 <- filter(as.data.frame(result1), result1$number_of_visits > 1e-06)
result1 <- as.matrix(result1)

# with the personal_costs and minimum and maximum time per activitity we  can
# estimate the expected visit duration per activity see excel workbook:
# Wag_documentation_English.xlsx, excel sheet: Wag_results_Z0-2023 and
# Wageningen_solution_2017_0

personal_costs <- Final_table$total_activities_costs
min_duration_abs <- Final_table$to_spend_minimum_time_per_activity
max_duration_abs <- Final_table$to_spend_maximum_time_per_activity
costs_per_min <- personal_costs / max_duration_abs
available_time_for_activity <- Final_table$Time_available_per_time_segment - (2 * Final_table$transport_travel_time_total)
maximum_duration_as_a_result_of_costs <- rep(NaN, length(available_time_for_activity))
x <- c(1:length(available_time_for_activity))
for (i in x) {
  if (personal_costs[i] == 0) {
    maximum_duration_as_a_result_of_costs[i] <- available_time_for_activity[i]
  } else {
    maximum_duration_as_a_result_of_costs[i] <- ((60 * (Final_table$maximum_budget_per_day[i] -
            Final_table$travel_costs[i])) / (personal_costs[i]))
  }
}
step1 <- pmin(max_duration_abs, maximum_duration_as_a_result_of_costs)
minimum_duration <- pmax(as.matrix(Final_table$maxduration), as.matrix(min_duration_abs))
maximum_duration <- pmin(as.matrix(available_time_for_activity), as.matrix(step1))
dMinDurationAbs <- min_duration_abs
dMaxDurationAbs <- max_duration_abs
dMinDuration <- minimum_duration
dMaxDuration <- maximum_duration
list11 <- expected_visit_duration(dMinDurationAbs, dMaxDurationAbs, dMinDuration, dMaxDuration)
expected_visiting_duration <- list11[[1]]

expected_costs <- Final_table$travel_costs + (costs_per_min * expected_visiting_duration)
remnant_time <- available_time_for_activity - expected_visiting_duration
Final_table <- data.frame(Final_table)

Ndata <- cbind(Final_table, personal_costs, min_duration_abs, max_duration_abs, costs_per_min,
    available_time_for_activity, maximum_duration_as_a_result_of_costs, step1, minimum_duration,
    maximum_duration,expected_visiting_duration, expected_costs, remnant_time)

Ndata <- data.frame(Ndata)

Ndata10 <- merge(x = Ndata, y = result1, by = intersect(c("RecrID","activity_pattern", "Time_segment"), c("RecrID", "activity_pattern", "Time_segment")), sort = TRUE)


Ndata10 <- as.matrix(Ndata10)
Ndata10 <- as_tibble(Ndata10)

fdata15 <- filter(Ndata10, (number_of_visits > 0.000001))
fdata15 <- filter(fdata15, expected_visiting_duration > 0)
fdata16 <- filter(Ndata10, expected_visiting_duration == 0)
Ndata10[] <- lapply(Ndata10, unlist)
fdata15 <- data.frame(fdata15)
fdata15[] <- lapply(fdata15, unlist)
fdata16 <- as.data.frame(fdata16)
fdata16[] <- lapply(fdata16, unlist)
fdata20 <- fdata15 %>% select(RecrID, activity_pattern_definition,transport_type_ID,transport_type_definition,Time_segment,zip_code,recreation_areas,Time_available_per_time_segment,distances_km,total_activities_costs,fuel_cost_total,expected_visiting_duration,number_of_visits)
write.csv(fdata20, file = get_out_path("fdata20.csv"), row.names = FALSE)


c3 <- c3_def
rows <- c(1:length(c3[, 1]))
c3 <- cbind(rows, c3_def)
rownames(c3) <- c()
colnames(c3) <- c("rows", "col1", "col2", "col3")
c3 <- data.frame(c3)


rows <- c(1:length(p[, 1]))
p <- cbind(rows, p)
rownames(p) <- c()
rownames(p) <- c()
colnames(p) <- c("rows", "col1", "col2", "col3", "col4")
p <- data.frame(p)


timeMax <- data.frame(timeMax)
rows <- c(1:length(timeMax[, 1]))
timeMax <- cbind(rows, timeMax)
rownames(timeMax) <- c()
colnames(timeMax) <- c("rows", "timeMax")


rows <- c(1:length(data7[, 1]))
data7 <- cbind(rows, data7)
rownames(data7) <- c()
colnames(data7) <- c("rows", "groupsRecr_act_ts", "col1", "col2", "col3", "col4")
data7 <- data.frame(data7)

NumberAvail <- data.frame(TimeInfo)
rownames(NumberAvail) <- c()
colnames(NumberAvail) <- c("Time segment", "NumberAvail")

rows <- c(1:length(lin_cond_max[, 1]))
lin_cond_max <- cbind(rows, lin_cond_max)
rownames(lin_cond_max) <- c()
colnames(lin_cond_max) <- c("rows", "col1")
lin_cond_max <- data.frame(lin_cond_max)


activity_pattern <- Table_5_activities_definition_costs[, 1]
activity_pattern <- matrix(activity_pattern)
l11 <- 2 %*% g11
l11 <- matrix(l11)
g11 <- matrix(g11)
activity_pattern <- data.frame(activity_pattern)


Table_l1 <- cbind(activity_pattern, ActMax, g11, l11)


rows <- c(1:length(qp$solution))
qp_solution <- cbind(rows, qp$solution)
rownames(qp_solution) <- c()
colnames(qp_solution) <- c("rows", "qp_solution")



rows <- c(1:length(sol5$The.optimal.solution.X))
linear_solution <- cbind(rows, amatrix$Var2, amatrix$Var1, sol5$The.optimal.solution.X)
rownames(linear_solution) <- c()
colnames(linear_solution) <- c("rows", "units", "time_segment", "linear_solution")

#list_files3 <- list(data7,c3,p,NumberAvail,timeMax,lin_cond_max,Table_l1,qp_solution,linear_solution)
#save(data7,c3,p,NumberAvail,timeMax,lin_cond_max,Table_l1,qp_solution, file = "dataset3.RData")
save(data7,c3,p,NumberAvail,timeMax,lin_cond_max,Table_l1,qp_solution,linear_solution, file = get_out_path("dataset3.RData"))

    svDialogs::dlg_input(
      message = "Welcome to ACRER model. 1. Zip codes of Wageningen city, 2. Recreation areas, 3. EXIT",
      default = "type 3 for EXIT",
      rstudio = getOption("svDialogs.rstudio", TRUE)
    )

    menuchoices <- cbind(c(1, 2, 3), c("1. Zip codes", "2. Recreation areas", "3. Exit"))
    res <- svDialogs::dlg_list(menuchoices[, 1], preselect = NULL, title = "Menu Choices",
                               multiple = FALSE, rstudio = getOption("svDialogs.rstudio", TRUE))$res
    if (length(res) > 0) {
      map_choice <- as.numeric(res)
      if (identical(map_choice, 1)) {
        zip_code <- c("6701", "6702", "6703", "6704", "6705", "6706", "6707", "6708", "6709")
        name <- c("Centrum", "Nude/Wageningen-West", "Wageningen-West", "Wageningen-Hoog", "Noord", "Noord-West", "Industrieterrein", "Tarthorst", "Rijnsteeg")
        lat <- c(51.968, 51.965, 51.961, 51.975, 51.978, 51.982, 51.970, 51.965, 51.958)
        lon <- c(5.662, 5.655, 5.648, 5.670, 5.678, 5.665, 5.690, 5.705, 5.675)
        wageningen_data <- data.frame(zip_code, name, lat = as.numeric(lat), lon = as.numeric(lon))
        if (requireNamespace("sf", quietly = TRUE) && requireNamespace("mapview", quietly = TRUE)) {
          wag_sf <- sf::st_as_sf(wageningen_data, coords = c("lon", "lat"), crs = 4326)
          html_fl1 <- tempfile(fileext = ".html")
          pal <- mapview::mapviewGetOption("raster.palette")
          m <- mapview::mapview(wag_sf, zcol = "zip_code", label = "name",
                                map.types = "OpenStreetMap",
                                col.regions = pal,
                                layer.name = "zip_code name")
          mapview::mapshot(m, html_fl1)
          if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
            rstudioapi::viewer(html_fl1)
          }
        }
      } else if (identical(map_choice, 2)) {
        recreation_areas <- c("Het Depot", "Arboretum Belmonte", "W Berg en Bergpad", "de Dreijen", "Uitwaarden W", "RenkumsBeekdal", "Grebbeberg", "De Blauwe Kamer", "Ouwehands Dierenpark", "Kasteel Doorwerth", "Edese bos", "Airborne hartenstein", "Westerrode", "Landgoed Kernhem", "Wekeromse Zand", "Kasteel Soelen", "Roekel", "Amerongse Bos", "Noord Ginkel", "Amer Bovenpolder", "Lunters Buurtbosch", "Warnsborn", "Park zypendaal", "Zuid Ginkel", "De hoge veluwe", "Burgers zoo", "Park sonsbeek", "Openluchtmuseum", "Park angerenstein", "Kasteel rosendael", "Utrechtse Heuvelrug", "Groesbeekse bos", "Posbank")
        recreation_names <- c("Beeldengalerij Het Depot", "Arboretum Belmonte", "Wageningen Berg en Bergpad", "Arboretum de Dreijen", "Uitwaarden Wageningen", "Renkums Beekdal", "Utrechtse Heuvelrug Grebbeberg", "De Blauwe Kamer", "Ouwehands Dierenpark Rhenen", "Kasteel Doorwerth", "Edese bos", "Airborne museum hartenstein", "Peppelenburg Westerrode", "Landgoed Kernhem", "Wekeromse Zand", "Kasteel Soelen", "Roekel", "Hoog en Laag Amerongse Bos", "Noord Ginkel", "Amerongse Bovenpolder", "Lunters Buurtbosch", "Warnsborn", "Park zypendaal", "Zuid Ginkel", "Het nationale park de hoge veluwe", "Burgers zoo", "Park sonsbeek", "Het nederlands openluchtmuseum", "Park angerenstein", "Park en kasteel rosendael", "Nationaal Park Utrechtse Heuvelrug", "Groesbeekse bos", "Veluwetransferium posbank")
        lat <- c(51.96840, 51.96638, 51.97111, 51.96802, 51.97037, 51.98697, 51.95361, 51.94353, 51.95702, 51.96675, 52.05237, 51.98771, 52.07328, 52.05281, 52.10083, 51.91018, 52.09492, 52.01544, 52.04512, 51.99069, 52.09086, 52.01039, 51.99838, 52.03775, 52.07867, 52.01025, 51.98774, 52.00995, 51.99323, 52.00964, 52.04447, 51.78360, 52.01579)
        lon <- c(5.67537, 5.69066, 5.70500, 5.67774, 5.68824, 5.73007, 5.59500, 5.61580, 5.59022, 5.78866, 5.68828, 5.83272, 5.70541, 5.65733, 5.68528, 5.40130, 5.73046, 5.47349, 5.75416, 5.47102, 5.64035, 5.86551, 5.89520, 5.73572, 5.83246, 5.90145, 5.90211, 5.90664, 5.94018, 5.96365, 5.36569, 5.90290, 6.02491)
        recreation_data <- data.frame(recreation_areas, recreation_names, lat = as.numeric(lat), lon = as.numeric(lon))
        if (requireNamespace("sf", quietly = TRUE) && requireNamespace("mapview", quietly = TRUE)) {
          recr_sf <- sf::st_as_sf(recreation_data, coords = c("lon", "lat"), crs = 4326)
          html_fl2 <- tempfile(fileext = ".html")
          pal <- mapview::mapviewGetOption("raster.palette")
          m <- mapview::mapview(recr_sf, zcol = "recreation_areas", label = "recreation_names",
                                map.types = "OpenStreetMap",
                                col.regions = pal,
                                layer.name = "recreation_data recreation_names")
          mapview::mapshot(m, url = html_fl2)
          if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
            rstudioapi::viewer(html_fl2)
          }
        }
      }
    }

## Pivot tables as data frames for the number of visits and expected visiting i(distance, total fuel cost, total activities cost  duration)
## pivot

my_data <- fdata20
pt <- generate_pivot(my_data)
pivot_df <- pt$asDataFrame()
pivot_df[is.na(pivot_df)] <- ""

save_pivots(pt, "pivot", output_dir = out_base)
rm(pt)

## percent of Grand Total for the number of visits
## pivot1
pt1 <- percent_GT_NV_pivot(my_data)
pivot1_df <- pt1$asDataFrame()
pivot1_df[is.na(pivot1_df)] <- ""

save_pivots(pt1, "pivot1", output_dir = out_base)
rm(pt1)

## percent of the row total number of visits per zip code
## pivot2
pt2 <- percent_NV_zip_code(my_data)
pivot2_df <- pt2$asDataFrame()
pivot2_df[is.na(pivot2_df)] <- ""

save_pivots(pt2, "pivot2", output_dir = out_base)
rm(pt2)

## percent of Grand Total for the expected visiting duration
## pivot3
pt3 <- percent_GT_EVD_pivot(my_data)
pivot3_df <- pt3$asDataFrame()
pivot3_df[is.na(pivot3_df)] <- ""

save_pivots(pt3, "pivot3", output_dir = out_base)
rm(pt3)

## percent of the row total number of visits per recreation areas
## pivot4
pt4 <- percent_NV_Recr(my_data)
pivot4_df <- pt4$asDataFrame()
pivot4_df[is.na(pivot4_df)] <- ""

save_pivots(pt4, "pivot4", output_dir = out_base)
rm(pt4)

## percent of the row expected visiting duration per zip code
## pivot5
pt5 <- percent_EVD_zip_code(my_data)
pivot5_df <- pt5$asDataFrame()
pivot5_df[is.na(pivot5_df)] <- ""

save_pivots(pt5, "pivot5", output_dir = out_base)
rm(pt5)


## percent of the row total expected visiting duration per recreation area
pt6 <- percent_EVD_Recr(my_data)
pivot6_df <- pt6$asDataFrame()
pivot6_df[is.na(pivot6_df)] <- ""

save_pivots(pt6, "pivot6", output_dir = out_base)
rm(pt6)
if (!is.null(conn) && DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn)
}
pivot_tables_l <- list(
    pivot = pivot_df,
    pivot1 = pivot1_df,
    pivot2 = pivot2_df,
    pivot3 = pivot3_df,
    pivot4 = pivot4_df,
    pivot5 = pivot5_df,
    pivot6 = pivot6_df,
    my_data = my_data,
    qp_solution = qp_solution,
    linear_solution = linear_solution)
return(pivot_tables_l)
}

