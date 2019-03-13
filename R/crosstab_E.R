#' crosstab function.
#' `crosstab function` Function to compute the group numbers of recreation area with the same activities on the same time segments or to compute the column created for the group numbers of recreation areas and the activities pattern on the same time segment.
#'
#' crosstab_E(Rmatrix, Cmatrix, C1)
#'
#' The data we use here are from the table data which we got from a query from a query of the Final table. The table data had n records (n rows) and 4 columns namely: recreation area, activities pattern, transport type and time segment.

#' This table data with the use of the function group_by was reduced in n1 records. The column of the trasport type is not important for the groups computation

#' The crosstab_E function has 5 subfunctions:
#' 1. NGroup0
#' 2. NGroup1
#' 3. NGroup2
#' 4. NGroup3
#' 5. NGroup4
#'
#' NGroup0 computes the group numbers of recreation areas where all the sequence(s) of their presences (1) on the combination of activities pattern and time segment are not interrupted by absence (0). Thus in this case we have one group of ones (presence).

#' NGroup1 computes the group numbers of recreation areas where all the sequence(s) of their presences (1) on the combination of activities pattern and time segment are once interrupted by absence (0). Thus in this case we have two groups of ones (presence).

#' NGroup2 computes the group numbers of recreation areas where all the sequence(s) of their presences (1) on the combination of activities pattern and time segment are twice interrupted by absences (0). Thus in this case we have three groups of ones (presence).

#' NGroup3 computes the group numbers of recreation areas where all the sequence(s) of their presences (1) on the combination of activities pattern and time segment are three times interrupted by absences (0). Thus in this case we have four groups of ones (presence).

#' NGroup4 computes the group numbers of recreation areas where all the sequence(s) of their presences (1) on the combination of activities pattern and time segment are four times interrupted by absences (0). Thus in this case we have five groups of ones (presence)
#' @param Rmatrix Matrix of 1 or 2 columns:
#' The first column contains the vector RcrID or the vector groupsRecr_at and the second column if exist the vextor of the corresponding activities pattern for the vector groupsRecr_at
#' @param Cmatrix Matrix of 2 or 1 coloumn(s)
#' Activities patern ID and time segment for 2 columns or time segment for one column.
#' @param C1 matrix empty,vector
#'
#' If the third argument of the function C1 is an empty matrix and when the Rmatrix  matrix contains 1 column THEN the results of this function are the group numbers of recreation areas with the same activities and on the same time segments. If the Rmatrix  matrix contains 2 columns then the results of this function are the group numbers of groupsRecr_at and the activities pattern on the same time segment (see Wageningen_tables_Excel folder: Procedures_ACRER.xls, Excel sheet: crosstab_procedure (1 or 9) or HTML folder: Procedures_ACRER.html (crosstab_procedure (1 or 9) section)
#' If the third argument of the function C1 matrix is NOT an empty matrix but contains the linear optimization solution of our model THEN the result of this function are the linear optimization solutions per time segment.
#' @return list1 Listrow_matrix, col_matrix, crosstable, groups_matrix, fdata, tdata.
#'


crosstab_E <- function(Rmatrix, Cmatrix, C1) {


    print(match.call())
    nargin <- length(as.list(match.call())) - 1
    if (nargin == 1) {
        stop("Number of input arguments must be greater than 1")
    }

    ## Initialize

    RowMatrix <- Rmatrix  ## number o rows for the cross table (recreation area) (=487 records)
    ColMatrix <- Cmatrix  ## number of columns for the cross table (activities vector and time segment vector)
    nRowMatrix <- ncol(RowMatrix)  ## the number of vectors for the rows of the cross table (=1,RecrID)
    nColMatrix <- ncol(ColMatrix)  ## the number of vectors for the columns of the cross table (=2, activities vector and time segment vector )
    ## data11 matrix with 3 columns: RecrID, activity_pattern and time_segment

    nRsize <- dim(RowMatrix)  ## (= 487 x 1)
    nRows <- c(nRsize[1])  ## (=487 records)
    data11 <- cbind(RowMatrix, ColMatrix)  ## matrix(487 x 3)
    data11 <- as.data.frame(data11)
    ffactor1 <- factor(as.matrix(data11[, 1]))  ## ffactor1 = RecrID as a factor with 33 levels
    levelsffactor1 <- as.numeric(levels(ffactor1))  ## (=33)
    lnc <- length(levelsffactor1)  ## (=33)
    ffactor2 <- factor(as.matrix(data11[, 2]))  ## ffactor2 = combination of activity_pattern and time_segment  with 34 levels
    levelsffactor2 <- as.numeric(levels(ffactor2))  ## (=34)
    lnr <- length(levelsffactor2)  ## (=34)
    fRecrIDX <- rep(0, 1)
    y <- c(1:lnc)
    number <- as.vector(y)  ## array(1:33)
    x1 <- c(1:nRows)  ## array(1:487)
    x2 <- c(1:lnr)  ## array(1:34)
    sum_of_rows <- c(rep(0, lnc))  ## initialize sum_of_rows with zeros (1:33)
    zr <- c(rep(0, lnc))
    sumofzeros <- c(rep(0, lnc))  ## initialize sum_of_rows with zeros (1:33)
    tot <- lnc * lnr  ## 34 x 33 = 1122
    data3 <- matrix(c(rep(0, tot)), nrow = lnc, ncol = lnr)  ## matrix data3 (33 rows x  34 columns)


    if ((nargin == 3) && (length(C1) != 0)) {
        count <- 1

        for (i in x1) {
            if (nRowMatrix == 1) {
                if (((count < lnc) && (i > 1)) && ((Rmatrix[i]) != (Rmatrix[i - 1]))) {
                  count <- count + 1
                }
            } else {
                if ((((count < lnc) && (i > 1)) && ((Rmatrix[i, 1]) != (Rmatrix[i -
                  1, 1]))) || (((Rmatrix[i, 2] != (Rmatrix[i - 1, 2]))))) {
                  count <- count + 1
                }
            }


            for (j in x2) {
                if (nRowMatrix == 2) {
                  if ((Cmatrix[i, 1] == levelsffactor2[j]) && (Cmatrix[i, 2] == levelsffactor2[j])) {
                    data3[count, j] = C1[i]
                  }
                } else {
                  if (Cmatrix[i, 1] == levelsffactor2[j]) {
                    data3[count, j] = C1[i]
                  }
                }
            }
        }

        for (i in number) {
            zr <- which(data3[i, ] == 0)
            sumofzeros[i] <- length(zr)
            sum_of_rows[i] <- 4 - sumofzeros[i]
        }

        sumofzeros <- as.matrix(sumofzeros)
        data4 <- cbind(number, levelsffactor1, data3, sum_of_rows, sumofzeros)
        Ndata4 <- dim(data4)
        dummy <- data4[, 3:(Ndata4[2] - 2)]
        r <- levelsffactor1
        c <- levelsffactor2
        list15 <- list(r, c, dummy)
        return(list15)
    }

    ## if the number of vectors for the rows of the cross table is 1

    if (nRowMatrix == 1) {
        data2 <- as.data.frame(data11)  ## the same dim as data11 Rmatrix+Cmatrix
        colnames(data2)[1] <- c("RecrID")
        data2 <- data2[order(data2[, 1], data2[, 2], data2[, 3]), ]
        FRecrID <- dplyr::select(data2, RecrID)
        FRecrID1 <- dplyr::group_by(FRecrID, RecrID)
        FRecrID2 <- dplyr::summarise(FRecrID1, cRecrID = dplyr::n())
        Fcombination <- dplyr::select(data2, activity_pattern, Time_segment)
        combination <- Fcombination
        combination <- dplyr::group_by(combination,activity_pattern,Time_segment)
        combination <- dplyr::summarise(combination,cactiv = dplyr::n(), ctime = dplyr::n())
        FRecrID2 <- dplyr::select(FRecrID2, RecrID)
        combination <- dplyr::select(combination, activity_pattern, Time_segment)
        dFRecrID <- length(FRecrID2$RecrID)
        lnc <- dFRecrID  ## number of levels of RecrID factor
        dcombi <- dim(combination)
        factors <- t(combination)
        lnr <- dcombi[1]
        tot <- lnc * lnr
        data3 <- matrix(c(rep(0, tot)), nrow = lnc, ncol = lnr)
    } else {
        data2 <- as.data.frame(data11)  ## the same dim as data11 Rmatrix+Cmatrix
        colnames(data2)[1] <- c("groupsRecr_act_ts")
        colnames(data2)[2] <- c("activity_pattern")
        colnames(data2)[3] <- c("Time_segment")
        data2 <- data2[order(data2[, 1], data2[, 2], data2[, 3]), ]
        Fcombination <- dplyr::select(data2, groupsRecr_act_ts, activity_pattern)
        combination <- Fcombination
        combination <- dplyr::group_by(combination,groupsRecr_act_ts,activity_pattern)
        combination <- dplyr::summarise(combination,cgroupsRecr_act_ts = dplyr::n(), cactiv = dplyr::n())
        combination <- dplyr::select(combination, groupsRecr_act_ts, activity_pattern)
        FTime_segment <- dplyr::select(data2, Time_segment)
        FTime_segment1 <- dplyr::group_by(FTime_segment, Time_segment)
        Time_segment <- dplyr::summarise(FTime_segment1, cTime_segment = dplyr::n())
        Time_segment <- dplyr::select(Time_segment, Time_segment)
        factors <- t(Time_segment)
        dTime_segment <- length(Time_segment$Time_segment)
        lnr <- dTime_segment  ## number of levels of dTime_segment factor
        dcombi <- dim(combination)
        lnc <- dcombi[1]
        tot <- lnc * lnr
        data3 <- matrix(c(rep(0, tot)), nrow = lnc, ncol = lnr)
    }
    count <- 1
    x1 <- c(1:nRows)
    x2 <- c(1:lnr)
    y <- c(1:lnc)
    number <- as.vector(y)

    ## Table construction of 0 and 1 for the presence or absence of the combination of
    ## the activities in time segment for a certain RecrID
    for (i in x1) {
        if (nRowMatrix == 1) {
            if (((count < lnc) && (i > 1)) && ((Rmatrix[i]) != (Rmatrix[i - 1]))) {
                count <- count + 1
            }
        } else {
            if ((count < lnc) && (i > 1) && ((Rmatrix[i, 1] != Rmatrix[i - 1, 1]) ||
                (Rmatrix[i, 2] != Rmatrix[i - 1, 2]))) {
                count <- count + 1
            }
        }
        for (j in x2) {
            if (nColMatrix == 2) {
                if ((Cmatrix[i, 1] == combination$activity_pattern[j]) && (Cmatrix[i,
                  2] == combination$Time_segment[j])) {
                  if ((nargin == 3) & (length(C1) == 0)) {
                    data3[count, j] = 1
                  } else {
                    data3[count, j] = C1[i]
                  }
                }
            } else {
                if (Cmatrix[i, 1] == Time_segment[j, ]) {
                  if ((nargin == 3) & (length(C1) == 0)) {
                    data3[count, j] = 1
                  } else {
                    data3[count, j] = C1[i]
                  }
                }
            }
        }
    }

    for (i in number) {
        sum_of_rows[i] <- sum(data3[i, ])  ## vector of the sum of rows
        zr <- which(data3[i, ] == 0)
        sumofzeros[i] <- length(zr)
    }
    ## sort the sum of rows in decreasing order and constuct the levels of them
    if (nRowMatrix == 1) {
        ## if the RecrID is the only factor for the presence absence of the combination of
        ## the activities in a given time segment
        data4 <- cbind(FRecrID2, data3, sum_of_rows, sumofzeros)
        ncoldata3 <- ncol(data3) + 3
        colnames(data4)[1] <- c("RecrID")
        colnames(data4)[ncoldata3 - 1] <- c("sum_of_rows")
        data4 <- data4[order(-data4$sum_of_rows), ]
        data4 <- data4[, -ncoldata3]
        Ndata4 <- ncol(data4)  ## the size of data4
        Ncrossdata <- data4
        NCcrossdata <- ncol(Ncrossdata)
        Gcrossdata <- dplyr::select(data4, sum_of_rows)
        Gcrossdata <- Ncrossdata
        Gcrossdata <- dplyr::group_by(Gcrossdata,sum_of_rows)
        Gcrossdata <- dplyr::summarise(Gcrossdata,sum_rows = dplyr::n())
        Gcrossdata <- dplyr::select(Gcrossdata, sum_of_rows)
        Gcrossdata <- sort(Gcrossdata$sum_of_rows, decreasing = TRUE)
        NGcrossdata <- length(Gcrossdata)
        activ_segment <- t(as.matrix(rep(0, lnr)))
        RecrID <- Ncrossdata[1]
        groups <- c(4, 6, 7, 3, 1, 16, 2, 34, 25, 82, 31, 41, 21, 19, 15, 27, 37, 67,77)
    } else {
        ## if the RecrID and acivities are the factors for the presence absence of a given
        ## time segment
        data4 <- cbind(combination$groupsRecr_act_ts, combination$activity_pattern,
            data3, sum_of_rows, sumofzeros)
        data4 <- data4[order(-data4[, 7]), ]
        data4 <- data4[, -8]
        Ndata4 <- ncol(data4)  ## the size of data4
        Ncrossdata <- data4
        colnames(Ncrossdata)[7] <- c("sum_of_rows")
        NCcrossdata <- ncol(Ncrossdata)
        Gcrossdata <- as.data.frame(Ncrossdata)
        Gcrossdata <- dplyr::group_by(Gcrossdata,sum_of_rows)
        Gcrossdata <- dplyr::summarise(Gcrossdata,sum_rows = dplyr::n())
        Gcrossdata <- dplyr::select(Gcrossdata, sum_of_rows)
        Gcrossdata <- sort(Gcrossdata$sum_of_rows, decreasing = TRUE)
        NGcrossdata <- length(Gcrossdata)
        activ_segment <- t(as.matrix(rep(0, lnr)))
        groupsRecr_at <- Ncrossdata[, 1]
        activity_pattern <- Ncrossdata[, 2]
        bfact <- cbind(groupsRecr_at, activity_pattern)
        nbfact <- dim(bfact)
        fbfactX1 <- rep(0, 1)
        fbfactX2 <- rep(0, 1)
        Fbfact11 <- rep(0, 1)
        Fbfact22 <- rep(0, 1)
        groups <- c(37, 1, 4, 25)
    }

    ## Initialization of the indexes
    count <- 1
    findex <- 1
    levels_sr <- 1  ## levels og the sum of rows
    Index_group_number <- 0  ## index of the group number
    ## index_row_record <- 0 ## index of the row record
    x <- c(1:lnc)

    ## Construct a for loop for the recreation areas start for loop i
    for (index_row_record in x) {
        ## select each row of the cross table if the vector for the rows matrix equals to one
        if (nRowMatrix == 1) {
            if (findex == 1) {
                activ_segment <- Ncrossdata[index_row_record, 2:(Ndata4 - 1)]  ## construct of the presence/absence of activities-timesegment matrix
            } else {
                temp <- Ncrossdata[index_row_record, 2:(Ndata4 - 1)]
                activ_segment <- rbind(activ_segment, temp)
            }
            ## construct the recreation vector
            fRecrIDX[findex] <- RecrID[index_row_record, ]
            c1 <- rep(1, lnr)
            c2 <- rep(1, lnr)
            c1 <- factors[1, ]  ## construct the row of the activites factor (1 t/m 5)
            c2 <- factors[2, ]  ## construct the row of the time segment factor ( 1 t/m 4)

            ## compute the the column totals of each block rows per level
            ## (levels:15,14,13,10,9,8,6,4,2,1)
            if (findex == 1) {
                sumc_of_groups <- activ_segment
            } else {
                sumc_of_groups <- colSums(as.matrix(activ_segment))
            }
        } else {
            if (findex == 1) {
                activ_segment <- Ncrossdata[index_row_record, 3:(Ndata4 - 1)]
            } else {
                temp <- Ncrossdata[index_row_record, 3:(Ndata4 - 1)]
                activ_segment <- rbind(activ_segment, temp)
            }

            fbfactX1[findex] <- bfact[index_row_record, 1]
            fbfactX2[findex] <- bfact[index_row_record, 2]
            c1 <- rep(1, lnr)
            c1 = factors[1, ]

            c1 <- t(c1)
            activ_segment <- t(activ_segment)

            activ_segment <- matrix(activ_segment, nrow = 1, ncol = 4)
            # c1 <- matrix(c1, nrow = 1, ncol = NCcrossdata)
            sumc_of_groups <- colSums(activ_segment)
            sumc_of_groups <- matrix(sumc_of_groups, nrow = 1, ncol = 4)
        }
        ## end of the row selection

        ## if the level of the sum per row is equal with the total number of levels and the
        ## index count equals with the number of recreation areas
        if ((levels_sr == NGcrossdata) && (count == lnc)) {
            ## exit the for loop if the number of level is greater than the total number of
            ## levels
            if (levels_sr > NGcrossdata) {
                break
            }

            ## rows margin levels for the cross table sum of rows
            q <- Gcrossdata[levels_sr]

            ## if the vector for the rows matrix equals to one
            if (nRowMatrix == 1) {
                z5 <- which(sumc_of_groups != 0)
                ## the row of the activities factor without zeros
                c1 <- c1[z5]
                ## the row of the time segment factor without zeros
                c2 <- c2[z5]
                ## the number of elements of the activities factor
                nc1c2 <- length(c1)
                Nsegment <- ncol(as.matrix(activ_segment))
                ## the presence/absence of activities-timesegment withou the zero columns
                if (Nsegment == 1) {
                  activ_segment <- activ_segment[z5]
                } else {
                  activ_segment <- activ_segment[, z5]
                }
                rowsumactivesegment <- rowSums(as.matrix(activ_segment))
                ## the sum per column of the presence/absence per block rows of the activities-time
                ## segment without zeros
                sumc1c2 <- sumc_of_groups[z5]
                sumr_c_of_groups <- colSums(as.matrix(sumc_of_groups))
                ## compute the differences of the sum per column of the presence/absence per block
                ## rows of the activities-time segment without zeros
                dif <- diff(as.matrix(sumc1c2))
                ## compute the index of the differences of the sum per column of the presence/absence
                ## per block rows of the activities-time segment without zeros
                pos1 <- which(dif != 0)
                ## compute the number of the elements of the vector of the index of the differences
                ## of the sum per column of the presence/absence per block rows of the
                ## activities-time segment without zeros
                ngroups <- length(pos1)
            } else {
                z5 <- which(sumc_of_groups != 0)
                c1 <- c1[z5]
                nc1 <- length(c1)
                Nsegment <- ncol(as.matrix(activ_segment))
                ## the presence/absence of activities-timesegment withou the zero columns
                if (Nsegment == 1) {
                  activ_segment <- activ_segment[z5]
                } else {
                  activ_segment <- activ_segment[, z5]
                }
                rowsumactivesegment = rowSums(as.matrix(activ_segment))
                sumc1 <- sumc_of_groups[z5]
                sumr_c_of_groups <- colSums(as.matrix(sumc_of_groups))
                dif <- diff(as.matrix(sumc1))
                pos1 <- which(dif != 0)
                ngroups <- length(pos1)
            }

            ## if there are 0 elements of the number of the elements of the vector of the index
            ## of the differences of the sum per column (one group)
            if (ngroups == 0) {
                list00 <- NGroup0(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22)
                if (nRowMatrix == 1) {
                  Nfactors <- list00[[1]]
                  FRecrID <- list00[[2]]
                  Ngroups <- list00[[3]]
                  levels_sr <- list00[[4]]
                  Index_group_number <- list00[[5]]
                } else {
                  Nfactors <- list00[[1]]
                  Fbfact11 <- list00[[2]]
                  Fbfact22 <- list00[[3]]
                  Ngroups <- list00[[4]]
                  levels_sr <- list00[[5]]
                  Index_group_number <- list00[[6]]
                  ## end if nRowMatrix == 1
                }
            } else if (ngroups == 1) {
                list11 <- NGroup1(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)
                if (nRowMatrix == 1) {
                  Nfactors <- list11[[1]]
                  FRecrID <- list11[[2]]
                  Ngroups <- list11[[3]]
                  levels_sr <- list11[[4]]
                  Index_group_number <- list11[[5]]
                } else {
                  Nfactors <- list11[[1]]
                  Fbfact11 <- list11[[2]]
                  Fbfact22 <- list11[[3]]
                  Ngroups <- list11[[4]]
                  levels_sr <- list11[[5]]
                  Index_group_number <- list11[[6]]
                }  ## end if nRowMatrix == 1
                ## end of if ngroups == 1 the number of ngroups equals to two (number of Groups are
                ## three)
            } else if (ngroups == 2) {
                list22 <- NGroup2(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)

                if (nRowMatrix == 1) {
                  Nfactors <- list22[[1]]
                  FRecrID <- list22[[2]]
                  Ngroups <- list22[[3]]
                  levels_sr <- list22[[4]]
                  Index_group_number <- list22[[5]]
                } else {
                  Nfactors <- list22[[1]]
                  Fbfact11 <- list22[[2]]
                  Fbfact22 <- list22[[3]]
                  Ngroups <- list22[[4]]
                  levels_sr <- list22[[5]]
                  Index_group_number <- list22[[6]]
                }  ## end if nRowMatrix == 1
                ## end of if ngroups == 2 the number of ngroups equals to 3
            } else if (ngroups == 3) {
                list33 <- NGroup3(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)
                if (nRowMatrix == 1) {
                  Nfactors <- list33[[1]]
                  FRecrID <- list33[[2]]
                  Ngroups <- list33[[3]]
                  levels_sr <- list33[[4]]
                  Index_group_number <- list33[[5]]
                } else {
                  Nfactors <- list33[[1]]
                  Fbfact11 <- list33[[2]]
                  Fbfact22 <- list33[[3]]
                  Ngroups <- list33[[4]]
                  levels_sr <- list33[[5]]
                  Index_group_number <- list33[[6]]
                }  ## end if nRowMatrix == 1
                ## end of if ngroups == 3 the number of ngroups equals to 4
            } else {
                if (ngroups == 4) {
                  list44 <- NGroup4(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                    levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                    Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)

                  if (nRowMatrix == 1) {
                    Nfactors <- list44[[1]]
                    FRecrID <- list44[[2]]
                    Ngroups <- list44[[3]]
                    levels_sr <- list44[[4]]
                    Index_group_number <- list44[[5]]
                  } else {
                    Nfactors <- list44[[1]]
                    Fbfact11 <- list44[[2]]
                    Fbfact22 <- list44[[3]]
                    Ngroups <- list44[[4]]
                    levels_sr <- list44[[5]]
                    Index_group_number <- list44[[6]]
                  }  ## end if nRowMatrix == 1
                  ## end of if ngroups == 4 the number of ngroups equals to 5 }
                }
            }
            ## end if ((levels_sr == NGcrossdata) && (count == lnc))
        }
        if ((count + 1) > lnc) {
            break
        }

        if (nColMatrix >= 1) {
            v1 <- Ncrossdata[index_row_record, (NCcrossdata)]
            v2 <- Ncrossdata[(count + 1), (NCcrossdata)]
            z <- identical(v1, v2)
            if ((z == FALSE) & ((count + 1) <= lnc)) {
                step <- 1
            } else {
                step <- 0
            }
        }

        if (step == 1) {
            if (levels_sr > NGcrossdata) {
                break
            }
            q <- Gcrossdata[levels_sr]
            if (nRowMatrix == 1) {
                z5 <- which(sumc_of_groups != 0)
                c1 <- c1[z5]
                c2 <- c2[z5]
                nc1c2 <- length(c1)
                Nsegment <- ncol(as.matrix(activ_segment))
                ## the presence/absence of activities-timesegment without the zero columns
                if (Nsegment == 1) {
                  activ_segment <- activ_segment[z5]
                } else {
                  activ_segment <- activ_segment[, z5]
                }
                rowsumactivesegment <- rowSums(as.matrix(activ_segment))
                sumc1c2 <- sumc_of_groups[z5]
                sumr_c_of_groups <- colSums(as.matrix(sumc_of_groups))
                dif <- diff(as.matrix(sumc1c2))
                pos1 <- which(dif != 0)
                ngroups <- length(pos1)
            } else {
                ## if (nRowMatrix > 1)
                z5 <- which(sumc_of_groups != 0)
                c1 <- c1[z5]
                nc1 <- length(c1)
                Nsegment <- ncol(as.matrix(activ_segment))
                ## the presence/absence of activities-timesegment withou the zero columns
                if (Nsegment == 1) {
                  activ_segment <- activ_segment[z5]
                } else {
                  activ_segment <- activ_segment[, z5]
                }
                rowsumactivesegment = rowSums(as.matrix(activ_segment))
                sumc1 <- sumc_of_groups[z5]
                sumr_c_of_groups <- colSums(as.matrix(sumc_of_groups))
                dif <- diff(as.matrix(sumc1))
                pos1 <- which(dif != 0)
                ngroups <- length(pos1)
            }
            if (ngroups == 0) {
                list00 <- NGroup0(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22)
                if (nRowMatrix == 1) {
                  Nfactors <- list00[[1]]
                  FRecrID <- list00[[2]]
                  Ngroups <- list00[[3]]
                  levels_sr <- list00[[4]]
                  Index_group_number <- list00[[5]]
                } else {
                  Nfactors <- list00[[1]]
                  Fbfact11 <- list00[[2]]
                  Fbfact22 <- list00[[3]]
                  Ngroups <- list00[[4]]
                  levels_sr <- list00[[5]]
                  Index_group_number <- list00[[6]]
                }
            } else if (ngroups == 1) {
                list11 <- NGroup1(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)
                if (nRowMatrix == 1) {
                  Nfactors <- list11[[1]]
                  FRecrID <- list11[[2]]
                  Ngroups <- list11[[3]]
                  levels_sr <- list11[[4]]
                  Index_group_number <- list11[[5]]
                } else {
                  Nfactors <- list11[[1]]
                  Fbfact11 <- list11[[2]]
                  Fbfact22 <- list11[[3]]
                  Ngroups <- list11[[4]]
                  levels_sr <- list11[[5]]
                  Index_group_number <- list11[[6]]
                }  ## end if nRowMatrix == 1
                ## end of if ngroups == 1 the number of ngroups equals to 2
            } else if (ngroups == 2) {
                list22 <- NGroup2(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)

                if (nRowMatrix == 1) {
                  Nfactors <- list22[[1]]
                  FRecrID <- list22[[2]]
                  Ngroups <- list22[[3]]
                  levels_sr <- list22[[4]]
                  Index_group_number <- list22[[5]]
                } else {
                  Nfactors <- list22[[1]]
                  Fbfact11 <- list22[[2]]
                  Fbfact22 <- list22[[3]]
                  Ngroups <- list22[[4]]
                  levels_sr <- list22[[5]]
                  Index_group_number <- list22[[6]]
                }  ## end if nRowMatrix == 1
                ## end of if ngroups == 2 the number of ngroups equals to 3
            } else if (ngroups == 3) {
                list33 <- NGroup3(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                  levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                  Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)
                if (nRowMatrix == 1) {
                  Nfactors <- list33[[1]]
                  FRecrID <- list33[[2]]
                  Ngroups <- list33[[3]]
                  levels_sr <- list33[[4]]
                  Index_group_number <- list33[[5]]
                } else {
                  Nfactors <- list33[[1]]
                  Fbfact11 <- list33[[2]]
                  Fbfact22 <- list33[[3]]
                  Ngroups <- list33[[4]]
                  levels_sr <- list33[[5]]
                  Index_group_number <- list33[[6]]
                }  ## end if nRowMatrix == 1
                ## end of if ngroups == 3 the number of ngroups equals to 4
            } else {
                if (ngroups == 4) {
                  list44 <- NGroup4(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1,
                    levels_sr, Index_group_number, groups, Nfactors, fRecrIDX, FRecrID,
                    Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q)

                  if (nRowMatrix == 1) {
                    Nfactors <- list44[[1]]
                    FRecrID <- list44[[2]]
                    Ngroups <- list44[[3]]
                    levels_sr <- list44[[4]]
                    Index_group_number <- list44[[5]]
                  } else {
                    Nfactors <- list44[[1]]
                    Fbfact11 <- list44[[2]]
                    Fbfact22 <- list44[[3]]
                    Ngroups <- list44[[4]]
                    levels_sr <- list44[[5]]
                    Index_group_number <- list44[[6]]
                  }  ## end if nRowMatrix == 1
                  ## end of if ngroups == 4 the number of ngroups equals to 5
                }
            }
            findex <- 1

            if (count < lnc) {
                count <- count + 1
            } else {
                count <- count
            }  ## end if count

            levels_sr <- levels_sr + 1
            ## end of if step == 1
        } else {
            ## if step == 0
            if (nRowMatrix >= 1) {
                ## if the present row is equal to the next row
                if (nColMatrix >= 1) {
                  v1 <- Ncrossdata[index_row_record, (NCcrossdata)]
                  v2 <- Ncrossdata[(count + 1), (NCcrossdata)]
                  z <- identical(v1, v2)
                  if ((z == TRUE) & (count + 1 <= lnc)) {
                    findex <- findex + 1
                    if (count < lnc) {
                      count <- count + 1
                    } else {
                      count <- count
                    }  ## end if count
                    next
                  }
                }
                ## end of if nRowMatrix >= 1
            }
            ## end if step == 0
        }

        findex <- 1
        fRecrIDX <- rep(NaN, 1)
        fbfactX1 <- rep(NaN, 1)
        fbfactX2 <- rep(NaN, 1)
        activ_segment <- rep(NaN, 1)
        Nfactors2 <- rep(NaN, 1)
        Nfactors21 <- rep(NaN, 1)
        Nfactors22 <- rep(NaN, 1)
        Nfactors23 <- rep(NaN, 1)
        Nfactors33 <- rep(NaN, 1)
        Ngroups2 <- rep(NaN, 1)
        vgroups <- rep(NaN, 1)
        ## end of the for loop
    }

    if (nRowMatrix == 1) {
        Ncross <- dim(Ncrossdata)
        Ncrossdata <-  Ncrossdata[order(Ncrossdata[,1],decreasing = FALSE),]
        crosstable <- Ncrossdata[, 1:(Ncross[2] - 1)]
        RecrID <- as.numeric(FRecrID)
        activity_pattern <- as.numeric(Nfactors[, 1])
        Time_segment <- as.numeric(Nfactors[, 2])
        groupsRecr_at <- as.numeric(Ngroups)
        data5 <- cbind(RecrID, activity_pattern, Time_segment, groupsRecr_at)
        data5 <- data5[order(data5[, 1], data5[, 2], data5[, 3]), ]
        rownames(data5) <- c()
    } else {
        Ncross <- dim(Ncrossdata)
        Ncrossdata <-  Ncrossdata[order(Ncrossdata[,1],decreasing = FALSE),]
        crosstable <- Ncrossdata[, 3:(Ncross[2] - 1)]
        groupsRecr_at <- as.numeric(Fbfact11)
        activity_pattern <- as.numeric(Fbfact22)
        Time_segment <- as.numeric(Nfactors)
        groupsRecr_act_ts <- as.numeric(Ngroups)
        data5 <- cbind(groupsRecr_at, activity_pattern, Time_segment, groupsRecr_act_ts)
        data5 <- data5[order(data5[, 1], data5[, 2], data5[, 3]), ]
        rownames(data5) <- c()
    }

    fdata <- as.matrix(Ncrossdata)
    tdata <- as.matrix(data5)
    col_matrix <- factors
    row_matrix <- as.matrix(data5[, 1])
    groups_matrix <- as.matrix(data5[, 4])
    list1 <- list(row_matrix, col_matrix, crosstable, groups_matrix, fdata, tdata)
    return(list1)
    ## end of function
}
