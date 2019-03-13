#' function to compute the expected visit duration per recreation area.
#'
#' function to compute via integration the expected visit duration per recreation area.
#'
#' expected_visit_duration(dMinDurationAbs, dMaxDurationAbs, dMinDuration,dMaxDuration).
#'
#' @param dMinDurationAbs This is equal to_spend_minimum_time_per_activity (in min).
#' @param dMaxDurationAbs This is equal to_spend_maximum_time_per_activity (in min).
#' @param dMinDuration This is equal to max(maxduration,min_duration_abs)  (in min).
#' @param dMaxDuration This is equal to min(available_time_for_activity,step1)  (in min).
#' @return expected_visit_duration Computation of the expected visit duration: list1a = matrix(dResult).

expected_visit_duration <- function(dMinDurationAbs, dMaxDurationAbs, dMinDuration,
    dMaxDuration) {

    # dMaxDurationAbs matrix of 127319 elements: 150,150,150,150,150,....
    # dMinDurationAbs matrix of 127319 elements: 45,45,45,45,45,45,...  dMaxDuration
    # matrix of 127319 elements: 62,150,150,150,80....  dMinDuration matrix of 127319
    # elements: 45,45,45,45,45,45,...  Initialization of the matrices

    dMaxDurationAbs <- as.matrix(dMaxDurationAbs)
    dMinDuration <- as.matrix(dMinDuration)
    dMaxDuration <- as.matrix(dMaxDuration)
    lMinDurationAbs <- length(dMinDurationAbs)
    x9 <- c(1:lMinDurationAbs)
    dResult <- c(rep(-99, lMinDurationAbs))
    dMean <- c(rep(-99, lMinDurationAbs))
    yMax <- c(rep(-99, lMinDurationAbs))
    BA <- c(rep(-99, lMinDurationAbs))
    B2A2 <- c(rep(-99, lMinDurationAbs))
    B3A3 <- c(rep(-99, lMinDurationAbs))
    MA <- c(rep(-99, lMinDurationAbs))
    M2A2 <- c(rep(-99, lMinDurationAbs))
    M3A3 <- c(rep(-99, lMinDurationAbs))
    BM <- c(rep(-99, lMinDurationAbs))
    B2M2 <- c(rep(-99, lMinDurationAbs))
    B3M3 <- c(rep(-99, lMinDurationAbs))

    for (i in x9) {
        if ((dMinDurationAbs[i] < 0) || (dMaxDurationAbs[i] < 0) || (dMinDuration[i] <
            0) || (dMaxDuration[i] < 0)) {
            dResult[i] <- 0
            next
        }
        if (dMaxDuration[i] == dMinDuration[i]) {
            dResult[i] <- dMaxDuration[i]
            next
        }
        if (dMaxDuration[i] < dMinDuration[i]) {
            dResult[i] <- 0
            next
        }
        if (dMinDuration[i] < dMinDurationAbs[i]) {
            dResult[i] <- 0
            next
        }
        if (dMaxDuration[i] > dMaxDurationAbs[i]) {
            dResult[i] <- 0
            next
        }
        if (dMaxDurationAbs[i] < dMinDurationAbs[i]) {
            dResult[i] <- 0
            next
        }


        dMean[i] <- (dMaxDurationAbs[i] + dMinDurationAbs[i])/2
        yMax[i] = 1/dMean[i]

        if (dMaxDuration[i] <= dMean[i]) {
            # up
            BA[i] <- dMaxDuration[i] - dMinDuration[i]
            B2A2[i] <- dMaxDuration[i] * dMaxDuration[i] - dMinDuration[i] * dMinDuration[i]
            B3A3[i] <- dMaxDuration[i] * dMaxDuration[i] * dMaxDuration[i] - dMinDuration[i] *
                dMinDuration[i] * dMinDuration[i]
            dResult[i] <- (B3A3[i]/3 - B2A2[i] * dMinDurationAbs[i]/2)/(B2A2[i]/2 -
                BA[i] * dMinDurationAbs[i])

        } else if (dMinDuration[i] >= dMean[i]) {
            # down
            BA[i] <- dMaxDuration[i] - dMinDuration[i]
            B2A2[i] <- dMaxDuration[i] * dMaxDuration[i] - dMinDuration[i] * dMinDuration[i]
            B3A3[i] <- dMaxDuration[i] * dMaxDuration[i] * dMaxDuration[i] - dMinDuration[i] *
                dMinDuration[i] * dMinDuration[i]
            dResult[i] <- (B2A2[i] * dMaxDurationAbs[i]/2 - B3A3[i]/3)/(BA[i] * dMaxDurationAbs[i] -
                B2A2[i]/2)
        } else {
            # up and down
            MA[i] <- dMean[i] - dMinDuration[i]
            M2A2[i] <- dMean[i] * dMean[i] - dMinDuration[i] * dMinDuration[i]
            M3A3[i] <- dMean[i] * dMean[i] * dMean[i] - dMinDuration[i] * dMinDuration[i] *
                dMinDuration[i]
            BM[i] <- dMaxDuration[i] - dMean[i]
            B2M2[i] <- dMaxDuration[i] * dMaxDuration[i] - dMean[i] * dMean[i]
            B3M3[i] <- dMaxDuration[i] * dMaxDuration[i] * dMaxDuration[i] - dMean[i] *
                dMean[i] * dMean[i]

            dResult[i] <- M3A3[i]/3 + dMaxDurationAbs[i] * B2M2[i]/2 - dMinDurationAbs[i] *
                M2A2[i]/2 - B3M3[i]/3
            dResult[i] <- dResult[i]/((M2A2[i]/2 + dMaxDurationAbs[i] * BM[i] - dMinDurationAbs[i] *
                MA[i] - B2M2[i]/2))
        }
    }
    dResult <- as.matrix(dResult)
    list1a <- list(dResult)
    return(list1a)
}
