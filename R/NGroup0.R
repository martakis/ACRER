#' NGroup0 computes the group numbers of the recreation areas.
#'
#' NGroup0 computes the group numbers of recreation areas where all the sequence(s) of their presences (1) on the combination of activities pattern and time segment are NOT interrupted by absences (0). Thus in this case we have 1 group of ones (presences).
#'
#' NGroup0(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1, levels_sr,Index_group_number, groups, Nfactors,fRecrIDX, FRecrID, Ngroups,fbfactX1, fbfactX2, Fbfact11, Fbfact22).
#'
#' @param nRowMatrix Matrix.
#' Select each row of the cross table if the vector for the rows matrix equals to one.
#' @param activ_segment Matrix.
#' activ_segment construct of the presence/absence of activities-time segment matrix.
#' @param c1 Numeric array.
#' Construct the row of the activities pattern factor in which (sumc_of_groups != 0).
#' @param c2 Numeric array.
#' Construct the row of the time segment factor in which (sumc_of_groups != 0).
#' @param sumc1c2 Numeric vector
#' The sum per column of the presence/absence per block rows of the activities-and time segment without zeros.
#' @param sumc1 Numeric vector.
#' The sum of the activ_segment.
#' @param levels_sr Numeric vector.
#' levels_sr levels of the sum of rows
#' @param Index_group_number Numeric vector.
#' Index_group_number the group number
#' @param groups Numeric vector.
#' Groups: 4 6 7 3 1 16 2 34 25 82 31 41 21 19 15 27 37 67 77 when Nfactors are two vectors, the time segment and the activities pattern and the RecrID is the only factor for the presence absence of the combination of the activities in a given time segment and groups: 37 1 4 25 when Nfactors is on vector the time segments vector.
#' @param Nfactors Numeric vector.
#' Nfactors: when Nfactors are two vectors, the time segment and the activities pattern then RecrID is the only factor for the presence absence of the combination of the activities in a given time segment and Nfactors is the time segments vector.
#' @param fRecrIDX Numeric vector.
#' fRecrIDX: temp. array for the RecrID vector.
#' @param FRecrID Numeric vector.
#' FRecrID: FRecrID for nRowMatrix is 1 column if the RecrID is the only factor for the presence absence of the combination of the activities in a given time segment (CMATRIX).
#' @param Ngroups Numeric vector.
#' Ngroups is the created vector groupsRecr_at when the RecrID is the only factor for the presence absence of the combination of the activities in a given time segment or the created vector groupsRecr_act_ts when the groupsRecr_at vector and activity pattern are the two factors for the presence absence of the time segment vector.
#' @param fbfactX1 Numerical array.
#' fbfactX1:  temp. array for the groupsRecr_at vector.
#' @param fbfactX2 Numerical array.
#' fbfactX2:  temp. array for the activity pattern vector
#' @param Fbfact11 Numerical vector.
#' Fbfact11: for the groupsRecr_at vector.
#' @param Fbfact22 Numerical vector.
#' Fbfact22: for the activity pattern vector
#' @return list00 Nfactors, Fbfact11, Fbfact22, Ngroups, levels_sr, Index_group_number.






NGroup0 <- function(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1, levels_sr, Index_group_number,
    groups, Nfactors, fRecrIDX, FRecrID, Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22)

  {
    if (nRowMatrix == 1) {
        nc1c2 <- length(c1)
        Nfactors2 <- rep(1, nc1c2)
        Nfactors2 <- rep(2, nc1c2)
        Nfactors2 <- rbind(c1, c2)
        z <- ncol(Nfactors2)
        z1 <- length(fRecrIDX)

        if (z > 1) {
            Nfactors2 <- t(as.matrix(Nfactors2))
            Nfactors21 <- rep(Nfactors2[, 1], times = z1)
            Nfactors22 <- rep(Nfactors2[, 2], times = z1)
        } else {
            Nfactors21 <- rep(Nfactors2[1, 1], times = z1 * z)
            Nfactors22 <- rep(Nfactors2[2, 1], times = z1 * z)
        }

        Nfactors33 <- cbind(Nfactors21, Nfactors22)

        if (levels_sr == 1) {
            Nfactors <- Nfactors33
        } else {
            ## if the number of vectors for the rows are more than one (eg. recreation area
            ## groups and activities)
            Nfactors <- rbind(Nfactors, Nfactors33)
        }

        fRecrID2 <- rep(fRecrIDX, times = z)
        fRecrID2 <- t(as.matrix(fRecrID2))
        fRecrID2 <- t(as.matrix(fRecrID2))

        if (levels_sr == 1) {
            FRecrID <- fRecrID2
        } else {
            FRecrID <- rbind(FRecrID, fRecrID2)
        }

        Index_group_number <- Index_group_number + 1
        vgroups <- groups[Index_group_number]
        Ngroups2 <- rep(vgroups, times = z * z1)
        Ngroups2 <- t(as.matrix(Ngroups2))
        Ngroups2 <- t(as.matrix(Ngroups2))

        if (levels_sr == 1) {
            Ngroups <- Ngroups2
        } else {
            Ngroups <- rbind(Ngroups, Ngroups2)
        }

        list00 <- list(Nfactors, FRecrID, Ngroups, levels_sr, Index_group_number)

    } else {
        nc1 <- length(c1)
        Nfactors2 <- rep(0, nc1)
        Nfactors2 <- c1
        Nfactors2 <- as.matrix(Nfactors2)
        Nsize <- length(Nfactors2)
        z <- length(Nfactors2)
        z1 <- length(fbfactX1)

        if (Nsize > 1) {
            Nfactors21 <- rep(Nfactors2, z1)
            Nfactors21 <- as.matrix(Nfactors21)
        } else {
            Nfactors21 <- rep(Nfactors2, z1 * z)
            Nfactors21 <- as.matrix(Nfactors21)
        }

        Nfactors33 <- Nfactors21
        Nfactors33 <- as.matrix(Nfactors33)

        if (levels_sr == 1) {
            Nfactors <- Nfactors33
            Nfactors <- as.matrix(Nfactors)
        } else {
            Nfactors <- rbind(Nfactors, Nfactors33)
            Nfactors <- as.matrix(Nfactors)
        }

        Fbfact1 <- rep(fbfactX1, z)
        Fbfact1 <- t(as.matrix(Fbfact1))
        Fbfact1 <- t(as.matrix(Fbfact1))
        Fbfact2 <- rep(fbfactX2, z)
        Fbfact2 <- t(as.matrix(Fbfact2))
        Fbfact2 <- t(as.matrix(Fbfact2))

        if (levels_sr == 1) {
            Fbfact11 <- Fbfact1
            Fbfact22 <- Fbfact2
        } else {
            Fbfact11 <- rbind(Fbfact11, Fbfact1)
            Fbfact22 <- rbind(Fbfact22, Fbfact2)
        }

        Index_group_number <- Index_group_number + 1
        vgroups <- groups[Index_group_number]
        Ngroups2 <- rep(vgroups, z * z1)
        Ngroups2 <- t(as.matrix(Ngroups2))
        Ngroups2 <- t(as.matrix(Ngroups2))

        if (levels_sr == 1) {
            Ngroups <- Ngroups2
        } else {
            Ngroups <- rbind(Ngroups, Ngroups2)
        }
        list00 <- list(Nfactors, Fbfact11, Fbfact22, Ngroups, levels_sr, Index_group_number)
    }  ## end of nRowmatrix > 1
    return(list00)
    ## end of Ngroup0
}


