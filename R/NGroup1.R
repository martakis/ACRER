#' NGroup1 computes the group numbers of recreation areas.
#'
#' NGroup1 computes the group numbers of recreation areas where all the sequence(s) of their presences (1) on the combination of activities pattern and time segment are only once interrupted by their absence (0). Thus in this case we have two groups of ones (presences).
#'
#' NGroup1(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1, levels_sr,Index_group_number, groups, Nfactors, fRecrIDX,FRecrID, Ngroups,fbfactX1, fbfactX2, Fbfact11, Fbfact22, pos1, q).
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
#' @param pos1 Scalar.
#' Is an index for slicing an array.
#' @param q Scalar.
#' To find out which combination is.
#' @return list11 Nfactors, Fbfact11, Fbfact22, Ngroups, levels_sr, Index_group_number.


NGroup1 <- function(nRowMatrix, activ_segment, c1, c2, sumc1c2, sumc1, levels_sr, Index_group_number,
    groups, Nfactors, fRecrIDX, FRecrID, Ngroups, fbfactX1, fbfactX2, Fbfact11, Fbfact22,
    pos1, q) {

    ## Number of groups is 2

    if (nRowMatrix == 1) {
        sumcgroups1 <- sumc1c2[1:pos1[1]]
        nc1c2 <- length(c1)
        rowsumactivesegment <- rowSums(as.matrix(activ_segment))
        c11 <- c1[1:pos1[1]]
        c12 <- c2[1:pos1[1]]
        activ_segment1 <- activ_segment[, 1:pos1[1]]
        sumcgroups2 <- sumc1c2[(pos1[1] + 1):nc1c2]
        c21 <- c1[(pos1[1] + 1):nc1c2]
        c22 <- c2[(pos1[1] + 1):nc1c2]
        sumcgroups <- sumc1c2
        v12 <- 0
        activ_segment2 <- activ_segment[, (pos1[1] + 1):nc1c2]
        g1 <- intersect(sumcgroups1, sumcgroups)
        g2 <- intersect(sumcgroups2, sumcgroups)
        if (g1 == g2) {
            sumcgroups12 <- c(sumcgroups1, sumcgroups2)
            sizecombined1 <- length(sumcgroups12)
            Cc1 <- c(c11, c21)
            Cc2 <- c(c12, c22)
            activsegment1 <- cbind(activ_segment1, activ_segment2)
            combined1 <- sumcgroups12
            cmb1 <- 1
            v12 <- 1
        } else {
            combined1 <- sumcgroups1
            sizecombined1 <- length(sumcgroups1)
            activsegment1 <- activ_segment1
            Cc1 <- c11
            Cc2 <- c12
            cmb1 <- 0
        }  ## end if g1 == g2

        if (g2 == g2)
            {
                combined2 <- sumcgroups2
                sizecombined2 <- length(sumcgroups2)
                activsegment2 <- activ_segment2
                Cc21 <- c21
                Cc22 <- c22
                if (v12 == 1) {
                  cmb2 <- 9999
                } else {
                  cmb2 <- 0
                }
            }  # end if g2 == g2

        if ((sizecombined1 == q) & (cmb1 == 1)) {
            dd <- data.frame(activsegment1)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment1 <- as.matrix(dd[d4, ])
            rowsumactivesegment1 <- rowSums(activsegment1)
            g1 <- intersect(rowsumactivesegment1, rowsumactivesegment)
            lg1 <- length(g1)
            if (lg1 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc1)
                Nfactors2b <- c(Cc2)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }
        if ((sizecombined1 == q) & (cmb1 == 0)) {
            dd <- data.frame(activsegment1)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment1 <- as.matrix(dd[d4, ])
            rowsumactivesegment1 <- rowSums(activsegment1)
            g1 <- intersect(rowsumactivesegment1, rowsumactivesegment)
            lg1 <- length(g1)
            if (lg1 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc1)
                Nfactors2b <- c(Cc2)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }

        if ((sizecombined2 == q) & (cmb2 == 1)) {
            dd <- data.frame(activsegment2)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment2 <- as.matrix(dd[d4, ])
            rowsumactivesegment2 <- rowSums(activsegment2)
            g2 <- intersect(rowsumactivesegment2, rowsumactivesegment)
            lg2 <- length(g2)
            if (lg2 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc21)
                Nfactors2b <- c(Cc22)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }
        if ((sizecombined2 == q) & (cmb2 == 0)) {
            dd <- data.frame(activsegment2)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment2 <- as.matrix(dd[d4, ])
            rowsumactivesegment2 <- rowSums(activsegment2)
            g2 <- intersect(rowsumactivesegment2, rowsumactivesegment)
            lg2 <- length(g2)
            if (lg2 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc21)
                Nfactors2b <- c(Cc22)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }

        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 1) & (cmb2 == 1)) {
            activsegment12 <- cbind(activsegment1, activsegment2)
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc1, Cc21)
                Nfactors2b <- c(Cc2, Cc22)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }
        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 1) & (cmb2 == 0)) {
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc1, Cc21)
                Nfactors2b <- c(Cc2, Cc22)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }
        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 0) & (cmb2 == 1)) {
            activsegment12 <- cbind(activsegment1, activsegment2)
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc1, Cc21)
                Nfactors2b <- c(Cc2, Cc22)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }
        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 0) & (cmb2 == 0)) {
            activsegment12 <- cbind(activsegment1, activsegment2)
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                fRecrIDX1 <- fRecrIDX
                fRecrIDX1 <- fRecrIDX1[d4]
                z1 = length(fRecrIDX1)
                Nfactors2a <- c(Cc1, Cc21)
                Nfactors2b <- c(Cc2, Cc22)
                Nfactors2 <- cbind(Nfactors2a, Nfactors2b)
                z <- length(Nfactors2[, 1])
                Nfactors21 <- rep(Nfactors2[, 1], z1)
                Nfactors22 <- rep(Nfactors2[, 2], z1)
                Nfactors33 <- cbind(Nfactors21, Nfactors22)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  fRecrID2 <- rep(fRecrIDX1, times = z)
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  fRecrID2 <- t(as.matrix(fRecrID2))
                  FRecrID <- rbind(FRecrID, fRecrID2)
                }
            }
        }
        list11 <- list(Nfactors, FRecrID, Ngroups, levels_sr, Index_group_number)

    } else {
        ## end of if nRowMatrix == 1 if the number of vectors for the rows are more than one
        ## (eg. recreation area groups and activities)
        sumcgroups1 <- sumc1[1:pos1[1]]
        nc1 <- length(c1)
        rowsumactivesegment <- rowSums(as.matrix(activ_segment))
        c11 <- c1[1:pos1[1]]
        activ_segment1 <- activ_segment[, 1:pos1[1]]
        sumcgroups2 <- sumc1[(pos1[1] + 1):nc1]
        c21 <- c1[(pos1[1] + 1):nc1]
        sumcgroups <- sumc1
        v12 <- 0
        activ_segment2 <- activ_segment[, (pos1[1] + 1):nc1]
        g1 <- intersect(sumcgroups1, sumcgroups)
        g2 <- intersect(sumcgroups2, sumcgroups)
        if (g1 == g2) {
            sumcgroups12 <- c(sumcgroups1, sumcgroups2)
            sizecombined1 <- length(sumcgroups12)
            Cc1 <- c(c11, c21)
            activsegment1 <- cbind(activ_segment1, activ_segment2)
            combined1 <- sumcgroups12
            cmb1 <- 1
            v12 <- 1
        } else {
            combined1 <- sumcgroups1
            sizecombined1 <- length(sumcgroups1)
            activsegment1 <- activ_segment1
            Cc1 <- c11
            cmb1 <- 0
        }  ## end if g1 == g2

        if (g2 == g2)
            {
                combined2 <- sumcgroups2
                sizecombined2 <- length(sumcgroups2)
                activsegment2 <- activ_segment2
                Cc21 <- c21
                if (v12 == 1) {
                  cmb2 <- 9999
                } else {
                  cmb2 <- 0
                }
            }  # end if g2 == g2

        if ((sizecombined1 == q) & (cmb1 == 1)) {
            dd <- data.frame(activsegment1)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment1 <- as.matrix(dd[d4, ])
            rowsumactivesegment1 <- rowSums(activsegment1)
            g1 <- intersect(rowsumactivesegment1, rowsumactivesegment)
            lg1 <- length(g1)
            if (lg1 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc1)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }
        if ((sizecombined1 == q) & (cmb1 == 0)) {
            dd <- data.frame(activsegment1)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment1 <- as.matrix(dd[d4, ])
            rowsumactivesegment1 <- rowSums(activsegment1)
            g1 <- intersect(rowsumactivesegment1, rowsumactivesegment)
            lg1 <- length(g1)
            if (lg1 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc1)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }

        if ((sizecombined2 == q) & (cmb2 == 1)) {
            dd <- data.frame(activsegment2)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment2 <- as.matrix(dd[d4, ])
            rowsumactivesegment2 <- rowSums(activsegment2)
            g2 <- intersect(rowsumactivesegment2, rowsumactivesegment)
            lg2 <- length(g2)
            if (lg2 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc21)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }
        if ((sizecombined2 == q) & (cmb2 == 0)) {
            dd <- data.frame(activsegment2)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment2 <- as.matrix(dd[d4, ])
            rowsumactivesegment2 <- rowSums(activsegment2)
            g2 <- intersect(rowsumactivesegment2, rowsumactivesegment)
            lg2 <- length(g2)
            if (lg2 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc21)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }
        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 1) & (cmb2 == 1)) {
            activsegment12 <- cbind(activsegment1, activsegment2)
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc1, Cc21)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }
        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 1) & (cmb2 == 0)) {
            activsegment12 <- cbind(activsegment1, activsegment2)
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc1, Cc21)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }
        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 0) & (cmb2 == 1)) {
            activsegment12 <- cbind(activsegment1, activsegment2)
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc1, Cc21)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }
        if (((sizecombined1 + sizecombined2) == q) & (cmb1 == 0) & (cmb2 == 0)) {
            activsegment12 <- cbind(activsegment1, activsegment2)
            dd <- data.frame(activsegment12)
            d4 <- apply(dd, 1, function(row) all(row != 0))
            activsegment12 <- as.matrix(dd[d4, ])
            rowsumactivesegment12 <- rowSums(activsegment12)
            g12 <- intersect(rowsumactivesegment12, rowsumactivesegment)
            lg12 <- length(g12)
            if (lg12 > 0) {
                FbfactX1 <- fbfactX1
                FbfactX2 <- fbfactX2
                FbfactX1 <- FbfactX1[d4]
                FbfactX2 <- FbfactX1[d4]
                z1 = length(FbfactX1)
                Nfactors2 <- c(Cc1, Cc21)
                Nfactors2 <- t(as.matrix(Nfactors2))
                Nfactors2 <- t(as.matrix(Nfactors2))
                z <- length(Nfactors2)
                Nfactors21 <- rep(Nfactors2, z1)
                Nfactors33 <- c(Nfactors21)
                Nfactors <- rbind(Nfactors, Nfactors33)
                if (z1 > 0) {
                  Index_group_number <- Index_group_number + 1
                  vgroups <- groups[Index_group_number]
                  Ngroups2 <- rep(vgroups, times = z * z1)
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups2 <- t(as.matrix(Ngroups2))
                  Ngroups <- rbind(Ngroups, Ngroups2)
                  Fbfact1 <- rep(FbfactX1, times = z)
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact1 <- t(as.matrix(Fbfact1))
                  Fbfact11 <- rbind(Fbfact11, Fbfact1)
                  Fbfact2 <- rep(FbfactX2, times = z)
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact2 <- t(as.matrix(Fbfact2))
                  Fbfact22 <- rbind(Fbfact22, Fbfact2)
                }
            }
        }
        list11 <- list(Nfactors, Fbfact11, Fbfact22, Ngroups, levels_sr, Index_group_number)
    }
    return(list11)
}
