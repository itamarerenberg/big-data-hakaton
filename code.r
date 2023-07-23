library("RSQLite")
library("tidyverse")
library("recommenderlab")
library("stringr")
library("dplyr")
library("caret")
library("cgwtools")

my_similarity_users <- function(rat_mat_1, rat_mat_2) {
    n <- nrow(rat_mat_1)
    m <- nrow(rat_mat_2)

    sim <- matrix(nrow = n, ncol = m)

    for (i in 1:n) {
        ui <- as(as(rat_mat_1[i], "matrix"), "vector")
        ui_fltr <- !is.na(ui)

        cat(paste(i, "\n"))

        for (j in 1:m) {
            uj <- as(as(rat_mat_2[j], "matrix"), "vector")

            uj_fltr <- !is.na(uj)

            cut_fltr <- ui_fltr & uj_fltr

            # d <- (ui[cut_fltr] %*% uj[cut_fltr]) / (sqrt(sum(ui[ui_fltr]**2)) * sqrt(sum(uj[uj_fltr]**2))) # cosine similarity
            d <- sum(cut_fltr) / (sqrt(sum((ui[cut_fltr] - uj[cut_fltr])**2)) + 1) # euclidean similarity

            tryCatch(
                {
                    d <- ifelse(is.na(d) | !is.numeric(d), 0, d)
                    sim[i, j] <- d
                },
                error = function(err) {
                    cat(paste("err: ", err, "\n", "d: ", d, "\n"))
                }
            )
        }
    }

    # rm(sim)
    # gc()
    rownames(sim) <- rownames(rat_mat_1)
    colnames(sim) <- rownames(rat_mat_2)
    return(sim)
}

my_similarity_books <- function(rat_mat_1, rat_mat_2) {
    n <- ncol(rat_mat_1)
    m <- ncol(rat_mat_2)

    sim <- matrix(nrow = n, ncol = m)

    for (i in 1:n) {
        bi <- as(as(rat_mat_1[, i], "matrix"), "vector")
        bi_fltr <- !is.na(bi)

        cat(paste(i, "\n"))

        for (j in 1:m) {
            bj <- as(as(rat_mat_2[, j], "matrix"), "vector")

            bj_fltr <- !is.na(bj)

            cut_fltr <- bi_fltr & bj_fltr

            # d <- (ui[cut_fltr] %*% uj[cut_fltr]) / (sqrt(sum(ui[ui_fltr]**2)) * sqrt(sum(uj[uj_fltr]**2))) # cosine similarity
            d <- sum(cut_fltr) / (sqrt(sum((bi[cut_fltr] - bj[cut_fltr])**2)) + 1) # euclidean similarity

            tryCatch(
                {
                    d <- ifelse(is.na(d) | !is.numeric(d), 0, d)
                    sim[i, j] <- d
                },
                error = function(err) {
                    cat(paste("err: ", err, "\n", "d: ", d, "\n"))
                }
            )
        }
    }

    # rm(sim)
    # gc()
    rownames(sim) <- colnames(rat_mat_1)
    colnames(sim) <- colnames(rat_mat_2)
    return(sim)
}

my_similarity <- function(rat_mat_1, rat_mat_2, which = "users") {
    if (which == "users") {
        return(my_similarity_users(rat_mat_1, rat_mat_2))
    } else if (which == "books") {
        return(my_similarity_books(rat_mat_1, rat_mat_2))
    } else {
        return(which)
    }
}

write.sim_mat <- function(sim_mat, file_path) {
    if (!file.exists(file_path)) {
        file.create(file_path)
    }

    write.table(sim_mat, file = file_path)
}

read.sim_mat <- function(file_path) {
    return(read.table(file_path))
}

calck_tile_similarity <- function(rating_mat_1, rating_mat_2, subset_indices_r, subset_indices_c, which, file_format) {
    # calc and write to disk all parts of the result sim_mat
    for (i in 1:length(subset_indices_r)) {
        si <- subset_indices_r[i]
        for (j in 1:length(subset_indices_c)) {
            sj <- subset_indices_c[j]
            cat(paste0("calc ", i, "x", j, " of ", length(subset_indices_r), "x", length(subset_indices_c), "\n"))
            # moc
            sim <- my_similarity(rating_mat_1[si[[1]]], rating_mat_2[sj[[1]]]) # , which = which, method = "cosine")
            sim <- replace(sim, is.na(sim), 0)
            # sim <- sim

            cat(paste0("write to file sim_", i, "x", j, " to disk\n"))
            write.sim_mat(sim, file = file_format(i, j))
        }
    }

    gc()
}

load_tile_similarity <- function(n, m, subset_indices_r, subset_indices_c, file_format) {
    # num_splits_r <- ceiling(n / max_size) # Number of splits
    # num_splits_c <- ceiling(m / max_size)
    # subset_indices_r <- split(1:n, rep(1:num_splits_r, each = max_size))
    # subset_indices_c <- split(1:m, rep(1:num_splits_c, each = max_size))


    sim_mat <- matrix(nrow = n, ncol = m)

    # read back from disk and insert the resukt into <sim_mat>
    for (i in 1:length(subset_indices_r)) {
        si <- subset_indices_r[i]
        for (j in 1:length(subset_indices_c)) {
            cat(paste0("reading ", i, "x", j, " out of ", length(subset_indices_r), "x", length(subset_indices_c), "\n"))

            sj <- subset_indices_c[j]

            sim <- read.sim_mat(file_format(i, j))


            sim_mat[si[[1]], sj[[1]]] <- as.matrix(sim)
        }
    }
    return(sim_mat)
}

tile_similarity <- function(rating_mat_1, rating_mat_2 = -1, which = "users", max_size = 20000, recalck = TRUE) {
    if (is.integer(rating_mat_2)) {
        rating_mat_2 <- rating_mat_1
    }

    if (which == "items") {
        # r_names_1 <- row_names

        rating_mat_1 <- as(rating_mat_1, "matrix")
        rating_mat_1 <- as(t(rating_mat_1), "realRatingMatrix")

        rating_mat_2 <- as(rating_mat_2, "matrix")
        rating_mat_2 <- as(t(rating_mat_2), "realRatingMatrix")
    }


    file_format <- function(i, j) {
        return(paste0("calculations/sim_mat/tile_", i, "x", j, ".Rdata"))
    }

    n <- nrow(rating_mat_1)
    m <- nrow(rating_mat_2)


    # preper the splits indices:
    # n = 4, max_size = 2 => subset_indices = [[1, 2], [3, 4]];
    # n = 5, max_size = 2 => subset_indices = [[1, 2], [3, 4], [4, 5]];

    num_splits_r <- ceiling(n / ceiling(sqrt(max_size))) # Number of splits
    num_splits_c <- ceiling(m / ceiling(sqrt(max_size)))
    subset_indices_r <- split(1:n, rep(1:num_splits_r, each = ceiling(sqrt(max_size))))
    subset_indices_c <- split(1:m, rep(1:num_splits_c, each = ceiling(sqrt(max_size))))

    if (recalck) {
        calck_tile_similarity(rating_mat_1, rating_mat_2, subset_indices_r, subset_indices_c, which = "users", file_format)
    }

    cat("reading data back from disk")
    res <- load_tile_similarity(nrow(rating_mat_1), nrow(rating_mat_2), subset_indices_r, subset_indices_c, file_format)

    rownames(res) <- rownames(rating_mat_1)
    colnames(res) <- rownames(rating_mat_2)
    return(res)
}

##################### load data ############################

books.db <- dbConnect(RSQLite::SQLite(), "db/BX-Books_hkv1.db")
books <- dbReadTable(books.db, "bx-books")
books <- distinct(books)
rm(books.db) # clean RAM


ratings.db <- dbConnect(RSQLite::SQLite(), "db/BX-Ratings_hkv1.db")
ratings <- dbReadTable(ratings.db, "bx-book-ratings")
rm(ratings.db) # clean RAM

users.db <- dbConnect(RSQLite::SQLite(), "db/BX-Users_hkv1.db")
users <- dbReadTable(users.db, "bx-users")
rm(users.db) # clean RAM


# 3a
nb_users <- nrow(users[!duplicated(users$User.ID), ])
# 3b
nb_books <- nrow(books[!duplicated(books$ISBN), ])
# 3c
nb_ratings <- nrow(ratings[ratings$Book.Rating, ])

# 3d
N <- 50
user_ratings <- data.frame(table(ratings$User.ID))
freq.user <- filter(user_ratings, Freq >= N)
freq.user <- arrange(freq.user, desc(Freq))
colnames(freq.user) <- c("ID", "Frequence")

filter(ratings, User.ID %in% freq.user$ID)


# 3e
N2 <- 7
book.freq.ratings <- data.frame(table(ratings$ISBN))
filter(book.freq.ratings, Freq >= N2)
# 3f
arrange(book.freq.ratings, desc(Freq))
# 3g
arrange(user_ratings, desc(Freq))



ratings <- ratings[(ratings$ISBN %in% books$ISBN), ] # remove unknowns ISBNs from ratings.
ratings <- ratings[(ratings$User.ID %in% users$User.ID), ] # remove unknowns ISBNs from ratings.


create_rating_matrix <- function(ratings, min.rating.user = 4, min.rating.book = 4) {
    real.rating.matrix <- as(ratings, "realRatingMatrix")

    # remove users and books with less ratings then <min.rating.book> and <min.rating.user>
    # after one iteration there culd be new users or books that need to be removed
    while (min(rowCounts(real.rating.matrix)) < min.rating.user || min(colCounts(real.rating.matrix)) < min.rating.book) {
        real.rating.matrix <- real.rating.matrix[rowCounts(real.rating.matrix) >= min.rating.user, colCounts(real.rating.matrix) >= min.rating.book]
        cat(".")
    }

    cat(paste("\nMin rating user", min(rowCounts(real.rating.matrix)), "\n", collapse = " "))
    cat(paste("Min rating book", min(colCounts(real.rating.matrix)), "\n", collapse = " "))
    cat(paste("Dim of rating matrix is:", real.rating.matrix@data@Dim[1], ":", real.rating.matrix@data@Dim[2], "(", object.size(real.rating.matrix) / 1000000000, "Gb)\n", collapse = " "))

    gc()
    return(real.rating.matrix)
}

calck_pred_mat_users <- function(given_rrm, sim_mat) {
    n <- ncol(given_rrm)
    m <- ncol(sim_mat)
    pred <- matrix(nrow = n, ncol = m)

    for (i in 1:n) {
        given <- as(as(given_rrm[, i], "matrix"), "vector")

        given_fltr <- !is.na(given)

        cat(paste(i, "\n"))
        for (j in 1:m) {
            sim <- sim_mat[, j]

            sim_fltr <- sim != 0

            cut_fltr <- sim_fltr & given_fltr

            p <- sum(sim[cut_fltr] * given[cut_fltr])
            pred[i, j] <- ifelse(is.na(p), 0, p)
        }
    }

    rownames(pred) <- colnames(given_rrm)
    colnames(pred) <- colnames(sim_mat)

    return(pred)
}

calck_pred_mat_books <- function(given_rrm, sim_mat) {
    n <- nrow(given_rrm)
    m <- ncol(sim_mat)

    pred <- matrix(nrow = n, ncol = m)

    for (i in 1:n) {
        given <- as(as(given_rrm[i], "matrix"), "vector")

        given_fltr <- !is.na(given)

        cat(paste(i, "\n"))
        for (j in 1:m) {
            sim <- sim_mat[, j]

            sim_fltr <- sim != 0

            cut_fltr <- sim_fltr & given_fltr

            p <- sum(sim[cut_fltr] * given[cut_fltr])
            pred[i, j] <- ifelse(is.na(p), 0, p)
        }
    }

    rownames(pred) <- colnames(given_rrm)
    colnames(pred) <- colnames(sim_mat)

    return(pred)
}

calck_pred_mat <- function(given_rrm, sim_mat, which = "users") {
    if (which == "users") {
        return(calck_pred_mat_users(given_rrm, sim_mat))
    } else if (which == "books") {
        return(calck_pred_mat_books(given_rrm, sim_mat))
    } else {
        return(which)
    }
}


# calck an write to disk the pred mat,
# based on given_rrm (real rating mat), sim_mat(similarity matrix)
# return: the over all size of pred_mat (nrow, ncol)
calck_tile_pred_mat <- function(given_rrm, sim_mat, subset_indices, file_format, which = "users") {
    # calc and write to disk all parts of the result sim_mat
    for (i in 1:length(subset_indices)) {
        si <- subset_indices[i]

        # creat a matrix object from given_rrm ("realRatingMatrix" class) for computation
        if (which == "users") {
            given_rrm_mat <- as(given_rrm[, si[[1]]], "matrix")
        } else {
            given_rrm_mat <- as(given_rrm[si[[1]], ], "matrix")
        }
        given_rrm_mat <- replace(given_rrm_mat, is.na(given_rrm_mat), 0)

        # calck one tile of pred_mat
        cat(paste0("calc ", i, " of ", length(subset_indices), "\n"))
        if (which == "users") {
            pred_mat <- as.matrix(t(given_rrm_mat) %*% sim_mat)
        } else {
            pred_mat <- as.matrix(given_rrm_mat %*% sim_mat)
        }

        # wite to disk
        cat(paste0("write to file pred_mat_", i, " to disk\n"))
        write.sim_mat(pred_mat, file = file_format(i))
    }

    krow <- ifelse(which == "users", ncol(given_rrm), nrow(given_rrm))
    kcol <- ncol(sim_mat)

    return(c(krow, kcol))
}

load_tile_pred_mat <- function(krow, kcol, subset_indices, file_format) {
    # num_splits_c <- ceiling(krow / max_size)
    # subset_indices_c <- split(1:krow, rep(1:num_splits_c, each = max_size))


    pred_mat <- matrix(nrow = krow, ncol = kcol)

    # read back from disk and insert the resukt into <sim_mat>
    for (i in 1:length(subset_indices)) {
        si <- subset_indices[i]
        cat(paste0("reading ", i, " out of ", length(subset_indices), "\n"))

        pred <- read.sim_mat(file_format(i))
        pred_mat[si[[1]], ] <- as.matrix(pred)
    }
    return(pred_mat)
}

tile_pred_mat <- function(given_rrm, sim_mat, max_size = 5000000, which = "users") {
    file_format <- function(i) {
        return(paste("calculations/pred_mat/pred_mat_", i, ".Rdata"))
    }

    # generate split indices
    space_in_use <- ncol(given_rrm) * nrow(given_rrm)

    n <- ifelse(which == "users", ncol(given_rrm), nrow(given_rrm))
    m <- ifelse(which == "users", nrow(given_rrm), ncol(given_rrm))

    num_splits <- ceiling(space_in_use / max_size) # Number of splits
    subset_indices <- split(1:n, rep(1:num_splits, each = max_size / m)) # important! max_(size/m) * num_splits = n


    size <- calck_tile_pred_mat(given_rrm, sim_mat, subset_indices, file_format, which = which)
    krow <- size[1]
    kcol <- size[2]

    pred_mat <- load_tile_pred_mat(krow, kcol, subset_indices, file_format)

    rownames(pred_mat) <- colnames(given_rrm)
    colnames(pred_mat) <- colnames(sim_mat)

    return(pred_mat)
}

predict_books_for_user <- function(given_rrm, new_rrm, k_rec = 10) {
    # k - number of books to find to the new user

    # comm
    sim_mat <- my_similarity(given_rrm, new_rrm)

    for (i in 1:ncol(sim_mat)) {
        k <- nrow(sim_mat) - 20 # ceiling(0.9 * nrow(sim_mat))
        sim_mat[head(order(sim_mat[, i]), k), i] <- 0 # filter the smallest 70% of the values

        # normalize sim_mat
        w <- 1 / sum(sim_mat[, i])
        if (is.infinite(w) | is.na(w)) {
            w <- 0
        }
        sim_mat[, i] <- sim_mat[, i] * w
    }

    pred_mat <- calck_pred_mat(given_rrm, sim_mat)

    new_rrm_mat <- as(new_rrm, "matrix")
    new_rrm_mat <- replace(new_rrm_mat, is.na(new_rrm_mat), 0)

    # remove books that the user read already
    pred_mat[t(new_rrm_mat) >= 1] <- 0

    min_val <- min(pred_mat)
    max_val <- max(pred_mat)

    pred_mat <- ((pred_mat - min_val) / (max_val - min_val)) * 9

    # pred_mat <- pred_mat - t(new_rrm_mat)

    recomended_books <- list()
    wached_books <- list()

    for (i in 1:nrow(new_rrm)) {
        col <- pred_mat[, i]
        rec <- col[order(col, decreasing = FALSE)]
        # rec <- rec[rec >= 5]
        recomended_books[i] <- list(tail(rec, k_rec))

        rm(col)
        rm(rec)

        row <- new_rrm_mat[i, ]
        rec <- row[order(row, decreasing = FALSE)]
        # rec <- rec[rec >= 5]
        wached_books[i] <- list(tail(rec, k_rec))

        # rm(col)
        # rm(rec)
    }
    rm(sim_mat)
    rm(pred_mat)
    return(recomended_books)
}

test_books_for_user <- function(rrm, train_ratio = 0.99) {
    users_list <- rownames(rrm)
    sample_users <- sample(c(TRUE, FALSE), length(users_list), replace = TRUE, prob = c(train_ratio, 1 - train_ratio)) # nolint

    given_rrm <- rrm[users_list[sample_users]]
    new_rrm <- rrm[users_list[!sample_users]]

    books_list <- colnames(rrm)
    sample_books <- sample(c(TRUE, FALSE), length(books_list), replace = TRUE, prob = c(0.99, 0.01))

    pred_test <- new_rrm
    pred_test[, books_list[!sample_books]] <- NA
    new_rrm[, books_list[sample_books]] <- NA

    pred_books <- predict_books_for_user(given_rrm, new_rrm)

    given_books <- as(pred_test, "matrix")
    given_books <- replace(given_books, is.na(given_books), 0)
    acc <- 0

    pos <- 0
    neg <- 0
    tp <- 0
    tn <- 0
    fp <- 0
    for (i in 1:length(pred_books)) {
        pb <- tail(pred_books[i][[1]], 10)
        rec_names <- names(pb)
        rec <- lapply(pb, as.numeric)
        nr_mat <- as(new_rrm, "matrix")
        nr_mat <- replace(nr_mat, is.na(nr_mat), 0)
        test <- nr_mat[i, rec_names]

        P <- test > 0
        pred <- rec > 0

        pos <- pos + sum(P)
        neg <- neg + sum(!P)
        tp <- tp + sum(P & pred)
        fp <- fp + sum((!P) & pred)
        tn <- tn + sum((!P) & (!pred))
    }

    score <- (tp) / (tp + fp)
    return(score)
    # k - number of books to find to the new user
}

rrm <- create_rating_matrix(ratings)
rrm <- rrm[1:10000, ]

given_rrm <- rrm[1:floor(nrow(rrm) * 0.999)]

new_rrm <- rrm[floor(nrow(rrm) * 0.999 + 1):(nrow(rrm))]

pred_users <- predict_books_for_user(given_rrm, new_rrm)

rm(given_rrm)
rm(new_rrm)
rm(rrm)

predict_users_for_book <- function(given_rrm, new_rrm, k_rec = 10) {
    sim_mat <- my_similarity(given_rrm, new_rrm, which = "books")

    for (i in 1:ncol(sim_mat)) {
        k <- nrow(sim_mat) - 10 # ceiling(0.9 * nrow(sim_mat))
        sim_mat[head(order(sim_mat[, i]), k), i] <- 0 # filter the smallest 70% of the values

        # normalize sim_mat
        w <- 1 / sum(sim_mat[, i])
        if (is.infinite(w) | is.na(w)) {
            w <- 0
        }
        sim_mat[, i] <- sim_mat[, i] * w
    }

    pred_mat <- calck_pred_mat(given_rrm, sim_mat, "books")

    new_rrm_mat <- as(new_rrm, "matrix")
    new_rrm_mat <- replace(new_rrm_mat, is.na(new_rrm_mat), 0)

    # pred_mat <- pred_mat - t(new_rrm_mat)

    recomended_books <- list()
    wached_books <- list()

    for (i in 1:nrow(new_rrm)) {
        col <- pred_mat[, i]
        rec <- col[order(col, decreasing = FALSE)]
        # rec <- rec[rec >= 5]
        recomended_books[i] <- list(tail(rec, k_rec))

        rm(col)
        rm(rec)

        row <- new_rrm_mat[i, ]
        rec <- row[order(row, decreasing = FALSE)]
        # rec <- rec[rec >= 5]
        wached_books[i] <- list(tail(rec, k_rec))

        # rm(col)
        # rm(rec)
    }
    rm(sim_mat)
    # rm(w_vec)
    # rm(given_rrm_mat)
    rm(pred_mat)
    return(recomended_books)
}

# rrm_b <- create_rating_matrix(ratings)
rm(rrm)

rrm <- create_rating_matrix(ratings)
rrm <- rrm[, 1:1000] # reduce number of books for esy debuging

given_rrm <- rrm[, 1:floor(ncol(rrm) * 0.99)]

new_rrm <- rrm[, floor(ncol(rrm) * 0.99 + 1):(ncol(rrm))]

pred_books <- predict_books_for_user(given_rrm, new_rrm)
