#' Open a connection to the Wageningen SQLite DB
#'
#' Connects to a local SQLite DB file. If the file is not found locally,
#' it is downloaded from the published Zenodo open-access repository (record 22671742)
#' and stored in a persistent user cache directory.
#'
#' @param db_choice Integer or character: 1 or "basic" for Basic tables,
#'   2 or "complete" for Wageningen complete tables. Default is 1.
#' @param db_path Optional character. Direct file path or directory containing the SQLite DB file.
#' @param force_download Logical. If TRUE, re-downloads the database even if cached. Default is FALSE.
#' @return A DBI SQLite connection. Remember to close it with `DBI::dbDisconnect()`
#'   when you are done.
#' @examples
#' \dontrun{
#' conn <- ACRER::wageningen_conn()
#' DBI::dbListTables(conn)
#' DBI::dbDisconnect(conn)
#' }
#' @export
wageningen_conn <- function(db_choice = 1, db_path = NULL, force_download = FALSE) {
    # -----------------------------------------------------------------
    # 1. Determine target file name and Zenodo published URL
    # -----------------------------------------------------------------
    is_complete <- identical(as.character(db_choice), "2") ||
        tolower(as.character(db_choice)) %in% c("complete", "wageningen_complete_tables.db")

    db_file_name <- if (is_complete) {
        "Wageningen_complete_tables.db"
    } else {
        "Basic_Tables.db"
    }

    record_id <- "22671742"
    zenodo_url <- paste0(
        "https://zenodo.org/api/records/", record_id, "/files/", db_file_name, "/content"
    )

    # -----------------------------------------------------------------
    # 2. Check local locations and cache
    # -----------------------------------------------------------------
    resolved_db_path <- if (!is.null(db_path) && nzchar(db_path)) {
        if (dir.exists(db_path)) file.path(db_path, db_file_name) else db_path
    } else ""

    env_dir <- Sys.getenv("ACRER_DB_PATH", "")
    resolved_env_path <- if (nzchar(env_dir)) {
        if (dir.exists(env_dir)) file.path(env_dir, db_file_name) else env_dir
    } else ""

    cache_dir <- tools::R_user_dir("ACRER", which = "cache")
    cached_path <- file.path(cache_dir, db_file_name)

    possible_paths <- c(
        resolved_db_path,
        resolved_env_path,
        system.file("extdata", db_file_name, package = "ACRER"),
        cached_path
    )
    possible_paths <- possible_paths[nzchar(possible_paths)]
    existing <- possible_paths[file.exists(possible_paths)]

    if (length(existing) > 0 && !force_download) {
        chosen_path <- existing[1]
    } else {
        # -----------------------------------------------------------------
        # 3. Download from published Zenodo repository
        # -----------------------------------------------------------------
        if (!dir.exists(cache_dir)) {
            dir.create(cache_dir, recursive = TRUE)
        }

        message("Downloading '", db_file_name, "' from Zenodo repository (DOI: 10.5281/zenodo.22671742) into cache...")

        token <- Sys.getenv("ZENODO_PAT", Sys.getenv("ZENODO_TOKEN", ""))
        has_token <- nzchar(token)

        old_opt <- options(timeout = max(900, getOption("timeout", 60)))
        on.exit(options(old_opt), add = TRUE)

        dl_ok <- FALSE
        err_msg <- NULL

        tryCatch({
            if (requireNamespace("curl", quietly = TRUE)) {
                h <- curl::new_handle()
                if (has_token) {
                    curl::handle_setheaders(h, "Authorization" = paste("Bearer", token))
                }
                curl::curl_download(zenodo_url, destfile = cached_path, handle = h, quiet = FALSE)
            } else {
                extra_headers <- if (has_token) c("Authorization" = paste("Bearer", token)) else character(0)
                utils::download.file(
                    zenodo_url,
                    destfile = cached_path,
                    mode = "wb",
                    headers = extra_headers
                )
            }
            if (file.exists(cached_path) && file.info(cached_path)$size > 0) {
                dl_ok <- TRUE
            }
        }, error = function(e) {
            err_msg <<- conditionMessage(e)
        })

        if (!dl_ok) {
            if (file.exists(cached_path)) unlink(cached_path)
            stop(
                "Could not locate SQLite DB file '", db_file_name, "' locally,\n",
                "and automatic download from Zenodo failed.\n",
                if (!is.null(err_msg)) paste0("Details: ", err_msg, "\n") else "",
                "To resolve, you can:\n",
                "  1. Provide the path directly: wageningen_conn(db_path = '/path/to/directory_or_file')\n",
                "  2. Set the environment variable: Sys.setenv(ACRER_DB_PATH = '/path/to/directory_or_file')\n",
                "  3. Manually download from https://doi.org/10.5281/zenodo.22671742 and place into:\n",
                "     ", cached_path, "\n",
                call. = FALSE
            )
        }
        chosen_path <- cached_path
    }

    # -----------------------------------------------------------------
    # 4. Check required packages & connect
    # -----------------------------------------------------------------
    if (!requireNamespace("DBI", quietly = TRUE)) {
        stop("Package 'DBI' is required but not installed. Run install.packages('DBI').")
    }
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("Package 'RSQLite' is required but not installed. Run install.packages('RSQLite').")
    }

    conn <- DBI::dbConnect(RSQLite::SQLite(),
        dbname = chosen_path,
        flags = RSQLite::SQLITE_RO
    )
    return(conn)
}
