library(magrittr)

config <- config::get(file = "inst/golem-config.yml")

mdb_collection_pull <- function(connection_string = NULL, collection_name = NULL, db_name = NULL) {
  # Connect to the MongoDB collection
  collection <- mongolite::mongo(collection = collection_name, db = db_name, url = connection_string)

  # Retrieve the metadata document
  metadata <- collection$find(query = '{"type": "metadata"}')

  # Retrieve all data documents
  data <- collection$find(query = '{"type": {"$ne": "metadata"}}')

  if (nrow(metadata) > 0 && "columns" %in% names(metadata)) {
    stored_columns <- metadata$columns[[1]]

    # Ensure all stored columns exist in the data
    for (col in stored_columns) {
      if (!(col %in% names(data))) {
        data[[col]] <- NA
      }
    }

    # Reorder columns to match stored order, and include any extra columns at the end
    data <- data[, c(stored_columns, setdiff(names(data), stored_columns))]
  }

  return(data)
}

summary_data <-
  mdb_collection_pull(
    connection_string = config$storage$mongodb$connection_string,
    collection_name = config$storage$mongodb$collection_name$summaries,
    db_name = config$storage$mongodb$database_name
  ) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(landing_date = as.Date(landing_date))

timeseries_month <-
  summary_data %>%
  dplyr::mutate(
    date_month = lubridate::floor_date(landing_date, unit = "month")
  ) %>%
  dplyr::group_by(sample_district, date_month) %>%
  dplyr::summarise(
    catch_kg = median(catch_kg, na.rm = TRUE),
    catch_price = median(catch_price, na.rm = TRUE),
    price_kg_USD = median(price_kg_USD, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::complete(sample_district, date_month, fill = list(
    catch_kg = NA_real_,
    catch_price = NA_real_,
    price_kg_USD = NA_real_
  )) %>%
  dplyr::rename(
    "Catch (kg)" = catch_kg,
    "Catch Value (MWK)" = catch_price,
    "Price per kg (USD)" = price_kg_USD
  )

usethis::use_data(timeseries_month, overwrite = TRUE)

n_submissions <-
  summary_data %>%
  dplyr::select(landing_date, submission_id) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    date_month = lubridate::floor_date(landing_date, unit = "month")
  ) %>%
  dplyr::group_by(date_month) %>%
  dplyr::summarise(n_submissions = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename("N. submissions" = n_submissions)


n_vessels <-
  summary_data %>%
  tidyr::separate(survey_id, into = c("id", "vessel", "catch"), sep = "-") %>%
  dplyr::select(landing_date, id, vessel) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    date_month = lubridate::floor_date(landing_date, unit = "month"),
    vessel = as.numeric(vessel)
  ) %>%
  dplyr::group_by(date_month) %>%
  dplyr::summarise(n_vessels = sum(vessel, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename("Vessels surveyed" = n_vessels)



n_catches <-
  summary_data %>%
  tidyr::separate(survey_id, into = c("id", "vessel", "catch"), sep = "-") %>%
  dplyr::select(landing_date, id, vessel, catch) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    date_month = lubridate::floor_date(landing_date, unit = "month"),
    catch = as.numeric(catch)
  ) %>%
  dplyr::group_by(date_month) %>%
  dplyr::summarise(n_catches = sum(catch, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::rename("Catches recorded" = n_catches)


homecards_plots <-
  list(
    submissions = n_submissions,
    vessels = n_vessels,
    catches = n_catches
  )

usethis::use_data(homecards_plots, overwrite = TRUE)



### map data ###

map_data <-
  summary_data %>%
  dplyr::select(submission_id, lat, lon) %>%
  dplyr::distinct() %>%
  dplyr::select(lat, lon)

usethis::use_data(map_data, overwrite = TRUE)


### table data ###

table_data <-
  summary_data %>%
  dplyr::filter(n_fishers != 0 & trip_length != 0) %>%
  dplyr::mutate(
    price_kg = catch_price / catch_kg,
    cpue = (catch_kg / n_fishers) / trip_length
  ) %>%
  dplyr::group_by(sample_district) %>%
  dplyr::summarise(dplyr::across(c(catch_kg, cpue, catch_price, price_kg, n_fishers, trip_length), ~ mean(.x, na.rm = TRUE))) %>%
  dplyr::rename(
    "Catch (kg)" = catch_kg,
    "Catch per unit effort (kg)" = cpue,
    "Catch Value (MWK)" = catch_price,
    "Price per kg (MWK)" = price_kg,
    "Trip length (hrs)" = trip_length,
    "N. fishers" = n_fishers
  )

usethis::use_data(table_data, overwrite = TRUE)


### spider catch data ###

spider_data <-
  summary_data %>%
  dplyr::mutate(
    month = lubridate::month(landing_date, label = TRUE, abbr = TRUE),
    month = as.character(month),
    price_kg = catch_price / catch_kg
  ) %>%
  dplyr::group_by(sample_district, month) %>%
  dplyr::summarise(
    catch_kg = median(catch_kg, na.rm = TRUE),
    catch_price = median(price_kg, na.rm = TRUE),
    price_kg = median(price_kg, na.rm = TRUE),
  ) %>%
  dplyr::ungroup() %>%
  tidyr::complete(month, sample_district, fill = list(catch_kg = NA_real_)) %>%
  dplyr::rename(
    "District" = sample_district,
    "Catch (kg)" = catch_kg,
    "Catch Value (MWK)" = catch_price,
    "Price per kg (MWK)" = price_kg
  )

month_order <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
# Ensure data is ordered correctly
spider_data <-
  spider_data %>%
  dplyr::arrange(match(month, month_order), District)


usethis::use_data(spider_data, overwrite = TRUE)



treeplot_data <-
  summary_data %>%
  dplyr::filter(n_fishers != 0 & trip_length != 0) %>%
  dplyr::mutate(
    cpue = (catch_kg / n_fishers) / trip_length,
    rpue = (catch_price / n_fishers) / trip_length,
  ) %>%
  # First calculate district-specific values
  dplyr::group_by(sample_district, gear) %>%
  dplyr::summarise(
    cpue = mean(cpue, na.rm = TRUE),
    rpue = mean(rpue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Then calculate "All districts" values
  dplyr::bind_rows(
    dplyr::group_by(., gear) %>%
      dplyr::summarise(
        sample_district = "All districts",
        cpue = mean(cpue, na.rm = TRUE),
        rpue = mean(rpue, na.rm = TRUE),
        .groups = "drop"
      )
  ) %>%
  dplyr::mutate(
    cpue = round(cpue, 3),
    rpue = round(rpue, 3),
    gear = ifelse(gear == "other_gear", "Other gears", gear)
  ) %>%
  tidyr::pivot_longer(cols = c(cpue, rpue), names_to = "metric", values_to = "value") %>%
  dplyr::ungroup() %>%
  tidyr::complete(sample_district, gear, metric, fill = list(value = NA_real_)) %>%
  split(.$metric) %>%
  purrr::map(~ .x %>% dplyr::select(-metric)) %>%
  purrr::map(~ .x %>% dplyr::arrange(sample_district, dplyr::desc(value)))

usethis::use_data(treeplot_data, overwrite = TRUE)
