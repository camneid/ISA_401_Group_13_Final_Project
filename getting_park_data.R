
# ==========================================
# ISA 401 - AMUSEMENT PARK WAIT TIME SCRAPER
# ==========================================

# STEP 1: Install packages (only needed once)
install.packages(c(
  "tidyverse",
  "rvest",
  "janitor",
  "jsonlite"
))

# STEP 2: Load libraries
library(tidyverse)
library(rvest)
library(janitor)
library(jsonlite)

# STEP 3: Get every park from Queue Times
park_groups <- fromJSON(
  "https://queue-times.com/parks.json",
  simplifyVector = FALSE
)

parks <- map_dfr(park_groups, function(group) {
  map_dfr(group$parks, function(park) {
    tibble(
      park_id = as.integer(park$id),
      park_name = park$name,
      country = park$country
    )
  })
})

# View the full park list
View(parks)

# STEP 4: Choose which parks to scrape
# TRUE = test on 3 parks
# FALSE = scrape every park
test_mode <- TRUE

if (test_mode) {
  parks_to_scrape <- parks |>
    filter(park_name %in% c(
      "Cedar Point",
      "Kings Island",
      "Hersheypark"
    ))
} else {
  parks_to_scrape <- parks
}

# STEP 5: Create empty storage tables
all_wait_times <- tibble(
  park_id = integer(),
  park_name = character(),
  country = character(),
  ride = character(),
  avg_wait_mins = double()
)

failed_parks <- tibble(
  park_id = integer(),
  park_name = character(),
  reason = character()
)

# STEP 6: Loop through the parks
for (i in seq_len(nrow(parks_to_scrape))) {

  park_id <- parks_to_scrape$park_id[i]
  park_name <- parks_to_scrape$park_name[i]
  country <- parks_to_scrape$country[i]

  url <- paste0(
    "https://queue-times.com/parks/",
    park_id,
    "/stats"
  )

  cat("Scraping:", park_name, "\n")

  tryCatch({

    # YOUR ORIGINAL SCRAPING CODE
    page <- read_html(url)

    all_tables <- page |>
      html_elements("table") |>
      html_table()

    matching_tables <- all_tables |>
      keep(~ any(str_detect(
        names(.x),
        regex("overall average", ignore_case = TRUE)
      )))

    # Stop if the expected table is missing
    if (length(matching_tables) == 0) {
      stop("Average wait time table not found")
    }

    avg_wait <- matching_tables |>
      pluck(1) |>
      clean_names() |>
      select(1:2) |>
      rename(
        ride = 1,
        avg_wait_mins = 2
      ) |>
      mutate(
        ride = str_squish(ride),
        avg_wait_mins = parse_number(
          as.character(avg_wait_mins)
        ),
        park_id = park_id,
        park_name = park_name,
        country = country
      ) |>
      relocate(
        park_id,
        park_name,
        country
      )

    # Combine with previously scraped parks
    all_wait_times <- bind_rows(
      all_wait_times,
      avg_wait
    )

    cat("Success:", nrow(avg_wait), "rides\n")

  }, error = function(e) {

    cat("FAILED:", park_name, "\n")

    failed_parks <- bind_rows(
      failed_parks,
      tibble(
        park_id = park_id,
        park_name = park_name,
        reason = conditionMessage(e)
      )
    )

  })

  # Wait 2 seconds before next request
  Sys.sleep(2)
}

# STEP 7: Check results
View(all_wait_times)

# Number of parks successfully scraped
n_distinct(all_wait_times$park_id)

# Total ride records
nrow(all_wait_times)

# Number of failed parks
nrow(failed_parks)

# STEP 8: Export results
write_csv(
  all_wait_times,
  "all_park_wait_times.csv"
)

write_csv(
  failed_parks,
  "failed_parks.csv"
)

# STEP 9: Find the saved files
getwd()



# ==============================================
# ISA 401 - COMPLETE ROLLER COASTER PROJECT
# CAPTAIN COASTER + QUEUE TIMES
# ==============================================

# Install packages once if needed:
# install.packages(c(
#   "tidyverse", "rvest", "stringi"
# ))

library(tidyverse)
library(rvest)
library(stringi)

# ==============================================
# PART 1: IMPORT QUEUE TIMES DATA
# ==============================================

wait_times <- read_csv(
  "all_park_wait_times.csv",
  show_col_types = FALSE
)

# ==============================================
# PART 2: COLLECT ALL 42 RANKING PAGES
# ==============================================

# Use only if automated collection is permitted.
# Start with 1:2 to test, then use 1:42.

page_numbers <- 1:42

all_links <- character()
failed_pages <- integer()

for (page_number in page_numbers) {
  
  url <- paste0(
    "https://captaincoaster.com/en/ranking/?page=",
    page_number
  )
  
  cat("Reading ranking page:", page_number, "\n")
  
  links <- tryCatch({
    
    page <- read_html(url)
    
    page |>
      html_elements("a[href*='/coasters/']") |>
      html_attr("href") |>
      url_absolute(url) |>
      unique() |>
      keep(~ str_detect(
        .x, "/coasters/[0-9]+/"
      ))
    
  }, error = function(e) {
    
    message(
      "Failed page ", page_number,
      ": ", conditionMessage(e)
    )
    
    failed_pages <<- c(
      failed_pages, page_number
    )
    
    character()
    
  })
  
  if (length(links) == 0) {
    failed_pages <- union(
      failed_pages, page_number
    )
  }
  
  all_links <- c(all_links, links)
  
  Sys.sleep(2)
}

coaster_links <- unique(all_links)

cat("Unique coaster links:", length(coaster_links), "\n")
cat("Failed pages:", failed_pages, "\n")

write_csv(
  tibble(coaster_url = coaster_links),
  "all_coaster_links.csv"
)

# Do not proceed if any ranking page failed
if (length(failed_pages) > 0) {
  stop("Some ranking pages failed. Review before continuing.")
}

# ==============================================
# PART 3: FUNCTION TO SCRAPE COASTER DETAILS
# ==============================================

scrape_coaster <- function(url) {
  
  page <- read_html(url)
  
  # Coaster name
  ride <- page |>
    html_element("h1") |>
    html_text2() |>
    str_squish()
  
  # Park name from page title
  page_title <- page |>
    html_element("title") |>
    html_text2()
  
  title_parts <- str_split(
    page_title,
    "\\s*•\\s*"
  )[[1]]
  
  if (length(title_parts) < 3) {
    stop("Could not extract park name")
  }
  
  park_name <- str_squish(
    title_parts[length(title_parts) - 1]
  )
  
  # Full webpage text
  page_text <- page |>
    html_element("body") |>
    html_text2() |>
    str_squish()
  
  # Extract quality score
  coaster_score <- str_match(
    page_text,
    regex(
      "SCORE\\s*([0-9]+(?:\\.[0-9]+)?)\\s*%",
      ignore_case = TRUE
    )
  )[, 2] |>
    as.numeric()
  
  # Extract worldwide ranking
  world_rank <- str_match(
    page_text,
    regex(
      "RANK\\s*#\\s*([0-9]+)",
      ignore_case = TRUE
    )
  )[, 2] |>
    as.integer()
  
  if (is.na(coaster_score) ||
      is.na(world_rank)) {
    stop("Score or ranking not found")
  }
  
  tibble(
    park_name = park_name,
    ride = ride,
    coaster_score = coaster_score,
    world_rank = world_rank,
    coaster_url = url
  )
}

# ==============================================
# PART 4: TEST THE FUNCTION
# ==============================================

test_url <- paste0(
  "https://captaincoaster.com/en/coasters/",
  "2268/steel-vengeance-cedar-point"
)

test_result <- scrape_coaster(test_url)

print(test_result)

stopifnot(
  test_result$ride == "Steel Vengeance",
  test_result$park_name == "Cedar Point",
  !is.na(test_result$coaster_score),
  !is.na(test_result$world_rank)
)

cat("Steel Vengeance test passed!\n")

# ==============================================
# PART 5: SCRAPE ALL COASTER PROFILES
# ==============================================

# Save results in batches so the collection
# can resume if RStudio closes.

progress_file <- "captain_coaster_progress.csv"

if (file.exists(progress_file)) {
  
  captain_rankings <- read_csv(
    progress_file,
    show_col_types = FALSE
  )
  
} else {
  
  captain_rankings <- tibble(
    park_name = character(),
    ride = character(),
    coaster_score = double(),
    world_rank = integer(),
    coaster_url = character()
  )
  
}

failed_links <- character()

remaining_links <- setdiff(
  coaster_links,
  captain_rankings$coaster_url
)

cat("Remaining coasters:", length(remaining_links), "\n")

for (i in seq_along(remaining_links)) {
  
  url <- remaining_links[i]
  
  cat(
    "Scraping", i, "of",
    length(remaining_links), "\n"
  )
  
  result <- tryCatch(
    scrape_coaster(url),
    error = function(e) {
      message(
        "Failed: ", url, " - ",
        conditionMessage(e)
      )
      NULL
    }
  )
  
  if (is.null(result)) {
    
    failed_links <- c(failed_links, url)
    
  } else {
    
    captain_rankings <- bind_rows(
      captain_rankings,
      result
    )
    
  }
  
  # Save every 25 records
  if (i %% 25 == 0) {
    
    write_csv(
      captain_rankings,
      progress_file
    )
    
    write_csv(
      tibble(coaster_url = failed_links),
      "failed_coaster_links.csv"
    )
  }
  
  Sys.sleep(2)
}

# Final save
captain_rankings <- captain_rankings |>
  distinct(coaster_url, .keep_all = TRUE)

write_csv(
  captain_rankings,
  "captain_coaster_rankings.csv"
)

write_csv(
  captain_rankings,
  progress_file
)

write_csv(
  tibble(coaster_url = failed_links),
  "failed_coaster_links.csv"
)

cat("Coasters collected:", nrow(captain_rankings), "\n")
cat("Failed profiles:", length(failed_links), "\n")

View(captain_rankings)

# ==============================================
# PART 6: CLEAN NAMES FOR MATCHING
# ==============================================

clean_name <- function(x) {
  
  x |>
    str_to_lower() |>
    stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^a-z0-9]", "")
  
}

wait_times <- wait_times |>
  mutate(
    park_key = clean_name(park_name),
    ride_key = clean_name(ride)
  )

captain_rankings <- captain_rankings |>
  mutate(
    park_key = clean_name(park_name),
    ride_key = clean_name(ride)
  )

# ==============================================
# PART 7: CHECK DUPLICATES
# ==============================================

duplicate_rankings <- captain_rankings |>
  count(park_key, ride_key) |>
  filter(n > 1)

if (nrow(duplicate_rankings) > 0) {
  stop("Duplicate coaster keys found. Review them.")
}

# ==============================================
# PART 8: MERGE WITH QUEUE TIMES
# ==============================================

combined <- wait_times |>
  inner_join(
    captain_rankings |>
      select(
        park_key,
        ride_key,
        coaster_score,
        world_rank,
        coaster_url
      ),
    by = c("park_key", "ride_key"),
    relationship = "many-to-one"
  )

cat("Matched coaster records:", nrow(combined), "\n")

View(combined)

# ==============================================
# PART 9: IDENTIFY UNMATCHED COASTERS
# ==============================================

unmatched <- captain_rankings |>
  anti_join(
    wait_times,
    by = c("park_key", "ride_key")
  )

write_csv(
  unmatched,
  "unmatched_coasters.csv"
)

View(unmatched)

# ==============================================
# PART 10: CALCULATE COASTER VALUE
# ==============================================

# Quality = 70%
# Wait time = 30%
# 120 minutes = maximum wait penalty

coaster_analysis <- combined |>
  filter(
    !is.na(coaster_score),
    !is.na(avg_wait_mins)
  ) |>
  mutate(
    wait_score = 100 * (
      1 - pmin(
        pmax(avg_wait_mins, 0),
        120
      ) / 120
    ),
    coaster_value = (
      0.70 * coaster_score +
        0.30 * wait_score
    )
  ) |>
  arrange(desc(coaster_value))

View(coaster_analysis)

# ==============================================
# PART 11: RANK AMUSEMENT PARKS
# ==============================================

# First rank each park's coasters by quality.
# Then calculate values for its best 5.

park_rankings <- coaster_analysis |>
  group_by(park_name) |>
  arrange(
    desc(coaster_score),
    .by_group = TRUE
  ) |>
  slice_head(n = 5) |>
  summarise(
    coasters_analyzed = n(),
    avg_quality = mean(coaster_score),
    avg_wait = mean(avg_wait_mins),
    avg_value = mean(coaster_value),
    .groups = "drop"
  ) |>
  arrange(desc(avg_value)) |>
  mutate(
    preliminary_rank = row_number()
  )

View(park_rankings)

# ==============================================
# PART 12: EXPORT FINAL DATA
# ==============================================

write_csv(
  coaster_analysis,
  "final_roller_coaster_analysis.csv"
)

write_csv(
  park_rankings,
  "final_amusement_park_rankings.csv"
)

cat("ALL FILES SAVED!\n")

# Find the files
getwd()

# ==============================================
# END
# ==============================================

write_csv(
  captain_rankings,
  "captain_coaster_progress.csv"
)

saved_data <- read_csv(
  "captain_coaster_progress.csv"
)

nrow(saved_data)

