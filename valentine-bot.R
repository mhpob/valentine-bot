library(httr2)
library(rvest)
library(atrrr)

extract_tag <- function(html, tag) {
  html |>
    html_elements(tag) |>
    html_text() |>
    trimws()
}

poll_catalog <- function(dir) {
  if (dir == "VALCOLL") {
    table <- "objects"
    search_term <- "*"
    fields <- "cat_nbr"

    cat("Searching collections...", sep = "")
  } else {
    table <- "biblio"
    # Making up a random search term based on numbers
    search_term <- sample(c(0:format(Sys.Date(), "%Y"), "Cook"), 1)
    fields <- "biblio_nbr,group_nbr,series_nbr,fileunit_nbr"

    cat("Searching archives for \"", search_term, "\"...", sep = "")
  }

  request(
    "https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/FindItXmlStringPage"
  ) |>
    req_body_json(
      list(
        Words = search_term,
        TableName = table,
        Directory = dir,
        FieldList = fields,
        SortFields = "sortable15",
        OnlyIfImages = "false",
        IncludeImage = "false",
        RecordsPerPage = -1,
        AttachedMediaOption = " "
      )
    ) |>
    req_user_agent("Valentine Bot (https://github.com/mhpob/valentine-bot)") |>
    req_perform() |>
    resp_body_json() |>
    _$d |>
    read_html()
}


post_log <- read.csv("post.log", header = F, col.names = c("time", "url", "status"))
post_log$item <- gsub(".*ID=(.*)&db.*", '\\1', post_log$url)


dir <- sample(c("VALCOLL", "VALARCH"), 1)
catalog <- poll_catalog(dir)

if (dir == "VALCOLL") {
  catalog <- catalog |>
    extract_tag('objects')
} else {
  catalog <- paste0(
    extract_tag(catalog, 'group_nbr'),
    "/",
    extract_tag(catalog, 'series_nbr'),
    "-",
    extract_tag(catalog, 'fileunit_nbr'),
    "#",
    extract_tag(catalog, 'biblio_nbr')
  )
  # Archives contain manuscript collections ("MSC") and photo collections ("PHC")
  #   Select only photos for the purposes of this bot.
  catalog <- catalog[!grepl("MSC", catalog)]
}

cat("done.\n", length(catalog), "items found.\nSelecting item...\n")

while (TRUE) {
  item <- sample(catalog, 1)
  if (item %in% post_log$time) {
    cat("Tried ", item, "; already posted.\n", sep = '')
  } else {
  item_record <- "https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetRecordDetails" |>
    request() |>
    req_body_json(
      list(
        TableName = ifelse(dir == "VALCOLL", "objects", "biblio"),
        Directory = dir,
        FieldList = ifelse(
          dir == "VALCOLL",
          "record_id,file_name,ExhibitId,db_exhibt_exhibt_dsc,cat_nbr,cat_nam,form,artist,categ_16,obj_mem,material,type,categ_4",
          "record_id,biblio_nbr,group_nbr,series_nbr,fileunit_nbr,edition,title,author,sortable14,origin,categ_12,categ_16,categ_1,categ_9[2],categ_3,user_2,user_4,sub_pers,sub_corp,sub_topic,sub_geo,categ_6,categ_7,categ_8,categ_13,subjects,categ_20,file_name"
        ),
        IncludeImage = "true",
        RecordID = -1,
        readablePrimaryKey = item
      )
    ) |>
    req_perform() |>
    resp_body_json() |>
    _$d |>
    read_html()

  item_info <- data.frame(
    id = item,
    file_name = extract_tag(item_record, 'file_name')
  )

  cat("Checking ", item_info$id, "for images...\n", sep = '')
  
  image_check <- 'https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetImagePaths' |>
    request() |>
    req_body_json(
      list(
        Directory = dir,
        ImageKey = item_info$file_name,
        FieldList = "imagedescription"
      )
    ) |>
    req_perform() |>
    resp_body_json() |>
    _$d

  if (length(image_check >= 1)) break else cat("  none found.\n")
  }
}


# Print for possible debugging
cat(item_info$id, '\nGetting item record...', sep = '')


if (dir == "VALCOLL") {
  item_info <- data.frame(
    id = extract_tag(item_record, 'cat_nbr'),
    title = extract_tag(item_record, "cat_nam"),
    file_name = extract_tag(item_record, "file_name"),
    date = extract_tag(item_record, "categ_16"),
    creator = extract_tag(item_record, "artist"),
    description = extract_tag(item_record, "obj_mem"),
    collection = extract_tag(item_record, "categ_4")
  )
} else {
  item_info <- data.frame(
    title = extract_tag(item_record, "title"),
    id = item_info$id,
    date = extract_tag(item_record, 'origin'),
    description = extract_tag(item_record, "categ_16"),
    file_name = item_info$file_name,
    creator = extract_tag(item_record, "author")
  )
}

for (i in 1:ncol(item_info)) {
  item_info[, i] <- gsub("\\s?(--|__)", "; ", x = item_info[, i])
}

item_url <- paste0(
  "https://valentine.rediscoverysoftware.com/",
  ifelse(dir == "VALCOLL", "mDetail", "MADetailB"),
  ".aspx?rID=",
  item_info$id,
  "&db=",
  ifelse(dir == "VALCOLL", 'objects', 'biblio'),
  "&dir=",
  dir
) |>
  URLencode() |>
  gsub(",", "%2C", x = _)

cat("done.\nItem URL:", item_url, "\nFinding image URLs...")

images <- 'https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetImagePaths' |>
  request() |>
  req_body_json(
    list(
      Directory = dir,
      ImageKey = item_info$file_name,
      FieldList = "imagedescription"
    )
  ) |>
  req_perform() |>
  resp_body_json() |>
  _$d |>
  read_html()

image_info <- data.frame(
  image_path = extract_tag(images, "fullimage") |>
    gsub("\\\\", "/", x = _)
)
image_info$url <- paste0(
  "https://valentine.rediscoverysoftware.com/FullImages",
  image_info$image_path
) |>
  URLencode()

cat('done.\nImage URLs:\n', paste(image_info$url, collapse = "\n"))


# Can only post 4 media items at a time
if (nrow(image_info) > 4) {
  image_info <- image_info[sample(1:nrow(image_info), 4), ]
  image_info <- image_info[order(row.names(image_info)), ]
}

## Uncomment for interactive checks
# paste0("https://valentine.rediscoverysoftware.com/",
#        ifelse(dir == "VALCOLL", "mDetail", "MADetailB"),
#        ".aspx?rID=",
#        item_info$id,
#        "&db=",
#        ifelse(dir == "VALCOLL", 'objects', 'biblio'),
#        "&dir=",
#        dir) |>
#   browseURL()
#
# paste0(
#   "https://valentine.rediscoverysoftware.com/FullImages",
#   image_info$image_path[sample(1:length(image_info$image_path), 1)]) |>
#   URLencode() |>
#   browseURL()

### Skeet it

cat("\nPosting to Bluesky...")

auth(
  user = 'thevalentinebot.bsky.social',
  password = Sys.getenv("BSKY_PAT")
)


post_skeet(
  text = paste0(
    "ID: ",
    item_info$id,
    ifelse(item_info$title != "", paste0("\n", item_info$title), ""),
    ifelse(item_info$date != "", paste0("\nDate: ", item_info$date), ""),
    ifelse(
      item_info$creator != "",
      paste0("\nArtist: ", item_info$creator),
      ""
    ),
    "\n",
    item_url,
    ifelse(
      dir == "VALCOLL" &
        grepl(
          "garments|hats|gloves|underwear|sandals|shoes|dresses",
          item_info$title,
          ignore.case = TRUE
        ),
      " #FashionHistory",
      ""
    )
  ),
  image = image_info$url,
  image_alt = rep(item_info$description, times = nrow(image_info))
)

cat("done.")



# # Missing images can return NULL (length == 0) or be empty (length == 1, but empty)
# if (length(image_check >= 1) & image_check$d != "") {
#   # Sometimes images can have dead links, returning 404
#   img_404s <- extract_tag(image_check |> read_html(), "fullimage") |>
#     sapply(FUN = function(x) {
#       paste0(
#         "https://valentine.rediscoverysoftware.com/FullImages",
#         x
#       ) |>
#         URLencode() |>
#         request() |>
#         req_error(is_error = \(resp) FALSE) |>
#         req_perform() |>
#         resp_is_error()
#     })
#   
#   if(!any(img_404s)) break
# }
# }

