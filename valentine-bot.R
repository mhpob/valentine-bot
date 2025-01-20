library(httr2)
library(rvest)
library(atrrr)

extract_tag <- function(html, tag){
  html |> 
    html_elements(tag) |> 
    html_text()
}

dir <- sample(c("VALCOLL", "VALARCH"), 1)
table <- ifelse(dir == "VALCOLL", "objects", "group")

n_objects <- request('https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/FindItXmlStringPageCount') |> 
  req_body_json(
    list(
      Directory = dir,
      TableName = table,
      Words = "*",
      OnlyIfImages = "false",
      AttachedMediaOption = " "
    )
  ) |> 
  req_perform() |> 
  resp_body_json() |> 
  _$d


catalog <- request("https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/FindItXmlStringPage") |> 
  req_body_json(
    list(
      Words= "*",
      TableName= table,
      Directory= dir,
      FieldList= ifelse(dir == "VALCOLL", "cat_nbr", "group_nbr"),
      SortFields = "sortable15",
      OnlyIfImages = "false",
      IncludeImage = "false",
      RecordsPerPage = n_objects,
      PageNumber = "1",
      AttachedMediaOption = " "
    )
  ) |> 
  req_perform() |> 
  resp_body_json() |> 
  _$d |> 
  read_html() |> 
  html_elements(xpath = ifelse(dir == "VALCOLL", '//objects', '//group')) |> 
  html_text()


if(dir == "VALARCH"){
  catalog <- catalog[grepl("PHC", catalog)]
  indiv_box <- NULL
  
  while(length(indiv_box) == 0){
    collection <- sample(catalog, 1)
    box_list <- 'https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetArchivalChildren' |> 
      request() |> 
      req_body_json(
        list(
          TableName = "GROUP",
          ArchivalNumber = collection,
          Directory = "VALARCH"
        )
      ) |> 
      req_perform() |> 
      resp_body_json() |> 
      _$d |> 
      read_html() |> 
      html_elements(xpath = '//archivallist/archivalnumber') |> 
      html_text()
    
    if(any(grepl("X", box_list))) {
      item_id <- sample(box_list, 1)
    } else{
      i <- 1
      
      while(length(indiv_box) == 0 & i <= length(box_list)){
        indiv_box <- 'https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetArchivalChildren' |> 
          request() |> 
          req_body_json(
            list(
              TableName = "FILEUNIT",
              ArchivalNumber = sample(box_list, 1),
              Directory = "VALARCH"
            )
          ) |> 
          req_perform() |> 
          resp_body_json() |> 
          _$d |> 
          read_html() |> 
          html_elements(xpath = '//archivallist')
        
        i <- i+1
      }
    }
    
    ids <- indiv_box |>
      html_elements(xpath ='//archivalnumber') |>
      html_text()
    
    item_id <- sample(ids, 1)
  }
} else {
  item_id <- sample(catalog, 1)
}


# Print for possible debugging
print(item_id)


item <- "https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetRecordDetails" |> 
  request() |> 
  req_body_json(
    list(
      TableName = ifelse(dir == "VALCOLL", "objects", "biblio"),
      Directory = dir,
      FieldList =
        ifelse(dir == "VALCOLL", 
               "record_id,file_name,ExhibitId,db_exhibt_exhibt_dsc,cat_nbr,cat_nam,form,artist,categ_16,obj_mem,material,type,categ_4",
               "record_id,biblio_nbr,group_nbr,series_nbr,fileunit_nbr,edition,title,author,sortable14,origin,categ_12,categ_16,categ_1,categ_9[2],categ_3,user_2,user_4,sub_pers,sub_corp,sub_topic,sub_geo,categ_6,categ_7,categ_8,categ_13,subjects,categ_20,file_name"
        ),
      IncludeImage="true",
      RecordID=-1,
      readablePrimaryKey=item_id
    )
  ) |> 
  req_perform() |> 
  resp_body_json() |> 
  _$d |> 
  read_html()

if(dir == "VALCOLL"){
  item_info <- data.frame(
    id = extract_tag(item, 'cat_nbr'),
    title = extract_tag(item, "cat_nam"),
    file_name = extract_tag(item, "file_name"),
    date = extract_tag(item, "categ_16"),
    creator = extract_tag(item, "artist"),
    description = extract_tag(item, "obj_mem"),
    collection = extract_tag(item, "categ_4")
  )
} else {
  item_info <- data.frame(
    title = extract_tag(item, "title"),
    id = item_id,
    date = extract_tag(item, 'origin'),
    description = extract_tag(item, "categ_16"),
    file_name = extract_tag(item, "file_name"),
    creator = extract_tag(item, "author")
  )
}

for (i in 1:ncol(item_info)){
  item_info[, i] <- gsub("\\s?(--|__)", "; ", x = item_info[, i])
}

images <- 'https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetImagePaths'|> 
  request() |> 
  req_body_json(
    list(
      Directory = dir,
      ImageKey = item_info$file_name,
      FieldList="imagedescription"
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

print(image_info)

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

auth(
  user = 'thevalentinebot.bsky.social',
  password = Sys.getenv("BSKY_PAT")
)


post_skeet(
  text = paste0(
    "ID: ", item_info$id,
    ifelse(item_info$title != "", paste0("\n",item_info$title), ""),
    ifelse(item_info$date != "",
           paste0("\nDate: ", item_info$date),
           ""
    ),
    ifelse(item_info$creator != "",
      paste0("\nArtist: ", item_info$creator),
      ""
    ),
    "\n",
    paste0("https://valentine.rediscoverysoftware.com/",
           ifelse(dir == "VALCOLL", "mDetail", "MADetailB"),
           ".aspx?rID=",
           item_info$id,
           "&db=",
           ifelse(dir == "VALCOLL", 'objects', 'biblio'),
           "&dir=",
           dir) |>
      URLencode() |> 
      gsub(",", "%2C", x = _)
  ),
  image = image_info$url,
  image_alt = rep(item_info$description, times = nrow(image_info))
)
