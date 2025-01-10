bsky_pat <- commandArgs(trailingOnly = TRUE)

library(httr2)
library(rvest)
library(atrrr)

extract_tag <- function(html, tag){
  html |> 
    html_elements(tag) |> 
    html_text()
}

n_objects <- request('https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/FindItXmlStringPageCount') |> 
  req_body_json(
    list(Words = "*",
         TableName = "objects",
         Directory = "VALCOLL",
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
      TableName= "objects",
      Directory= "VALCOLL",
      FieldList= "cat_nbr",
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
  html_elements(xpath = '//objects') |> 
  html_text()




item <- "https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetRecordDetails" |> 
  request() |> 
  req_body_json(
    list(
      TableName= "objects",
      Directory="VALCOLL",
      FieldList="record_id,file_name,ExhibitId,db_exhibt_exhibt_dsc,cat_nbr,cat_nam,form,artist,categ_16,obj_mem,material,type,categ_4",
      IncludeImage="true",
      RecordID=-1,
      readablePrimaryKey=sample(catalog, 1)
    )
  ) |> 
  req_perform() |> 
  resp_body_json() |> 
  _$d |> 
  read_html()

info <- data.frame(
  catalog_number = extract_tag(item, 'cat_nbr'),
  file_name = extract_tag(item, "file_name"),
  category = extract_tag(item, "cat_nam"),
  date = extract_tag(item, "categ_16"),
  artist = extract_tag(item, "artist"),
  description = extract_tag(item, "obj_mem"),
  collection = extract_tag(item, "categ_4")
)

images <- 'https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/GetImagePaths'|> 
  request() |> 
  req_body_json(
    list(
      Directory = "VACOLL",
      ImageKey = info$file_name,
      FieldList="imagedescription"
    )
  ) |> 
  req_perform() |> 
  resp_body_json() |> 
  _$d |> 
  read_html()

image_info <- data.frame(
  image_path = extract_tag(images, "fullimage")
)



auth(
  user = 'thevalentinebot.bsky.social',
  password = bsky_pat
)
post(
  text = paste0(
    "Cat #: ",
    info$catalog_number,
    "\nDate: ",
    info$date,
    "\nArtist: ",
    info$artist,
    "\nArea: ",
    info$collection,
    "\n",
    paste0("https://valentine.rediscoverysoftware.com/mDetail.aspx?rID=",
    info$catalog_number,
    "&db=objects&dir=VALCOLL")
  ),
  image = paste0(
    "https://valentine.rediscoverysoftware.com/FullImages",
    image_info$image_path[sample(1:length(image_info$image_path))]),
  image_alt = info$description
)
