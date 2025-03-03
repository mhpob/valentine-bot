library(httr2)

poll_catalog <- function(catalog){
  n_objects <- request('https://valentine.rediscoverysoftware.com/ProficioWcfServices/ProficioWcfService.svc/FindItXmlStringPageCount') |> 
    req_body_json(
      list(
        Directory = catalog,
        TableName = ifelse(catalog == "VALCOLL", "objects", "group"),
        Words = "*",
        OnlyIfImages = "false",
        AttachedMediaOption = " "
      )
    ) |> 
    req_perform() |> 
    resp_body_json() |> 
    _$d
}


n_objects <- read.csv("data/n_digitized.csv") |> 
  rbind(
    data.frame(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      collections = poll_catalog("VALCOLL"),
      archives = poll_catalog("VALARCH")
    )
  )

new_coll <- n_objects$collections[nrow(n_objects)] -
  n_objects$collections[nrow(n_objects) - 1]

new_arch <- n_objects$archives[nrow(n_objects)] -
  n_objects$archives[nrow(n_objects) - 1]


if(new_coll != 0 | new_arch != 0){
  write.csv(n_objects,
            "data/n_digitized.csv",
            row.names = FALSE)
  
  if (new_coll != 0 & new_arch != 0) {
    txt <- paste0(
      new_coll,
      " new items were added to the digitized Valentine collection and ",
      new_arch, " to the digitized archives on ",
      as.Date(n_objects$time[nrow(n_objects)]), "."
    )
  } else {
    txt <- paste0(
      ifelse(new_coll != 0, new_coll, new_arch),
      " new items were added to the digitized Valentine ",
      ifelse(new_coll != 0, "collections on ", "archives on "),
      as.Date(n_objects$time[nrow(n_objects)]), "."
    )
  }
  
  auth(
    user = 'thevalentinebot.bsky.social',
    password = Sys.getenv("BSKY_PAT")
  )
  
  post_skeet(
    text = txt
  )
}

