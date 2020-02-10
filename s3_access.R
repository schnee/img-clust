library(paws)
library(purrr)
library(magick)
library(here)


svc <- s3(
  config = list(
    credentials = list(
      profile = "default"
    ),
    endpoint = "https://s3.us-west-2.amazonaws.com/",
    region = "us-west-2"
  )
)

b <- svc$list_buckets()

my_bucket <- "app-secure-media"
my_prefix <- "LilyAI/old_patterns_data/patterns"

o <- svc$list_objects_v2(Bucket=my_bucket, Prefix = my_prefix)

images <- o$Contents %>% map("Key")

o2 <- svc$list_objects_v2(Bucket=my_bucket, Prefix = my_prefix, StartAfter = images[length(images)])
i2 <-images <- o2$Contents %>% map("Key")

copy_image <- function(key, svc, the_bucket, the_prefix){
  
  print(key)
  
  response <- svc$get_object(
    Bucket = the_bucket,
    Key = key
  )
  
  fn <- file(here::here(key), "wb")
  
  writeBin(response$Body, con = fn)
  
}

images %>% map(copy_image, svc, my_bucket, my_prefix)
