file.remove('.httr-oauth')
library(httr)
library(httpov)
oauth_endpoints("github")
myapp <- oauth_app("github",
                   key = "629dcf359066ca9692b9",
                   secret = "452a79f7057529dd794a8cf995ecb8df6fee4d8c")

options(httr_oob_default=TRUE)
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

# OR:
# req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
# stop_for_status(req)
# content(req)


# None of the above worked...
#
# but this did:
#


req <- GET("https://api.github.com/users/jtleek/repos")
library(jsonlite)
json1 = content(req)
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"]
gitDF[gitDF$full_name == "datasharing", "created_at"]
# "2013-11-07T13:25:07Z"
