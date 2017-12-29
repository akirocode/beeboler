
# ApiFake -----------------------------------------------------------------

ApiFake <- function(request, response) {
	this <- list(request      = request
							 , requestObj = fromJSON(request)
							 , response   = response
							 , responseObj= fromJSON(response)) # bad: func from package
	class(this) <- "ApiFake"
	this
}

# company_fakes -----------------------------------------------------------

## Ref: https://beebole.com/timesheet-api/#company

# companyFake1 <- ApiFake(request = '{
#   "service": "company.create",
#   "company": {
#     "name": "myCompany",
#     "corporate" : true
#   }
# }'
# 												, response = '{
#   "status": "ok",
#   "id": 233
# }')

companyFake2 <- ApiFake(request = '{
  "service": "company.get",
  "id": 233
}'
												, response = '{
  "status" : "ok",
  "company" : {
    "id" : 233,
    "name" : "myCompany",
    "projects": {
      "count": 3
    },
    "active" : true
  }
}')

# people_fakes ------------------------------------------------------------

personGetFake <- ApiFake(request = '
{
  "service": "person.get",
  "id": 455
}'
												 , response = '
{
  "status" : "ok",
  "person" : {
    "id" : 455,
    "name" : "Rosa Parks",
    "company" : {"id" : 233},
    "active" : true,
    "email": "rosa.parks@example.com",
    "userGroup": "employee",
    "leaders" : {"count" : 1}
  }
}')

personListFake <- ApiFake(request = '
{
	"service": "person.list",
	"company" : {"id" : 233}
}'
	, response = '
{
	"status": "ok",
	"people": [
		{
			"id": 455,
			"name" : "RosaParks",
			"company": {"id" : 233},
			"active" : true,
			"email": "rosa.parks@example.com",
			"userGroup": "employee",
			"leaders" : {"count" : 1}
		},
    {
			"id": 456,
			"name" : "JohnDoe",
			"company": {"id" : 233},
			"active" : true,
			"email": "john.doe@example.com",
			"userGroup": "employee",
			"leaders" : {"count" : 1}
		}
		]
}')

# group_fakes -------------------------------------------------------------

groupGetFake <- ApiFake(request = '{
  "service": "group.get",
  "id": 1005
}'
													, response = '{
  "status" : "ok",
  "group" : {
    "id" : 1005,
    "name" : "Texas",
    "parent" : {"id" : 4787}
  }
}
')

# apiFakes ----------------------------------------------------------------

apiFakes <- list(companyFake2
								 , personGetFake
								 , personListFake)

# conn --------------------------------------------------------------------

conn <- beebole(user = "JohnDoe", pass = "JohnDoe")

# fake -----------------------------------------------------------

probe <- new.env()
probe$error <- FALSE

beeboleAppsRequestFake <- function(uri, config, body) {
	toMinJson <- function(json) {
		gsub(pattern = "\n*|\ *|\t*", replacement = "", x = json)
	}
	if (!is.character(body)) {
		body <- toJSON(body, auto_unbox = TRUE)
	}
	body <- toMinJson(body)
	res <- NULL
	for (apiFake in apiFakes) {
		request  <- toMinJson(apiFake$request)
		response <- toMinJson(apiFake$response)
		if (body == request) {
			res <- list()
			res$content <- response
		}
	}
	res
	if (is.null(res$content)) {
		cat(body)
		probe$body <- body  # mock
		probe$error <- TRUE
	}
	content <- res$content
	content
}

# company_tests -----------------------------------------------------------

test_that("get_company test", {
	library(jsonlite)

	with_mock(
		`beeboler::beeboleAppsRequest` = beeboleAppsRequestFake
		, company <- company_get(conn, 233))

	expect_equal(company$company$id, 233)
})

# person_tests ------------------------------------------------------------

test_that("person_get test", {

	with_mock(
		`beeboler::beeboleAppsRequest` = beeboleAppsRequestFake
		, people <- person_get(conn, 233))
	expected <- as.list(personListFake$responseObj$people$id)
	names(expected) <- personListFake$responseObj$people$name

	expect_false(probe$error)

	expect_equal(people, expected)
})

# group_tests -------------------------------------------------------------


