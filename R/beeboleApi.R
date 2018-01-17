
# beebole -----------------------------------------------------------------

beebole <- function(user, pass, url = "beebole-apps.com/api/v2") {
	b <- list(url = url,
						credential = list(user = user, pass = pass) )
	class(b) <- c("beebole", class(b))
	b
}

# get-functions -----------------------------------------------------------

companies_get <- function(conn) {
	body <- list( service = "company.list" )
	out <- api_call(conn, body) # this returns a (row,column) df
	companies <- as.list(out$companies$id)
	names(companies) <- out$companies$name
	companies
}

company_absence_list_get <- function(conn, company_id) {
	body <- list( "service" = "absence.list",
								"company" = list("id" = company_id)	)
	out <- api_call(conn, body)
	return(out)
}

company_get <- function(conn, company_id) {
	body <- paste0(	"{ \"service\": \"company.get\", \"id\":", company_id, " }")
	out <- api_call(conn, body)
	return(out)
}

# api_function
person_list <- function(conn, company_id) {
	body <- list(
		"service" = "person.list",
		"company" = list("id" = company_id )
	)
	out <- api_call(conn, body)
	out
}

# RStyle function
person_get <- function(conn, company_id) {
	out <- person_list(conn, company_id)
	subject <- as.list(out$people$id)
	names(subject) <- out$people$name
	subject
}

# api_function
person_groups <- function(conn, person_id) {
	body <- list(
		"service" = "person.groups",
		"id" = person_id
	)
	out <- api_call(conn, body)
	out
}
# RStyle_function
person_group_get <- function(conn, person_id) {
	person_groups(conn, person_id)
	subject <- as.list(out$groups$id)
	names(subject) <- out$groups$name
	subject
}

#' @import jsonlite
time_entry_get <- function(conn, person_id, from, to) {
	body <-
		list("service" = "time_entry.list",
				 "person" = list("id" = person_id ),
				 "from"   = as.character(from),
				 "to"     = as.character(to) )
	out <- api_call(conn, body)
	out <- out[['timeEntries']]
	jsonlite::flatten(out)
}

# api-generic -------------------------------------------------------------

# #not useful
# rapi_call <- function(credential, body) {
# 	json_body <- from_json(body)
# 	out <- api_call(credential, json_body)
# 	out
# }

beeboleAppsRequest <- function(uri, config, body) {
	if (is.list(body)) body <- toJSON(body, auto_unbox = TRUE)
	res <- POST(url = uri
							, config = config
							, body = body
							# , encode = "json"
							##, verbose()
	)
	content <- rawToChar(res$content)
	content
}
contentToList <- function(content) {
	out <- fromJSON(content)
	if (out$status!="ok") {
		message(out$message)
		stop("error in api call")
	}
	out['status'] <- NULL
	out
}

api_call <- function(conn, body) {
	uri   <- paste0("https://"
									, conn$credential$user
									, ":"
									, conn$credential$pass
									, "@beebole-apps.com/api/v2")
	content <- beeboleAppsRequest(uri
														, config = authenticate(conn$credential$user, conn$credential$pass, type = "basic")
														, body)
	out <- contentToList(content)
	out
}

