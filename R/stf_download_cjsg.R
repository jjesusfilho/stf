#' Download STF decisions by free search
#'
#' @param search Terms (use AND or OR if needed)
#' @param base Acordaos or decisoes
#' @param size Number of decisions per page
#' @param dir Directory
#'
#' @return json files.
#' @export
#'
stf_download_cjsg <- function(search = "",
                              base = c("acordaos", "decisoes"),
                              size = 10,
                              dir = "."){



size <- as.integer(size)

if (size > 200 | is.na(size)){

  stop("Argument size cannot be bigger then 200 \nand must be coercible to numeric")

}

base <- base[1]

### This function uses SQLite json engine to set values to the json body,
### as there is no R engine to manipulate json as is.

q <- glue::glue_sql(" with cte1 as (
                    select json_set({modelo},'$.query.function_score.query.bool.filter[0].query_string.query', {search}) as value
                     ),
                     cte2 as (
                      select json_set(value,'$.post_filter.bool.must[0].term.base', {base}) as value from cte1
                     ),
                     cte3 as (
                    select json_set(value,'$.size', {size}) as value from cte2
                     )
                     select value from cte3
                    ", .con = conn)

body <- DBI::dbGetQuery(conn,q) |>
        dplyr::pull("value")


  h1 <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:107.0) Gecko/20100101 Firefox/107.0",
                    `Accept` = "application/json, text/plain, */*",
                    `Accept-Language` = "en-US,en;q=0.5",
                    `Accept-Encoding` = "gzip, deflate, br",
                    `Content-Type` = "application/json",
                    `Origin` = "https://jurisprudencia.stf.jus.br",
                    `Connection` = "keep-alive",
                    `Sec-Fetch-Dest` = "empty",
                    `Sec-Fetch-Mode` = "cors",
                    `Sec-Fetch-Site` = "same-origin")

url <- "https://jurisprudencia.stf.jus.br/api/search/search"


 conteudo <- httr::POST(url,
                   encode = "json",
                   h1,
                   body = body) |>
          httr::content("text", encoding = "UTF-8")


  q <- glue::glue_sql("select json_extract({conteudo}, '$.result.hits.total.value') as value", .con = conn)



  total <- DBI::dbGetQuery(conn,q) |>
             dplyr::pull("value")

  sequencia <- seq(0, total,by = size )


  purrr::walk(sequencia, purrr::possibly(~{

  q <- glue::glue_sql("select json_set({body}, '$.from', {.x}) as value", .con = conn)

  body <- DBI::dbGetQuery(conn,q) |>
          dplyr::pull("value")

  arquivo <- file.path(dir, paste0("stf_search_page_",.x, ".json") )

  httr::POST(url,
             encode = "json",
             h1,
             body = body,
             httr::write_disk(arquivo, overwrite = T))

  },NULL))


}

