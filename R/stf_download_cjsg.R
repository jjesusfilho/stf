#' Baixa acórdãos e decisões monocráticas do STF.
#'
#' @param corpo Você pode usar o body(json da sua busca no STF)
#' @param livre Termos (use AND ou OR se necessário)
#' @param base Acórdãos ou decisões
#' @param dt_ini Data início dos julgamentos
#' @param dt_fim Data fim dos julgamentos
#' @param dtp_ini Data início da publicação
#' @param dtp_fim Data final da publicação
#' @param ministros Vetor com nome dos ministros. Não funciona ainda.
#' @param classes Vetor com as classes processuais. Não funciona ainda.
#' @param ufs Vetor com as siglas das unidades federativas. Não funciona ainda.
#' @param tamanho Número de decisões por página
#' @param dir Diretório
#'
#' @return arquivos em json.
#' @export
#'
stf_baixar_cjsg <- function(corpo = NULL,
                              livre = "",
                              base = c("acordaos", "decisoes"),
                              dt_ini = "",
                              dt_fim = "",
                              dtp_ini = "",
                              dtp_fim = "",
                              ministros = "",
                              classes = "",
                              ufs = "",
                              tamanho = 10,
                              dir = "."){


  datas <- mget(c("dt_ini","dt_fim","dtp_ini","dtp_fim")) |>
  lubridate::dmy() |>
  format("%d%m%Y") |>
  tidyr::replace_na("")


 tamanho <- as.integer(tamanho)

  if (tamanho > 200 | is.na(tamanho)){

    stop("Argumento tamanho tem de ser convers\u00EDvel para num\u00E9rico e, no m\u00E1ximo, 200.")

  }

  base <- base[1]

  classes <- classes |>
    jsonlite::toJSON()


  ufs <- ufs |>
    toupper() |>
    jsonlite::toJSON()


  ministros <- ministros |>
    toupper() |>
    stringi::stri_trans_general( "latin-ascii") |>
    jsonlite::toJSON()

  if (is.null(corpo)){

  body <- stf::modelo |>
    jqr::jq(glue::glue('.query.function_score.query.bool.filter[0].query_string.query = "{livre}"')) |>
    jqr::jq(glue::glue('.post_filter.bool.must[0].term.base = "{base}"')) |>
    jqr::jq(glue::glue('.size = {tamanho}'))


  datas <- mget(c("dt_ini","dt_fim","dtp_ini","dtp_fim")) |>
    lubridate::dmy() |>
    format("%d%m%Y")

  nd <- which(!is.na(datas))

  if (identical(nd, c(1L,2L))){

    body <- body |>
      jqr::jq(glue::glue('.query.function_score.query.bool.filter[1].range.julgamento_data.format = "ddMMyyyy"')) |>
      jqr::jq(glue::glue('.query.function_score.query.bool.filter[1].range.julgamento_data.gte = "{datas[1]}"')) |>
      jqr::jq(glue::glue('.query.function_score.query.bool.filter[1].range.julgamento_data.lte = "{datas[2]}"'))

    dt_arquivo <- datas |>
      na.omit() |>
      stringr::str_replace_all("\\D","_") |>
      paste(collapse = "_")

    } else if (identical(nd, c(3L,4L))){

    body <- body |>
      jqr::jq(glue::glue('.query.function_score.query.bool.filter[1].range.julgamento_data.format = "ddMMyyyy"')) |>
      jqr::jq(glue::glue('.query.function_score.query.bool.filter[1].range.publicacao_data.gte = "{datas[3]}"')) |>
      jqr::jq(glue::glue('.query.function_score.query.bool.filter[1].range.publicacao_data.lte = "{datas[4]}"'))

    dt_arquivo <- datas |>
           na.omit() |>
           stringr::str_replace_all("\\D","_") |>
           paste(collapse = "_")

     } else if (all(is.na(nd))){

    body <- body

    dt_arquivo <- ""

  } else {

    stop("Voc\u00EA deve informar as datas de in\u00EDcio e fim do julgamento ou da publica\u00E7\u00E3o, n\u00E3o as duas.")
  }


  } else {

    if (file.exists(corpo)){

      body <- jqr::jq(file(corpo))

      body <- body |>
      jqr::jq(glue::glue('.size = {tamanho}'))

      dt_arquivo <- ""

    } else {

      stop("Voc\u00EA deve informar um caminho v\u00E1lido para o corpo(json)")
    }

  }

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





  total <- conteudo |>
    jqr::jq(".result.hits.total.value")


  sequencia <- seq(0, total,by = tamanho )


  purrr::walk(sequencia, purrr::possibly(~{


    body <- body |>
      jqr::jq(glue::glue(".from = {.x}"))

    arquivo <- file.path(dir, paste0("stf_",dt_arquivo,"_search_page_",.x, ".json") ) |>
               stringr::str_replace("__","_")

    httr::POST(url,
               encode = "json",
               h1,
               body = body,
               httr::write_disk(arquivo, overwrite = T))

  },NULL))


}

