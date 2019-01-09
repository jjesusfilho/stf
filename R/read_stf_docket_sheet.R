#' Reads Brazilian Supreme Court decision's docket sheet.
#'
#' @param path where to find the htmls downloaded by download_docket
#' @param plan defalts to sequential. See \code{future::plan} for all options.
#' @return tibble with follow-up details
#' @export
#'
#' @examples
#' \dontrun{
#' andamento <- read_stf_docket_sheet(path=".",plan="multiprocess")
#' }
#'
read_stf_docket_sheet <- function(path=".", plan = "sequential"){


  oplan <- plan
  on.exit(future::plan(oplan), add = TRUE)

  arquivos <- list.files(path,full.names = TRUE)

  incidentes <- stringr::str_extract(arquivos,"\\d+(?=.html)")

  future::plan(plan)

  furrr::future_map2_dfr(arquivos,incidentes,purrr::possibly(~{

    item <- xml2::read_html(.x,encoding="UTF-8") %>%
      xml2::xml_find_all("//div[@class='andamento-item']")

    data_andamento <- item %>%
      purrr::map(~xml2::xml_find_first(.x,".//div[contains(@class,'andamento-data')]") %>%
                   xml2::xml_text(trim=TRUE)) %>%
      unlist() %>%
      as.Date(format="%d/%m/%Y")

    titulo <- item %>%
      purrr::map(~xml2::xml_find_first(.x,".//h5[contains(@class,'andamento-nome')]") %>%
                   xml2::xml_text(trim=TRUE)) %>%
      unlist()

    descricao<-item %>%
      purrr::map(~xml2::xml_find_first(.x,".//div[contains(@class,'col-md-9 p-0')]") %>%
                   xml2::xml_text(trim=TRUE)) %>%
      unlist()

    orgao_julgador<-item %>%
      purrr::map(~xml2::xml_find_first(.x,".//div/span[contains(@class,'andamento-julgador')]") %>%
                   xml2::xml_text(trim=TRUE)) %>%
      unlist()


    doc <- item %>%
      purrr::map(~xml2::xml_find_all(.x,".//div[contains(@class,'andamento-docs')]/a") %>%
                   xml2::xml_text() %>%
                   stringr::str_squish(),.default = NA) %>%
      purrr::map_if(rlang::is_empty,~NA_character_)

    doc_url <- item %>%
      purrr::map(~xml2::xml_find_all(.x,".//div[contains(@class,'andamento-docs')]/a/@href") %>%
                   xml2::xml_text() %>%
                   xml2::url_absolute("http://portal.stf.jus.br/processos/")) %>%
      purrr::map_if(rlang::is_empty,~NA_character_)

     doc_id <- stringr::str_extract(doc_url,"\\d{3,}")

    tibble::tibble(incidente = .y,data_andamento,titulo,orgao_julgador,descricao,doc,doc_url,doc_id) %>%
      tidyr::unnest() %>%
      dplyr::distinct()

  }, NULL),.progress = TRUE)

}
