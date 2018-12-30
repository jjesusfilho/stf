# This function gets the urls according to the search parameters provided for
# stf_mono_metadata. So this function is going to be used inside the stf_metadata function.


stf_url <- function(x, y) {
  ## y will take the query parameter according to the options selected for the parameter database from stf_metadata function.


  ## Creates the url, x is the open_search parameter.
  url1 <-
    stringr::str_c(
      "http://www.stf.jus.br/portal/jurisprudencia/listarConsolidada.asp?txtPesquisaLivre=",
      x,
      "&base=baseMonocraticas"
    )

  ## Encodes the URL replacing specially spaces by "%" plus the hexadecimal representation
  url1 <- URLencode(url1)

  ## Gets the number of precedents
  numero_tinyurl <- httr::GET(url1) %>%
    httr::content() %>%
    xml2::xml_find_all("//*[@class='linkPagina']|//*[@class='linkPagina']/@href") %>%
    xml2::xml_text()

  ##
  paginas <- stringr::str_extract(numero_tinyurl[[1]], "\\d+") %>%
    as.numeric() %>%
    magrittr::divide_by(10) %>%
    ceiling()
  tinyURL <- numero_tinyurl[[2]]
  urls <-
    stringr::str_c("http://www.stf.jus.br/portal/jurisprudencia/",
                   tinyURL,
                   "&pagina=",
                   1:paginas)
}
# End of the function

# This functions encapsulates the previous function in the purrr::possibly function for the control
# of errors and the absense of results in the specified database. So this the actual function
# that's going to grab the data.

stf_urls <- purrr::possibly(stf_url, "ignore")
# End of the function

# STF parties description is very messy. This functions does its best to correct all the parties descriptions.
stf_parties_names <- function(z) {
  z %>%
    purrr::map_chr( ~ {
      .x %>%
        stringr::str_replace(stringr::regex("(adv|dpu).*\\s*", ignore_case = T),
                             "Advogado") %>%
        stringr::str_replace(stringr::regex("AG.*E.*\\s*", ignore_case = T),
                             "Agravante") %>%
        stringr::str_replace(stringr::regex("AG.*(o|a).*\\s*", ignore_case = T),
                             "Agravado") %>%
        stringr::str_replace(
          stringr::regex(".*(COATOR|coatro|autoridade).*\\s*", ignore_case = T),
          "Coator"
        ) %>%
        stringr::str_replace(stringr::regex("emb.*(o|a).*\\s*", ignore_case = T),
                             "Embargado") %>%
        stringr::str_replace(stringr::regex("emb.*e.*\\s*", ignore_case = T),
                             "Embargante") %>%
        stringr::str_replace(stringr::regex("EXT.*\\s*", ignore_case = T),
                             "Extraditando") %>%
        stringr::str_replace(stringr::regex("imp.*d.*\\s*", ignore_case = T),
                             "Impetrado") %>%
        stringr::str_replace(stringr::regex("imp.*t.*\\s*|IMPRE\\s*", ignore_case = T),
                             "Impetrante") %>%
        stringr::str_replace(stringr::regex("^p(a|c|e|t).*\\s*", ignore_case = T),
                             "Paciente") %>%
        stringr::str_replace(stringr::regex(".*rec.*e.*\\s*", ignore_case = T),
                             "Recorrente") %>%
        stringr::str_replace(stringr::regex(".*rec.*(o|a).*\\s*", ignore_case = T),
                             "Recorrido") %>%
        stringr::str_replace(stringr::regex(".*req.*e.*\\s*", ignore_case = T),
                             "Requerente") %>%
        stringr::str_replace(stringr::regex(".*req.*(o|a).*\\s*", ignore_case = T),
                             "Requerido") %>%
        stringr::str_replace(stringr::regex("^proc.*\\s*", ignore_case = T),
                             "Procurador") %>%
        stringr::str_replace(stringr::regex("^sus.*e.*\\s*", ignore_case = T),
                             "Suscitante") %>%
        stringr::str_replace(stringr::regex("^sus.*(o|a).*\\s*", ignore_case = T),
                             "Suscitado") %>%
        stringr::str_replace(stringr::regex(".*curiae.*\\s*", ignore_case = T),
                             "Amicus_curiae") %>%
        stringr::str_replace(stringr::regex("rc.*e.*\\s*", ignore_case = T),
                             "Reclamante") %>%
        stringr::str_replace(stringr::regex("rc.*(o|a).*\\s*", ignore_case = T),
                             "Reclamado") %>%
        stringr::str_replace(stringr::regex("intd(o|a).*\\s*", ignore_case = T),
                             "Interessado") %>%
        stringr::str_replace(stringr::regex("r\u00E9u*.*\\s*", ignore_case = T),
                             "R\u00E9u") %>%
        stringr::str_replace(stringr::regex("autor.*", ignore_case = T), "Autor") %>%
        stringr::str_replace(stringr::regex("litis.*pass.*", ignore_case = T),
                             "Listisconsorte_passivo") %>%
        stringr::str_replace(stringr::regex("litis.*at.*", ignore_case = T),
                             "Listisconsorte_ativo") %>%
        stringr::str_replace(stringr::regex("DND(oa).*", ignore_case = T), "Denunciado") %>%
        stringr::str_replace(stringr::regex("inves(oa).*", ignore_case = T),
                             "Investigado")

    })
}
# End of the function

# This is the main function. It collects all the metadata.

#' Returns metadada from Brazilian Supreme Court monocratic decisions
#'
#' @param open_search Words to be searched
#' @param parties_names Logical. If TRUE (default), it will attempt to fix
#'    parties prefixes.
#'
#' @keywords stf, precedents, metadata
#'
#' @return Dataframe with the metadata
#'
#' @export
get_stf_mono_metadata <- function(open_search, parties_names = TRUE) {
  ## calls the stf_urls function to grab all urls.
  urls <- stf_urls(x = open_search, y = "&base=baseMonocraticas")

  ## If nothing was found, it returns a error message informing that no  precedent was found.
  assertthat::assert_that(urls[1] != "ignore", msg = paste("No document was found in the database"))

  ## This whole chunck collects the content of every ten precedents loaded by the urls

  urls %>% purrr::map_dfr(purrr::possibly(
    ~ {
      ## Grabs the parsed page.
      principal <- .x %>%
        httr::GET() %>%
        httr::content()

      ## Unfortunately, stf doesn't have a tag for every element of the metadata.
      ## So in the same element div and class attribute "processosJurisprudenciaAcordaos"
      ## we get the lawsuit number, the appealed court, the procedural class and the name
      ## of the justice who reports the case, the occasional justice appointed to report that case,
      ## and date of the trial, and the Panel.

      recurso <- principal %>%
        xml2::xml_find_all("//div[@class='processosJurisprudenciaAcordaos']/p[1]/strong") %>%
        xml2::xml_text() %>%
        stringr::str_split("\n")

      processo <- recurso %>%
        purrr::map_chr( ~ stringr::str_extract(.x[[1]], ".*?(?=\\/)"))

      origem <- recurso %>%
        purrr::map_chr( ~ stringr::str_extract(.x[[1]], "(?<=\\/).*")) %>%
        stringr::str_trim()

      classe <- recurso %>%
        purrr::map_chr( ~ stringr::str_trim(.x[[2]]))



      relator <- recurso %>%
        purrr::map_chr( ~ {
          .x[[3]] %>%
            stringr::str_extract("(?<=Relator\\(a\\)\\:).*?(?=Relator|Julgamento)") %>%
            stringr::str_extract("(?<=Min\\.\\s).*")
        })






      data_julgamento <- recurso %>%
        purrr::map_chr( ~ {
          .x[[3]] %>%
            stringr::str_extract("\\d{2}\\/\\d{2}\\/\\d{4}")
        }) %>%
        lubridate::dmy()





      ##  Date of the decision's publication
      publicacao <- principal %>%
        xml2::xml_find_all("//p[strong='Publica\u00E7\u00E3o']/following-sibling::*[1]") %>%
        xml2::xml_text()

      data_publicacao <- publicacao %>%
        stringr::str_extract("(?<=PUBLIC\\s|DJ\\s)\\d{2}.\\d{2}.\\d{4}") %>%
        lubridate::dmy()

      ## In case of the Panel decision's report (acordãos), this will return whether it's electronic or not.
      ## This if information will be valueble to distinguish between reports that are text or images.

      ## This chunk will get all parties of the lawsuit and their respective descriptions.
      partes <- principal %>%
        xml2::xml_find_all("//p[strong[contains(.,'Parte')]]/following-sibling::pre[1]") %>%
        xml2::xml_text() %>%
        stringr::str_extract_all("\\w.*\\:.*(\r\n)*\\w*?") %>%

        purrr::modify_depth(1,  ~ {
          .x %>%
            setNames(stringr::str_extract(., ".*(?=\\:)"))
        })

      partes <- dplyr::bind_rows(!!!partes)

      partes <- partes %>%
        purrr::map_dfr( ~ stringr::str_replace(.x, ".*?(\\:\\s)", ""))

      partes <- partes %>%
        dplyr::select(-dplyr::matches(stringr::regex("rela|red.*", ignore_case =
                                                       TRUE)))

      if (parties_names) {
        names(partes) <- stf_parties_names(z = names(partes))
      }


      ## This chunk gets the decision.

      decisao <- principal %>%
        xml2::xml_find_all("//p[strong='Decis\u00E3o']/following-sibling::pre[1]") %>%
        xml2::xml_text()



      ## The code below gets the url of the Panel decision's report.


      ## The code below gets the timeline of the lawsuit
      url_andamento <- principal %>%
        xml2::xml_find_all(
          "//div[@class='abasAcompanhamento']/ul[@class='abas']/li/a[contains(@href,'verProcessoAndamento')]"
        ) %>%
        xml2::xml_attrs() %>%
        stringr::str_extract("numero.*") %>%
        stringr::str_c("http://www.stf.jus.br/portal/processo/verProcessoAndamento.asp?",
                       .)

      ## The code below creates a data frame with all the metadata grabbed above.

      s <-
        data.frame(
          processo,
          origem,
          classe,
          relator,
          data_julgamento,
          data_publicacao,
          decisao,
          url_andamento,
          partes,
          stringsAsFactors = FALSE
        )

    },
    data.frame(
      processo = NA_character_,
      origem = NA_character_,
      classe = NA_character_,
      relator = NA_character_,
      data_julgamento = NA_character_,
      data_publicacao = NA_character_,
      decisao = NA_character_,
      url_andamento = NA_character_,
      partes = NA_character_
    ),
    quiet = FALSE
  ))
}
