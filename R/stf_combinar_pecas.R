#' Combina pecas baixados com stf_baixar_pecas
#'
#' @param arquivos Vetor de arquivos
#' @param dir_origem Diretório de origem, se não
#'     informar arquivos
#' @param dir_destino Diretório destino
#' @param nivel Juntar processo inteiro ou por documento?
#' @param forcar Padrão para TRUE. Documentos inválidos são excluídos.
#'
#' @return único pdf
#' @export
#'
stf_combinar_pecas <- function(arquivos = NULL,
                                           dir_origem = ".",
                                           dir_destino = NULL,
                                           nivel = c("incidente", "doc"),
                                           forcar = TRUE){
  
  if (is.null(dir_destino) || !dir.exists(dir_destino)){
    
    stop("Voc\u00ea deve informar um diret\u00f3rio existente")
    
  }
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(dir_origem, full.names = TRUE, pattern = "pdf$")
    
  }
  
  nivel = nivel[1]
  
  
  lista <- tibble::tibble(arquivos) |>
    dplyr::mutate(incidente = stringr::str_extract(arquivos,"(?<=_incidente_)\\d+") |> as.integer(),
                  proc_id = stringr::str_extract(arquivos,"(?<=_prcID_)\\d+") |> as.integer(),
                  id_doc = stringr::str_extract(arquivos,'(?<=docID_)\\d+') |> as.integer(),
                  sequencia = stringr::str_extract(arquivos, '(?<=sequencia_)\\d+') |> as.integer()) |> # nolint
    dplyr::arrange(incidente,sequencia)
  
  
  if (forcar){
    
    lista <- lista |>
      dplyr::filter(is_pdf(arquivos))
  }
  
  if (nivel == "incidente"){
    
    lista <- dplyr::group_split(lista, incidente)
    
    
    purrr::walk(lista, purrr::possibly(~{
      
      incidente <- unique(.x$incidente)
      
      qpdf::pdf_combine(.x$arquivos, file.path(dir_destino,paste0(incidente,".pdf")))
      
    },NULL))
    
    
  } else {
    
    lista <- dplyr::group_split(lista, incidente, id_doc)
    
    purrr::walk(lista, purrr::possibly(~{
      incidente<- unique(.x$incidente)
      id_doc <- unique(.x$id_doc)
      suppressWarnings(
        qpdf::pdf_combine(.x$arquivos, file.path(dir_destino,
                                                 paste0(incidente,"_id_doc_",id_doc, ".pdf")))
      )
    }, NULL))
  }
  
  
  
  
}


#' Verifica se um arquivo é pdf (vetorizado)
#'
#' @param x Vetor de arquivos
#'
#' @return Valor lógico
#'
is_pdf <- function(x) {
  purrr::map_lgl(x, is_pdf1)
}

#' Verifica se um arquivo é pdf (não vetorizado)
#'
#' @param arquivo Suposto pdf a ser verificado
#'
#' @return Valor lógico
#'
is_pdf1 <- function(arquivo) {
  if(file.exists(arquivo)) {
    res <- suppressMessages(try(pdftools::pdf_info(arquivo),
                                silent = TRUE))
    if(!methods::is(res,'try-error')) return(TRUE)
    warning(paste(arquivo, "Parece n\u00E3o se tratar de um pdf."))
    return(FALSE)
  }
  warning(paste(arquivo, "n\u00E3o existe."))
  return(FALSE)
}