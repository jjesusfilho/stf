#' Authenticate to use STF API
#'
#' @param login tax number (CPF) with or without punctiation
#' @param password Character password
#'
#' @return TRUE if authenticated
#' @export
#'
stf_authenticate <- function(login = NULL, password = NULL){

  if (is.null(login) || is.null(password)) {
    login <- Sys.getenv("STFLOGIN")
    password <- Sys.getenv("STFPASSWORD")
    if (login == "" || password == "") {
      login <- as.character(getPass::getPass(msg = "Enter your login: "))
      password <- as.character(getPass::getPass(msg = "Enter your password: "))
    }
  }


  login <- stringr::str_remove_all(login, "\\D")

  url1 <- "https://sistemas.stf.jus.br/cas/login?service=https%3A%2F%2Fsistemas.stf.jus.br%2Fpeticionamento%2Fj_spring_cas_security_check"

  resposta <- httr::GET(url1)
  conteudo <- httr::content(resposta)


  lt <- conteudo |>
    xml2::xml_find_first("//div//input[@name='lt']") |>
    xml2::xml_attr("value")

  execution <- conteudo |>
    xml2::xml_find_first("//div//input[@name='execution']") |>
    xml2::xml_attr("value")

  url2 <- "https://sistemas.stf.jus.br/cas/login?service=https://sistemas.stf.jus.br/peticionamento/j_spring_cas_security_check"

  body <- list(
    username = login,
    password = password,
    lt = lt,
    execution = execution,
    `_eventId` = "submit",
    submit = "Entrar"
  )

  resposta <- httr::POST(url2, body = body, encode = "form",
                         httr::set_cookies(unlist(resposta$cookies)),
                         httr::user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/100.0.4896.75 Safari/537.36'),
                         httr::add_headers(Referer = url1),
                         httr::config(ssl_verifypeer = FALSE)
  )



}
