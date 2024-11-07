# 7ª Pagina (About) ----

# Função para a página About
about_page <- function() {
  tagList(
    fluidRow(
      box(
        title = "Sobre o aplicativo",
        width = 12,
        status = "danger",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        htmltools::includeMarkdown("www/about/about.md")
      )
    ),
    fluidRow(
      box(
        title = "Outros Projetos",
        width = 12,
        status = "danger",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        htmltools::includeMarkdown("www/about/other-projects.md")
      )
    )
  )
}
