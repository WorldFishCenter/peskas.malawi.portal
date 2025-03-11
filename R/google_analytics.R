google_analytics <- function() {
  tagList(
    tags$head(
      tags$script(
        src = "https://www.googletagmanager.com/gtag/js?id=G-4QWM8JRWMH",
        async = TRUE
      ),
      tags$script(
        'window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag("js", new Date());

  gtag("config", "G-4QWM8JRWMH");'
      )
    )
  )
}
