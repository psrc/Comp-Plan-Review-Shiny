# modules/material_detail.R - standalone Material Detail page
# Opened in its own browser tab via a link from the Materials table
# (see ?view=material&material_id=...&jurisdiction=... routing in ui.R)
#
# Built entirely at request time (inside ui.R's function(request)) rather than
# via a server-side render, since the query string is only reliably available
# on the initial HTTP request, not on the follow-up websocket handshake.

materialDetailUI <- function(material_id, jurisdiction_name) {
  material <- if (!is.null(material_id)) get_material_by_id(material_id) else NULL

  content <- if (is.null(material) || nrow(material) == 0 || "Error" %in% names(material)) {
    tags$p("Material not found.")
  } else {
    tagList(
      h2(material$MaterialTitle),
      h4(if (!is.null(jurisdiction_name)) jurisdiction_name else ""),
      p(paste0("Material Received: ", format(as.Date(material$MaterialDateReceived), "%m/%d/%Y")))
    )
  }

  page_fluid(
    tags$head(tags$title("Material Detail")),
    content
  )
}
