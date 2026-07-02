# server.R - Server logic

function(input, output, session) {
  # Call module servers
  organizationsServer("organizations")
  materialsServer("materials")

  # Handles the standalone Material Detail page (?view=material&...). Harmless
  # no-op when that page's UI/inputs aren't present, e.g. on the main app.
  materialDetailServer(input, output, session)
}
