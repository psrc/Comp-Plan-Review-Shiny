# server.R - Server logic

function(input, output, session) {
  # Call module servers
  organizationsServer("organizations")
  materialsServer("materials")
}
