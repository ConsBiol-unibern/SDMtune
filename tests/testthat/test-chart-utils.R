test_that(".create_chart created the correct files", {
  folder <- tempfile("SDMtune-chart-")
  create_local_chart(folder = folder,
                     script = "gridSearch.js",
                     update = FALSE)

  # The folder lib is created and contains the correct files
  expect_true(file.exists(file.path(folder, "lib", "chart_script.js")))
  expect_true(file.exists(file.path(folder, "lib", "Chart.min.js")))
  expect_true(file.exists(file.path(folder, "lib", "jquery.min.js")))
  expect_true(file.exists(file.path(folder, "lib", "style.css")))
  # The template file is created
  expect_true(file.exists(file.path(folder, "chart_template.html")))
})

test_that(".render_script function renders script, settings and data", {
  folder <- tempfile("SDMtune-chart-")
  create_local_chart(folder = folder,
                     script = "gridSearch.js",
                     update = FALSE)

  # Grid Search
  # Correct script
  expect_equal(readLines(file.path(folder, "lib", "chart_script.js"),
                         encoding = "UTF-8")[1],
               "// Grid Search Script")

  # Settings are rendered
  expect_equal(readLines(file.path(folder, "lib", "chart_script.js"),
                         encoding = "UTF-8")[3],
               "var settings = {\"update\":[false]};")
  # Data are rendered
  expect_equal(readLines(file.path(folder, "lib", "chart_script.js"),
                         encoding = "UTF-8")[4], "var data = [];")

  # Optimize Model
  folder <- tempfile("SDMtune-chart-")
  create_local_chart(folder = folder,
                     script = "optimizeModel.js",
                     update = FALSE)

  expect_equal(readLines(file.path(folder, "lib", "chart_script.js"),
                         encoding = "UTF-8")[1],
               "// Optimize Model Script")

  # Variable Selection
  folder <- tempfile("SDMtune-chart-")
  create_local_chart(folder = folder,
                     script = "varSelection.js",
                     update = FALSE)

  expect_equal(readLines(file.path(folder, "lib", "chart_script.js"),
                         encoding = "UTF-8")[1],
               "// Variable Selection Script")
})

test_that(".update_data function works corretly", {
  folder <- tempfile("SDMtune-chart-")
  create_local_chart(folder = folder,
                     script = "varSelection.js",
                     update = TRUE)

  # The data.json file is created
  expect_true(file.exists(file.path(folder, "data.json")))
  # The data.json file contains the data
  expect_equal(readLines(file.path(folder, "data.json"), encoding = "UTF-8"),
               "{\"update\":[false]}")
})
