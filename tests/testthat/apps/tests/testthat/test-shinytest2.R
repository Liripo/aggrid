library(shinytest2)

test_that("{shinytest2} recording: test aggrid rows selected", {
  app <- AppDriver$new(name = "test aggrid rows selected", height = 957, width = 1619)
  app$set_inputs(agid_rows_selected = 1, allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$set_inputs(agid_rows_selected = c(1, 4), allow_no_input_binding_ = TRUE)
  app$expect_values()
})
