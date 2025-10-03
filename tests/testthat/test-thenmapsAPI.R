test_that("get_data input checking works for dates", {
  api <- thenmapsAPI()
  expect_error(get_data(api, "se-4", "a"), "date format must be", ignore.case = TRUE)
  expect_error(get_data(api, "se-4", "200"), "date format must be", ignore.case = TRUE)
  expect_error(get_data(api, "se-4", "1000-"), "date format must be", ignore.case = TRUE)
  expect_error(get_data(api, "se-4", "1323-13-12w"), "date format must be", ignore.case = TRUE)
  expect_error(get_data(api, "se-4", "1234-12w"), "date format must be", ignore.case = TRUE)
  expect_error(get_data(api, "se-4", "a1234"), "date format must be", ignore.case = TRUE)

  expect_no_error(get_data(api, "se-4", "1234"))
  expect_no_error(get_data(api, "se-4", "1234-12"))
  expect_no_error(get_data(api, "se-4", "1234-12-12"))
})

test_that("input checking for data arguments works", {
  api <- thenmapsAPI()
  expect_error(get_data(api, "se-4", "*", data_format=NULL), "data_format must be", ignore.case = TRUE)
  expect_error(get_data(api, "se-4", "*", data_format="txt"), "data_format must be", ignore.case = TRUE)
  expect_no_error(get_data(api, "se-4", "*", data_format="csv"))
  expect_no_error(get_data(api, "se-4", "*", data_format="json"))

})

test_that("input checking for geo arguments works", {
  api <- thenmapsAPI()
  expect_error(get_geo(api, "se-4", "*", geo_type=NULL), "geo_type must be", ignore.case = TRUE)
  expect_error(get_geo(api, "se-4", "*", geo_type="json"), "geo_type must be", ignore.case = TRUE)
  expect_no_error(get_geo(api, "se-4", "*", geo_type="geojson"))
  expect_no_error(get_geo(api, "se-4", "*", geo_type="topojson"))


})

test_that("input checking for svg arguments works", {
  api <- thenmapsAPI()
  expect_error(get_svg(api, "se-4", "*", svg_height="5"), "svg_height must be", ignore.case = TRUE)
  expect_error(get_svg(api, "se-4", "*", svg_height=-1), "svg_height must be", ignore.case = TRUE)
  expect_no_error(get_svg(api, "se-4", "*", svg_height=NULL))
  expect_no_error(get_svg(api, "se-4", "*", svg_height=300))
  expect_no_error(get_svg(api, "se-4", "*", svg_height=0))


  expect_error(get_svg(api, "se-4", "*", svg_width="5"), "svg_width must be", ignore.case = TRUE)
  expect_error(get_svg(api, "se-4", "*", svg_width=-1), "svg_width must be", ignore.case = TRUE)
  expect_no_error(get_svg(api, "se-4", "*", svg_width=NULL))
  expect_no_error(get_svg(api, "se-4", "*", svg_width=300))
  expect_no_error(get_svg(api, "se-4", "*", svg_width=0))

})

test_that("get_data returns the json as data.frame", {
  api <- thenmapsAPI()
  expect_true(is.data.frame(get_data(api, "se-4", "*")))
  expect_true(is.data.frame(get_data(api, "se-4", "*", data_format = "csv")))
  expect_true(is.data.frame(get_data(api, "se-4", "*", data_format = "json")))
})

test_that("get_info returns a list", {
  api <- thenmapsAPI()
  expect_true(is.list(get_info(api, "se-4", "*")))

})

test_that("get_geo returns an sf object", {
  api <- thenmapsAPI()
  expect_true(inherits(get_geo(api, "se-4", "*"), "sf"))
  expect_true(is.data.frame(get_geo(api, "se-4", "*", geo_type = "geojson")))
  expect_true(is.data.frame(get_geo(api, "se-4", "*", geo_type = "topojson")))
})

test_that("get_svg returns an svg object", {
  api <- thenmapsAPI()
  expect_true(inherits(get_svg(api, "se-4", "*"), "svg_obj"))

})

