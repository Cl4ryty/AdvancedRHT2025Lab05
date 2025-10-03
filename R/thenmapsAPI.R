library(httr2)
library(svgtools)
library(jsonlite)
library(sf)

#' Create thenmapsAPI instance
#'
#' @param version The version of the API to use, either "v1" or "v2"
#'
#' @returns A list of class thenmapAPI with the attributes base_url, version, datasets, modules, valid_projections, data_formats, and geo_type
#' @export
#'
#' @examples
#' api <- thenmapsAPI(version="v2")
#'
#' @references \url{https://www.thenmap.net/}
thenmapsAPI <- function(version="v2"){
  if(is.null(version) || !version %in% c("v1", "v2")){
    stop("version must be either 'v1' or 'v2'")
  }

  datasets <- c(
    "fi-8",
    "ch-8",
    "no-7",
    "no-4",
    "dk-7",
    "se-7",
    "se-4",
    "us-4",
    "gl-7",
    "world-2")

  modules <- c(
    "data",
    "info",
    "geo",
    "svg"
  )

  laguages <- c("")

  valid_projections <- c(
    "tm35fin",
    "swissgrid",
    "euref89no",
    "euref89dk",
    "sweref99tm",
    "albersUsa",
    "gr96",
    "robinsonmollweide"
  )

  data_formats <- c(
    "json",
    "csv"
  )

  geo_type <- c(
    "geojson",
    "topojson"
  )

  api <- list(
    base_url="http://api.thenmap.net",
    version=version,
    datasets=datasets,
    modules=modules,
    valid_projections=valid_projections,
    data_formats=data_formats,
    geo_type=geo_type
  )
  class(api) <- "thenmapsAPI"
  return(api)
}

#' Get data from the thenmap API
#'
#' @param api A thenmapAPI object
#' @param dataset_name The name of the dataset for which to retrieve the data
#' @param date A character vector with the for which to retrieve the data, must be one of the formats 'YYYY', 'YYYY-MM', 'YYYY-MM-DD'
#' @param language Optional: The two-letter language code of the language in which to retrieve the data
#' @param data_format The format in which to request the data from the API, one of 'json', 'csv'. Default: json
#' @param return_object Whether to return on object suitable for further processing, in this case a data.frame. If FALSE, the response object returned by httr2 is returned. Default: TRUE
#'
#' @returns A data.frame if return_object==TRUE, a response object if return_object==FALSE
#' @export
#'
#' @examples
#' get_data(api=api, dataset_name="se-7", date="2015")
#'
#' @references \url{http://api.thenmap.net/doc/v2/#data}
get_data <- function(api, dataset_name, date, language=NULL, data_format="json", return_object=TRUE){

  if(is.null(data_format) || !(data_format %in% api$data_formats)){
    stop("data_format must be one of ", api$datasets)
  }

  req_parameters = NULL
  if(!is.null(language)){
    req_parameters <- list(language=language)
  }

  # add data_format to request parameters
  req_parameters <- c(req_parameters, list(data_format=data_format))


  resp <- access_api(api=api,
             dataset_name=dataset_name,
             module="data",
             date=date,
             req_parameters=req_parameters)

  # handle response by parsing to df depending on data_format
  if(req_parameters$data_format=="csv"){
    tmpfile <- tempfile()
    writeLines(httr2::resp_body_string(resp, encoding = "UTF-8"), con=tmpfile)
    return(read.csv(tmpfile))
  }

  if(!return_object){
    return(resp)
  }

  return(jsonlite::fromJSON(httr2::resp_body_string(resp, encoding = "UTF-8")))
}

#' Get metadata for a dataset from the thenmap API
#'
#' @param api A thenmapAPI object
#' @param dataset_name The name of the dataset for which to retrieve the data
#' @param date A character vector with the for which to retrieve the data, must be one of the formats 'YYYY', 'YYYY-MM', 'YYYY-MM-DD'
#' @param language Optional: The two-letter language code of the language in which to retrieve the data

#' @param return_object Whether to return on object suitable for further processing, in this case a list. If FALSE, the response object returned by httr2 is returned. Default: TRUE
#'
#' @returns A list if return_object==TRUE, a response object if return_object==FALSE
#' @export
#'
#' @examples
#' get_info(api=api, dataset_name="se-7", date="2015")
#'
#' @references \url{http://api.thenmap.net/doc/v2/#info}
get_info <- function(api, dataset_name, date, language=NULL, return_object=TRUE){
  req_parameters = NULL
  if(!is.null(language)){
    req_parameters <- list(language=language)
  }

  resp <- access_api(api=api,
                     dataset_name=dataset_name,
                     module="info",
                     date=date,
                     req_parameters=req_parameters)
  if(!return_object){
    return(resp)
  }
  # parse response json
  return(jsonlite::fromJSON(httr2::resp_body_string(resp, encoding = "UTF-8")))
}

#' Get geo data from the thenmap API
#'
#' @param api A thenmapAPI object
#' @param dataset_name The name of the dataset for which to retrieve the data
#' @param date A character vector with the for which to retrieve the data, must be one of the formats 'YYYY', 'YYYY-MM', 'YYYY-MM-DD'
#' @param language Optional: The two-letter language code of the language in which to retrieve the data
#' @param geo_type The format in which to request the geodata from the API, one of 'geojson', 'topojson'. Default: geojson
#' @param geo_crs The coordinate reference system to use for the geodata. Default: wgs84
#' @param geo_props A character vector containing all comma-separated fields from the data module to be included in the shape properties
#' @param return_object Whether to return on object suitable for further processing, in this case an sf object. If FALSE, the response object returned by httr2 is returned. Default: TRUE
#'
#' @returns An sf object if return_object==TRUE, a response object if return_object==FALSE
#' @export
#'
#' @examples
#' get_geo(api=api, dataset_name="se-7", date="2015")
#'
#' @references \url{http://api.thenmap.net/doc/v2/#geo}
get_geo <- function(api, dataset_name, date, language=NULL, geo_type="geojson", geo_crs="wgs84", geo_props=NULL, return_object=TRUE){
  if(is.null(geo_type) || !(geo_type %in% api$geo_type)){
    stop("geo_type must be one of ", api$geo_type)
  }

  req_parameters = NULL
  if(!is.null(language)){
    req_parameters <- list(language=language)
  }

  # add geo specific parameters
  req_parameters <- c(req_parameters, list(geo_type=geo_type, geo_crs=geo_crs))
  if(!is.null(geo_props)){
    req_parameters <- c(req_parameters, list(geo_props=geo_props))
  }

  resp <- access_api(api=api,
                     dataset_name=dataset_name,
                     module="geo",
                     date=date,
                     req_parameters=req_parameters)

  if(!return_object){
    return(resp)
  }

  # parse geo-/toppjson
  sf::read_sf(httr2::resp_body_string(resp, encoding = "UTF-8"))


}

#' Get svg representations from the thenmap API
#'
#' @param api A thenmapAPI object
#' @param dataset_name The name of the dataset for which to retrieve the data
#' @param date A character vector with the for which to retrieve the data, must be one of the formats 'YYYY', 'YYYY-MM', 'YYYY-MM-DD'
#' @param language Optional: The two-letter language code of the language in which to retrieve the data
#' @param svg_proj The projection to use for the dataset when represented in the SVG. Defaults to the first recommendation if not provided
#' @param svg_width The with of the SVG in pixels
#' @param svg_height The height of the SVG in pixels
#' @param svg_props A character vector containing all comma-separated fields from the data module to be included as data attributes
#' @param return_object Whether to return on object suitable for further processing, in this case an svg_obj. If FALSE, the response object returned by httr2 is returned. Default: TRUE
#'
#' @returns An svg_obj if return_object==TRUE, a response object if return_object==FALSE
#' @export
#'
#' @examples
#' get_svg(api=api, dataset_name="se-7", date="2015")
#'
#' @references \url{http://api.thenmap.net/doc/v2/#svg}
get_svg <- function(api, dataset_name, date, language=NULL, svg_proj=NULL, svg_width=NULL, svg_height=NULL, svg_props=NULL, return_object=TRUE){
    # check arguments - ignore if null, check for validity otherwise
    if(!is.null(svg_height) && (!is.numeric(svg_height) || svg_height<0)){
      stop("svg_height must be a nonnegative numeric")
    }
    if(!is.null(svg_width) && (!is.numeric(svg_width) || svg_width<0)){
      stop("svg_width must be a nonnegative numeric")
    }

    req_parameters = NULL
    if(!is.null(language)){
      req_parameters <- list(language=language)
    }

    # add svg specific parameters
    req_parameters <- NULL
    if(!is.null(svg_proj)){
      req_parameters <- c(req_parameters, list(svg_proj=svg_proj))
    }
    if(!is.null(svg_width)){
      req_parameters <- c(req_parameters, list(svg_width=svg_width))
    }
    if(!is.null(svg_height)){
      req_parameters <- c(req_parameters, list(svg_height=svg_height))
    }
    if(!is.null(svg_props)){
      req_parameters <- c(req_parameters, list(svg_props=svg_props))
    }

    resp <- access_api(api=api,
                       dataset_name=dataset_name,
                       module="svg",
                       date=date,
                       req_parameters=req_parameters)
    if(!return_object){
      return(resp)
    }

    # parse with svgtools and return the svg object
    svgtools::read_svg(httr2::resp_body_string(resp, encoding = "UTF-8"))
}

#' Query the thenmap API
#' @param api A thenmapAPI object
#' @param dataset_name The name of the dataset for which to retrieve the data
#' @param module The API module to be queried, one of 'data', 'info', 'geo', 'svg'
#' @param date A character vector with the for which to retrieve the data, must be one of the formats 'YYYY', 'YYYY-MM', 'YYYY-MM-DD'

#' @param req_parameters List of request parameters
#'
#' @returns The httr2 response object
#'
#' @examples
#' access_api(api, "se-7", "data", "2015")
access_api <- function(api, dataset_name, module, date, req_parameters=NULL){
  # check that api is a thenmaps api object
  if(!inherits(api, "thenmapsAPI")){
    stop("api must be an instance of thenmapsAPI")
  }
  # check that dataset name is one of the valid datasets
  # we check because the actual api does not seem to check
  if(is.null(dataset_name) || !(dataset_name %in% api$datasets)){
    stop("dataset_name must be one of ", api$datasets)
  }

  if(is.null(module) || !(module %in% api$modules)){
    stop("module must be one of ", api$datasets)
  }

  if(!is.null(req_parameters) && !is.list(req_parameters)){
    stop("req_parameters must be a list")
  }

  if(date !="*" && !all(grepl("^\\d\\d\\d\\d(-\\d\\d)?(-\\d\\d)?$", date))){
    stop("date format must be wildcard ('*') or match one of the
         following formats: YYYY, YYYY-MM, YYYY-MM-DD")
  }

  request_url <- sprintf("%s/%s/%s/%s/%s", api$base_url, api$version, dataset_name, module, date)
  # add request parameters
  if(!is.null(req_parameters) && length(req_parameters)>0){
    params <- paste(names(req_parameters), req_parameters, sep="=", collapse="&")
    request_url <- sprintf("%s?%s", request_url, params)
  }

  # create request
  req <- httr2::request(request_url)
  resp <- req |> httr2::req_retry(max_tries = 5) |> httr2::req_perform() # retry automatically

  return(resp)
  # extract response body to correct type depending on module
  if(module=="svg"){
    # get raw and convert (TODO)
    return(httr2::resp_body_raw(resp))
  }

  if(module=="data" && req_parameters$data_format=="csv"){
    tmpfile <- tempfile()

    writeLines(httr2::resp_body_string(resp, encoding = "UTF-8"), con=tmpfile)

  }

  return(httr2::resp_body_json(resp, check_type=TRUE))
}
