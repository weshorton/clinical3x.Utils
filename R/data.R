#' Patient Colors
#' 
#' Colors to differentiate patients
#' 
#' @format ## 'ptColors_v'
#' Named vector of length 5
#' \describe{
#'    \item{values}{Hex Color}
#'    \item{names}{Patient ID}
#' }
"ptColors_v"

#' RECIST Colors
#' 
#' Colors to differentiate RECIST responses
#' 
#' @format ## 'recistColors_v'
#' Named vector of length 3
#' \describe{
#'    \item{values}{Hex Color}
#'    \item{names}{RECIST Group}
#'  }
"recistColors_v"

#' Clinical Benefit Colors
#' 
#' Colors to differentiate Clinical Benefit responses
#' 
#' @format ## 'cbColors_v'
#' Named vector of length 2
#' \describe{
#'    \item{values}{Hex Color}
#'    \item{names}{Clinical Benefit Group}
#'  }
"cbColors_v"

#' Timepoints
#' 
#' Timepoints of patient measuremenets
#' 
#' @format ## 'timePoints_v'
#' Vector of length 17
#' \describe{
#'    \item{values}{Timepoint}
#' }
"timePoints_v"

#' Time Colors
#' 
#' Colors to differentiate timepoints
#' 
#' @format ## 'timeColors_v'
#' Named vector of length 17
#' \describe{
#'    \item{values}{Hex Color}
#'    \item{names}{Timepoint}
#'  }
"timeColors_v"

#' Individual Time Colors
#' 
#' Colors for each cycle for specific plots
#' 
#' @format ## 'indTimeColors_lsv'
#' Named list of length 5
#' \describe{
#'    \item{values}{Color-blind friendly colors}
#'    \item{names}{name of cycle or day set}
#' }
"indTimeColors_lsv"

#' Dir Colors
#' 
#' Colors to differentiate direction of measurement change
#' 
#' @format ## 'dirColors_v'
#' Named vector of length 3
#' \describe{
#'    \item{values}{Hex Color}
#'    \item{names}{Direction}
#'  }
"dirColors_v"

#' Cycles
#' 
#' Base names of each cycle that have measurements
#' 
#' @format ## 'cycles_v'
#' Vector of length 7
#' \describe{
#'    \item{values}{Cycle}
#'  }
"cycles_v"

#' Cycles and Days
#' 
#' List of each cycle and the day measurements it contains
#' 
#' @format ## 'cycles_lsv'
#' Named list of length 4
#' \describe{
#'    \item{values}{Cycle/Day designations}
#'    \item{names}{Cycle designation}
#'  }
"cycles_lsv"

#' Sheet Rename
#' 
#' Map table to rename populations that get cut off by excel
#' 
#' @format ## 'sheetRename_dt'
#' data.table mapping short and long names
#' \describe{
#'    \item{Short}{Shortened name}
#'    \item{Long}{Original (long) name}
#'  }
"sheetRename_dt"

#' Colors
#' 
#' Population and Marker colors
#' 
#' @format ## 'colors_v'
#' Named vector of length 56
 #' \describe{
#'    \item{values}{Hex Code}
#'    \item{names}{population/marker name}
#'  }
"colors_v"

#' Short Colors
#' 
#' Short Population and Marker colors
#' 
#' @format ## 'shortNameColors_v'
#' Named vector of length 56
#' \describe{
#'    \item{values}{Hex Code}
#'    \item{names}{shortened population/marker name}
#'  }
"shortNameColors_v"

#' Colors2
#' 
#' Version2 of Population and Marker colors
#' 
#' @format ## 'colors_v2_v'
#' Named vector of length 56
#' \describe{
#'    \item{values}{Hex Code}
#'    \item{names}{population/marker name}
#'  }
"colors_v2_v"

#' Short Colors2
#' 
#' Version2 of Short Population and Marker colors
#' 
#' @format ## 'shortNameColors_v2_v'
#' Named vector of length 56
#' \describe{
#'    \item{values}{Hex Code}
#'    \item{names}{shortened population/marker name}
#'  }
"shortNameColors_v2_v"

#' CD4/CD8 Markers
#' 
#' Markers of CD4+ and CD8+ Cells
#' 
#' @format ## 'cd48Markers_v'
#' Vector of length 9
#' \describe{
#'    \item{values}{Marker}
#'  }
"cd48Markers_v"

#' CD4/CD8 Markers List
#' 
#' Markers of CD4+ and CD8+ Cells (split into groups)
#' 
#' @format ## 'cd48Markers_lsv'
#' List of length 3
#' \describe{
#'    \item{names}{placeholder separater}
#'    \item{values}{Marker}
#'  }
"cd48Markers_lsv"

#' Radar Markers
#' 
#' Markers for radar plots
#' 
#' @format ## 'radarMarkers_v'
#' Vector of length 7
#' \describe{
#'    \item{values}{Marker}
#'  }
"radarMarkers_v"

#' Compact Schema
#' 
#' One-row table of treatment schema
#' 
#' @format ## 'compactSchema_dt'
#' data.table identifying treatments given on each day
#' \describe{
#'    \item{A}{Day}
#'    \item{B}{Treatment(s) given that day}
#'  }
"compactSchema_dt"

#' Full Schema
#' 
#' Multi-row table of treatment schema
#' 
#' @format ## 'fullSchema_dt'
#' data.table identifying treatments given on each day
#' \describe{
#'    \item{A}{Day}
#'    \item{B}{Axatilimab (aCSF1R) treatments}
#'    \item{C}{Retifanlimab (aPD1) treatments}
#'    \item{D}{Paclitaxel (PTX) treatments}
#'  }
"fullSchema_dt"

#' Color Map Table
#' 
#' data.table to map color names (including combo names) with hex codes
#' 
#' @format ## 'schemaColorMap_dt'
#' data.table identifying treatments given on each day
#' \describe{
#'    \item{Tx}{Treatment (or treatment combination)}
#'    \item{Color}{Hex code}
#'  }
"schemaColorMap_dt"

#' Color Map Vector
#' 
#' vector to map color names (including combo names) with hex codes
#' Same as schemaColorMap_dt, but vector form
#' 
#' @format ## 'schemaColorMap_v'
#' vector with treatment colors
#' \describe{
#'    \item{names}{Treatment (or treatment combination)}
#'    \item{values}{Hex code}
#'  }
"schemaColorMap_v"

#' Treatment-Date Map Table
#' 
#' data.table to map time points (date) with cycle, day, treatment, and color
#' 
#' @format ## 'treatDateMap_dt'
#' data.table identifying treatments given on each day/cycle and their color
#' \describe{
#'    \item{Time points}{CycleDay time points}
#'    \item{Cycle}{treatment cycle}
#'    \item{Day}{treatment day (within cycle)}
#'    \item{Tx}{Treatment (or treatment combination)}
#'    \item{Color}{Hex code}
#'  }
"treatDateMap_dt"