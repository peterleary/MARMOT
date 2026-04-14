# test-marker-types.R
# Exhaustive testing of markerType handling: named vector construction,
# marker_class assignment, feature selection, DS override, heatmap split,
# marker pairs, and Parquet round-trip.

# ── Local helpers replicating QMD logic ──────────────────────────────────────

#' Replicate QMD lines 330-339: extract markerType + featureType from study_data
extract_marker_info_test <- function(smd) {
  mt_mask <- !is.na(smd$`Marker Type`)
  include_col <- colnames(smd)[grep("include|cluster", colnames(smd))]
  markerType <- setNames(smd$`Marker Type`[mt_mask], smd[[include_col]][mt_mask])
  featureType <- if ("type" %in% markerType) "type" else NULL
  list(markerType = markerType, featureType = featureType)
}

#' Replicate QMD lines 460-470: build panel with marker_class from param_df
#'
#' @param param_df data.frame with `name` and `desc` columns (like pData(parameters(ff)))
#' @param useMarkers character vector of markers to include
#' @param markerType named character vector (names=marker names, values=types)
#' @param removeMarkers character vector of markers to exclude (default empty)
build_marker_class_test <- function(param_df, useMarkers, markerType,
                                     removeMarkers = character(0)) {
  panel <- param_df |>
    dplyr::select(fcs_colname = name, marker_name = desc) |>
    dplyr::mutate(
      antigen       = marker_name,
      marker_class  = ifelse(marker_name %in% names(markerType), markerType[marker_name], "none"),
      use_channel   = marker_name %in% useMarkers
    ) |>
    dplyr::filter(!marker_name %in% removeMarkers) |>
    dplyr::filter(marker_name %in% useMarkers) |>
    dplyr::arrange(match(marker_name, useMarkers))
  panel
}

#' Replicate QMD: is the panel homogeneous (missing one of type/state)?
#' Matches the idiom used at MARMOT_Pipeline.qmd L622.
is_homogeneous_test <- function(markerType) {
  !(("type" %in% markerType) && ("state" %in% markerType))
}

#' Replicate QMD clustering feature selection (MARMOT_Pipeline.qmd L1044-1058).
#' When homogeneousPanel is TRUE, the user's markersToClusterBy choice is
#' overridden and all panel markers are returned.
select_cluster_features_test <- function(panel, featureType, markersToClusterBy,
                                         homogeneousPanel = FALSE) {
  if (homogeneousPanel) {
    panel$marker_name
  } else if (!is.null(featureType) && markersToClusterBy == "type") {
    panel$marker_name[panel$marker_class == "type"]
  } else if (!is.null(featureType) && markersToClusterBy == "state") {
    panel$marker_name[panel$marker_class == "state"]
  } else if (markersToClusterBy %in% c("type", "state", "all")) {
    panel$marker_name
  } else {
    panel$marker_name
  }
}

#' Replicate QMD DR feature selection (MARMOT_Pipeline.qmd L1224-1233 and
#' L1241-1250). When homogeneousPanel is TRUE, the user's markersToDimRedBy
#' choice is overridden and all panel markers are returned.
select_dr_features_test <- function(panel, featureType, markersToDimRedBy,
                                    homogeneousPanel = FALSE) {
  if (homogeneousPanel) {
    panel$marker_name
  } else if (!is.null(featureType) && markersToDimRedBy == "type") {
    panel$marker_name[panel$marker_class == "type"]
  } else if (!is.null(featureType) && markersToDimRedBy == "state") {
    panel$marker_name[panel$marker_class == "state"]
  } else {
    panel$marker_name
  }
}

#' Replicate QMD lines 1232-1236: DS override when no state markers
#' (featureType is no longer used in the guard — kept in signature for
#'  documentation parity with the pre-fix code)
apply_ds_override_test <- function(marker_classes, featureType = NULL) {
  if (!any(marker_classes == "state")) {
    rep("state", length(marker_classes))
  } else {
    marker_classes
  }
}

# ── Build study_data sheets for each scenario ───────────────────────────────

#' Build a study_data data.frame for testing
#' @param marker_names Character vector of marker names
#' @param marker_types Character vector of types ("type"/"state"), same length
#' @param n_contrasts Number of contrast rows (default 1)
make_study_data <- function(marker_names, marker_types,
                             n_contrasts = 1, include_pairs = FALSE) {
  n <- length(marker_names)
  stopifnot(length(marker_types) == n)
  n_rows <- max(n, n_contrasts + 1, 4)

  contrasts <- if (n_contrasts >= 1) {
    c(paste0("Contrast_", seq_len(n_contrasts)), rep(NA, n_rows - n_contrasts))
  } else {
    rep(NA, n_rows)
  }
  conditions <- c("Control", "Treatment", rep(NA, n_rows - 2))

  pairs <- if (include_pairs) {
    c(paste(marker_names[1], marker_names[min(2, n)], sep = ":"),
      rep(NA, n_rows - 1))
  } else {
    rep(NA, n_rows)
  }

  data.frame(
    `Markers to include for clustering` = c(marker_names, rep(NA, n_rows - n)),
    `Marker Type` = c(marker_types, rep(NA, n_rows - n)),
    `Markers to exclude completely` = rep(NA, n_rows),
    `Cofactors for markers to use` = c(rep(150, n), rep(NA, n_rows - n)),
    `Conditions To Test` = contrasts,
    `Conditions Order` = conditions,
    `Cells per condition in UMAPs etc.` = c(rep("500", 2), rep(NA, n_rows - 2)),
    `Marker Pairs` = pairs,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Build a param_df with scatter/time channels + bio markers
#' (Mimics real FCS files that always have non-bio channels)
make_param_df_with_scatter <- function(marker_names) {
  scatter_channels <- data.frame(
    name = c("FSC-H", "FSC-A", "SSC-H", "SSC-A", "SSC-B-H", "SSC-B-A", "Time",
             "FJComp-Zombie-NIR-A", "FJComp-APC-Fire810-A"),
    desc = c("FSC-H", "FSC-A", "SSC-H", "SSC-A", "SSC-B-H", "SSC-B-A", "Time",
             "LD", "CD45"),
    stringsAsFactors = FALSE
  )
  bio_channels <- data.frame(
    name = paste0("Ch", seq_along(marker_names)),
    desc = marker_names,
    stringsAsFactors = FALSE
  )
  rbind(scatter_channels, bio_channels)
}

#' Build a param_df with ONLY bio markers (no scatter — like test FCS files)
make_param_df_bio_only <- function(marker_names) {
  data.frame(
    name = paste0("Ch", seq_along(marker_names)),
    desc = marker_names,
    stringsAsFactors = FALSE
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# Part A: Unit Tests
# ══════════════════════════════════════════════════════════════════════════════

# ── Scenario definitions ─────────────────────────────────────────────────────

# Org19-like: 13 type + 8 state
org19_markers <- c("CD4", "CD8a", "MHCII", "CD19", "Ly6G", "CD103",
                    "CD11b", "F480", "NK11", "FoxP3", "TCRb", "CD11c",
                    "Ly6C", "KLRG1", "CD44", "LAG3", "Ki67", "PD1",
                    "CD25", "TIM3", "ICOS")
org19_types <- c(rep("type", 13), rep("state", 8))
org19_type_markers <- org19_markers[1:13]
org19_state_markers <- org19_markers[14:21]

scenarios <- list(
  org19_mix = list(
    name = "Org19 mix (13T+8S)",
    markers = org19_markers,
    types = org19_types,
    expect_featureType = "type",
    expect_type_markers = org19_type_markers,
    expect_state_markers = org19_state_markers,
    expect_heatmap_split = TRUE,
    expect_homogeneous = FALSE,
    include_pairs = TRUE
  ),
  all_state = list(
    name = "All state (21S)",
    markers = org19_markers,
    types = rep("state", 21),
    expect_featureType = NULL,
    expect_type_markers = character(0),
    expect_state_markers = org19_markers,
    expect_heatmap_split = FALSE,
    expect_homogeneous = TRUE,
    include_pairs = FALSE
  ),
  all_type = list(
    name = "All type (21T)",
    markers = org19_markers,
    types = rep("type", 21),
    expect_featureType = "type",
    expect_type_markers = org19_markers,
    expect_state_markers = character(0),
    expect_heatmap_split = FALSE,
    expect_homogeneous = TRUE,
    include_pairs = FALSE
  ),
  min_1t_1s = list(
    name = "1 type + 1 state",
    markers = c("CD4", "KLRG1"),
    types = c("type", "state"),
    expect_featureType = "type",
    expect_type_markers = "CD4",
    expect_state_markers = "KLRG1",
    expect_heatmap_split = TRUE,
    expect_homogeneous = FALSE,
    include_pairs = TRUE
  ),
  min_1t_20s = list(
    name = "1 type + 20 state",
    markers = org19_markers,
    types = c("type", rep("state", 20)),
    expect_featureType = "type",
    expect_type_markers = "CD4",
    expect_state_markers = org19_markers[2:21],
    expect_heatmap_split = TRUE,
    expect_homogeneous = FALSE,
    include_pairs = TRUE
  ),
  min_20t_1s = list(
    name = "20 type + 1 state",
    markers = org19_markers,
    types = c(rep("type", 20), "state"),
    expect_featureType = "type",
    expect_type_markers = org19_markers[1:20],
    expect_state_markers = "ICOS",
    expect_heatmap_split = TRUE,
    expect_homogeneous = FALSE,
    include_pairs = TRUE
  )
)


# ── A1: markerType extraction ────────────────────────────────────────────────

test_that("extract_marker_info: markerType named vector correct for each scenario", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types, include_pairs = sc$include_pairs)
    info <- extract_marker_info_test(smd)

    # markerType has correct names
    expect_equal(names(info$markerType), sc$markers, info = paste(sc$name, "names"))

    # markerType has correct values
    expect_equal(unname(info$markerType), sc$types, info = paste(sc$name, "values"))

    # featureType correct
    if (is.null(sc$expect_featureType)) {
      expect_null(info$featureType, info = paste(sc$name, "featureType"))
    } else {
      expect_equal(info$featureType, sc$expect_featureType,
                   info = paste(sc$name, "featureType"))
    }
  }
})


# ── A2: marker_class assignment (critical regression: with scatter channels) ─

test_that("build_marker_class: correct with scatter/time channels in param_df", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)

    # param_df WITH scatter/time channels (real FCS layout)
    param_df <- make_param_df_with_scatter(sc$markers)
    expect_true(nrow(param_df) > length(sc$markers),
                info = paste(sc$name, "param_df has extra scatter rows"))

    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)

    # Every marker gets correct class
    for (i in seq_along(sc$markers)) {
      expect_equal(panel$marker_class[i], sc$types[i],
                   info = paste(sc$name, "marker_class for", sc$markers[i]))
    }

    # Panel order matches Excel order
    expect_equal(panel$marker_name, sc$markers,
                 info = paste(sc$name, "panel order"))

    # No "none" class in the filtered panel
    expect_false(any(panel$marker_class == "none"),
                 info = paste(sc$name, "no 'none' in panel"))
  }
})

test_that("build_marker_class: correct with bio-only param_df (test FCS)", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)

    # param_df with ONLY bio markers (like test fixture FCS)
    param_df <- make_param_df_bio_only(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)

    # Every marker gets correct class
    for (i in seq_along(sc$markers)) {
      expect_equal(panel$marker_class[i], sc$types[i],
                   info = paste(sc$name, "bio-only class for", sc$markers[i]))
    }
  }
})

test_that("REGRESSION: old positional recycling assigns wrong classes with scatter", {

  # This test demonstrates the bug that the fix addresses.
  # With the OLD code: ifelse(marker_name %in% useMarkers, markerType, "none")
  # where markerType is positional (length 21), the 9 scatter rows cause

  # recycling to shift types starting at position 10.
  sc <- scenarios$org19_mix
  smd <- make_study_data(sc$markers, sc$types)
  info <- extract_marker_info_test(smd)
  param_df <- make_param_df_with_scatter(sc$markers)

  # Simulate the OLD buggy code path
  old_markerType <- smd$`Marker Type`[!is.na(smd$`Marker Type`)]  # positional, no names
  old_panel <- param_df |>
    dplyr::select(fcs_colname = name, marker_name = desc) |>
    dplyr::mutate(
      marker_class = ifelse(marker_name %in% sc$markers, old_markerType, "none")
    ) |>
    dplyr::filter(marker_name %in% sc$markers) |>
    dplyr::arrange(match(marker_name, sc$markers))

  # The old code recycles — some markers get WRONG types
  # With 9 scatter rows and 21 bio markers, recycling wraps at position 22
  # So markers past position 12 in the 30-row param_df get shifted types
  wrong_count <- sum(old_panel$marker_class != sc$types)
  expect_true(wrong_count > 0, info = "Old code should produce wrong assignments")

  # The FIXED code should produce correct assignments
  panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
  expect_equal(panel$marker_class, sc$types, info = "Fixed code: all correct")
})


# ── A3: Feature selection for clustering ─────────────────────────────────────

test_that("select_cluster_features: 'all' returns all markers in Excel order", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    param_df <- make_param_df_with_scatter(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
    hp <- is_homogeneous_test(info$markerType)

    features <- select_cluster_features_test(panel, info$featureType, "all",
                                             homogeneousPanel = hp)
    expect_equal(features, sc$markers, info = paste(sc$name, "cluster by all"))
  }
})

test_that("select_cluster_features: 'type' returns type markers (or all if homogeneous)", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    param_df <- make_param_df_with_scatter(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
    hp <- is_homogeneous_test(info$markerType)

    features <- select_cluster_features_test(panel, info$featureType, "type",
                                             homogeneousPanel = hp)

    if (hp) {
      # Homogeneous panel → override to all markers regardless of choice
      expect_equal(features, sc$markers,
                   info = paste(sc$name, "homogeneous override (cluster by type)"))
    } else {
      expect_equal(features, sc$expect_type_markers,
                   info = paste(sc$name, "cluster by type"))
    }
  }
})

test_that("select_cluster_features: 'state' returns state markers (or all if homogeneous)", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    param_df <- make_param_df_with_scatter(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
    hp <- is_homogeneous_test(info$markerType)

    features <- select_cluster_features_test(panel, info$featureType, "state",
                                             homogeneousPanel = hp)

    if (hp) {
      expect_equal(features, sc$markers,
                   info = paste(sc$name, "homogeneous override (cluster by state)"))
    } else {
      expect_equal(features, sc$expect_state_markers,
                   info = paste(sc$name, "cluster by state"))
    }
  }
})


# ── A4: Feature selection for DR ─────────────────────────────────────────────

test_that("select_dr_features: 'all' returns all markers in Excel order", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    param_df <- make_param_df_with_scatter(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
    hp <- is_homogeneous_test(info$markerType)

    features <- select_dr_features_test(panel, info$featureType, "all",
                                        homogeneousPanel = hp)
    expect_equal(features, sc$markers, info = paste(sc$name, "DR by all"))
  }
})

test_that("select_dr_features: 'type' returns type markers (or all if homogeneous)", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    param_df <- make_param_df_with_scatter(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
    hp <- is_homogeneous_test(info$markerType)

    features <- select_dr_features_test(panel, info$featureType, "type",
                                        homogeneousPanel = hp)

    if (hp) {
      expect_equal(features, sc$markers,
                   info = paste(sc$name, "homogeneous override (DR by type)"))
    } else {
      expect_equal(features, sc$expect_type_markers,
                   info = paste(sc$name, "DR by type"))
    }
  }
})

test_that("select_dr_features: 'state' returns state markers (or all if homogeneous)", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    param_df <- make_param_df_with_scatter(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
    hp <- is_homogeneous_test(info$markerType)

    features <- select_dr_features_test(panel, info$featureType, "state",
                                        homogeneousPanel = hp)

    if (hp) {
      expect_equal(features, sc$markers,
                   info = paste(sc$name, "homogeneous override (DR by state)"))
    } else {
      expect_equal(features, sc$expect_state_markers,
                   info = paste(sc$name, "DR by state"))
    }
  }
})


# ── A3b: homogeneousPanel detection ──────────────────────────────────────────

test_that("is_homogeneous_test: correct for each scenario", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    expect_equal(is_homogeneous_test(info$markerType), sc$expect_homogeneous,
                 info = paste(sc$name, "homogeneousPanel"))
  }
})

test_that("is_homogeneous_test: TRUE for empty markerType (missing column / all-NA)", {
  # Missing Marker Type column → markerType is named(character(0))
  expect_true(is_homogeneous_test(setNames(character(0), character(0))))
  # Explicitly NULL
  expect_true(is_homogeneous_test(NULL))
})

test_that("REGRESSION: all-type panel + 'state' choice no longer returns empty", {
  # Before the homogeneousPanel fix, an all-type panel with markersToClusterBy
  # or markersToDimRedBy set to "state" produced character(0) and crashed
  # downstream in prcomp / FlowSOM / phenograph. It must now return all markers.
  sc <- scenarios$all_type
  smd <- make_study_data(sc$markers, sc$types)
  info <- extract_marker_info_test(smd)
  param_df <- make_param_df_with_scatter(sc$markers)
  panel <- build_marker_class_test(param_df, sc$markers, info$markerType)
  hp <- is_homogeneous_test(info$markerType)
  expect_true(hp, info = "All-type panel is homogeneous")

  # Reproduce the old broken behaviour without the override for comparison
  old_cluster <- select_cluster_features_test(panel, info$featureType, "state",
                                              homogeneousPanel = FALSE)
  expect_length(old_cluster, 0)

  old_dr <- select_dr_features_test(panel, info$featureType, "state",
                                    homogeneousPanel = FALSE)
  expect_length(old_dr, 0)

  # Fixed behaviour with the homogeneous override
  new_cluster <- select_cluster_features_test(panel, info$featureType, "state",
                                              homogeneousPanel = hp)
  expect_equal(new_cluster, sc$markers)

  new_dr <- select_dr_features_test(panel, info$featureType, "state",
                                    homogeneousPanel = hp)
  expect_equal(new_dr, sc$markers)
})


# ── A5: DS override ──────────────────────────────────────────────────────────

test_that("apply_ds_override: no-op when state markers exist", {
  for (sc_name in c("org19_mix", "min_1t_1s", "min_1t_20s", "min_20t_1s")) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types)
    info <- extract_marker_info_test(smd)
    param_df <- make_param_df_with_scatter(sc$markers)
    panel <- build_marker_class_test(param_df, sc$markers, info$markerType)

    result <- apply_ds_override_test(panel$marker_class, info$featureType)
    expect_equal(result, panel$marker_class,
                 info = paste(sc$name, "DS override no-op"))
  }
})

test_that("apply_ds_override: forces all to state when no state markers", {
  sc <- scenarios$all_type
  smd <- make_study_data(sc$markers, sc$types)
  info <- extract_marker_info_test(smd)
  param_df <- make_param_df_with_scatter(sc$markers)
  panel <- build_marker_class_test(param_df, sc$markers, info$markerType)

  result <- apply_ds_override_test(panel$marker_class, info$featureType)
  expect_true(all(result == "state"), info = "All-type: all forced to state")
  expect_equal(length(result), 21)
})

test_that("apply_ds_override: forces all to state when all state (featureType NULL)", {
  sc <- scenarios$all_state
  smd <- make_study_data(sc$markers, sc$types)
  info <- extract_marker_info_test(smd)
  param_df <- make_param_df_with_scatter(sc$markers)
  panel <- build_marker_class_test(param_df, sc$markers, info$markerType)

  # All-state: marker_class is already all "state", so override is a no-op
  result <- apply_ds_override_test(panel$marker_class, info$featureType)
  expect_true(all(result == "state"), info = "All-state: remains state")
})


# ── A6: Heatmap split condition ──────────────────────────────────────────────

test_that("heatmap split condition correct for each scenario", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types, include_pairs = sc$include_pairs)
    info <- extract_marker_info_test(smd)

    has_split <- ("type" %in% info$markerType) && ("state" %in% info$markerType)
    expect_equal(has_split, sc$expect_heatmap_split,
                 info = paste(sc$name, "heatmap split"))
  }
})


# ── A7: Marker pair condition ────────────────────────────────────────────────

test_that("marker pair condition correct for each scenario", {
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    smd <- make_study_data(sc$markers, sc$types, include_pairs = sc$include_pairs)
    info <- extract_marker_info_test(smd)

    run_pairs <- ("type" %in% info$markerType) &&
                 ("state" %in% info$markerType) &&
                 ("Marker Pairs" %in% colnames(smd))
    # Pairs should run when we have both types AND a pairs column with data
    expect_equal(run_pairs, sc$expect_heatmap_split,
                 info = paste(sc$name, "marker pairs"))
  }
})


# ── A8: Named lookup preserves marker order ──────────────────────────────────

test_that("named lookup preserves Excel marker order regardless of param_df order", {
  # Shuffle the param_df rows to simulate different FCS channel orderings
  set.seed(99)
  sc <- scenarios$org19_mix
  smd <- make_study_data(sc$markers, sc$types)
  info <- extract_marker_info_test(smd)

  param_df <- make_param_df_with_scatter(sc$markers)
  # Shuffle ALL rows (scatter + bio)
  param_df <- param_df[sample(nrow(param_df)), ]

  panel <- build_marker_class_test(param_df, sc$markers, info$markerType)

  # Panel should be in Excel order, not param_df order
  expect_equal(panel$marker_name, sc$markers)
  expect_equal(panel$marker_class, sc$types)
})


# ── A9: Edge cases ──────────────────────────────────────────────────────────

test_that("markers with special characters in names work correctly", {
  markers <- c("CD3-e", "CD4/CD8", "HLA DR")
  types <- c("type", "state", "type")
  smd <- make_study_data(markers, types)
  info <- extract_marker_info_test(smd)

  expect_equal(names(info$markerType), markers)
  expect_equal(unname(info$markerType), types)

  param_df <- make_param_df_with_scatter(markers)
  panel <- build_marker_class_test(param_df, markers, info$markerType)
  expect_equal(panel$marker_class, types)
})

test_that("single marker scenario works", {
  smd <- make_study_data("CD4", "type")
  info <- extract_marker_info_test(smd)

  expect_equal(names(info$markerType), "CD4")
  expect_equal(unname(info$markerType), "type")
  expect_equal(info$featureType, "type")

  param_df <- make_param_df_with_scatter("CD4")
  panel <- build_marker_class_test(param_df, "CD4", info$markerType)
  expect_equal(nrow(panel), 1)
  expect_equal(panel$marker_class, "type")

  # DS override should force to state
  result <- apply_ds_override_test(panel$marker_class, info$featureType)
  expect_equal(result, "state")
})

test_that("removeMarkers excludes specified markers", {
  sc <- scenarios$org19_mix
  smd <- make_study_data(sc$markers, sc$types)
  info <- extract_marker_info_test(smd)
  param_df <- make_param_df_with_scatter(sc$markers)

  # Remove 2 markers
  remove <- c("CD4", "KLRG1")
  panel <- build_marker_class_test(param_df, sc$markers, info$markerType,
                                    removeMarkers = remove)

  expect_false("CD4" %in% panel$marker_name)
  expect_false("KLRG1" %in% panel$marker_name)
  expect_equal(nrow(panel), 19)
})


# ── A10: DS h5ad save fix ─────────────────────────────────────────────────

test_that("save_da_ds_h5ad handles nested dsList entries", {
  skip_if_not_installed("anndataR")

  # Create a minimal h5ad to update
  sce <- make_mock_sce()
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  save_h5ad_data(tmp, envir = env)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  # Mock DA list (plain data.frames)
  daList <- list(
    "contrast1" = data.frame(cluster_id = 1:3, p_adj = c(0.01, 0.05, 0.9))
  )

  # Mock DS list (nested lists, matching pipeline structure)
  mock_tbl_DS <- data.frame(
    marker_id = c("CD4", "KLRG1", "CD44"),
    cluster_id = rep("c1", 3),
    p_adj = c(0.01, 0.5, 0.002)
  )
  dsList <- list(
    "contrast1" = list(res_DS = "placeholder", tbl_DS = mock_tbl_DS)
  )

  save_da_ds_h5ad(h5ad_path, daList = daList, dsList = dsList)

  # Read back and check
  ad <- anndataR::read_h5ad(h5ad_path)
  expect_true(!is.null(ad$uns$da_results$contrast1))
  expect_true(!is.null(ad$uns$ds_results$contrast1))

  ds_read <- as.data.frame(ad$uns$ds_results$contrast1)
  expect_equal(nrow(ds_read), 3)
  expect_true("marker_id" %in% colnames(ds_read))
  expect_equal(ds_read$marker_id, c("CD4", "KLRG1", "CD44"))
})

test_that("save_da_ds_h5ad still handles plain data.frame dsList entries", {
  skip_if_not_installed("anndataR")

  sce <- make_mock_sce()
  tmp <- withr::local_tempdir()
  env <- new.env(parent = emptyenv())
  env$sce <- sce
  env$md <- S4Vectors::metadata(sce)$experiment_info
  save_h5ad_data(tmp, envir = env)
  h5ad_path <- file.path(tmp, "marmot_results.h5ad")

  dsList <- list(
    "contrast1" = data.frame(marker_id = "CD4", p_adj = 0.01)
  )

  save_da_ds_h5ad(h5ad_path, daList = list(), dsList = dsList)

  ad <- anndataR::read_h5ad(h5ad_path)
  expect_true(!is.null(ad$uns$ds_results$contrast1))
})


# ══════════════════════════════════════════════════════════════════════════════
# Part B: Integration Tests (require pipeline deps + Quarto)
# ══════════════════════════════════════════════════════════════════════════════

# ── B1: Default mix (13T+8S) ─────────────────────────────────────────────────

test_that("integration: default mix (13T+8S) marker types correct", {
  skip_pipeline_deps()
  skip_if(Sys.getenv("NOT_CRAN") != "true", "Integration tests require NOT_CRAN=true")

  result <- run_realistic_pipeline_test(
    n_cells = 300,
    test_name = "MarkerTypeMix"
  )

  validate_pipeline_output(result)

  # Expected marker classes
  expected <- setNames(
    c(rep("type", 13), rep("state", 8)),
    org19_markers
  )
  validate_marker_type_output(
    result,
    expected_marker_classes = expected,
    expected_n_contrasts = 3,
    expect_ds_markers = org19_state_markers,
    expect_ds_saved = TRUE
  )
})

# ── B2: All state ────────────────────────────────────────────────────────────

test_that("integration: all state markers", {
  skip_pipeline_deps()
  skip_if(Sys.getenv("NOT_CRAN") != "true", "Integration tests require NOT_CRAN=true")

  result <- run_realistic_pipeline_test(
    n_cells = 300,
    marker_types = rep("state", 21),
    test_name = "MarkerTypeAllState"
  )

  validate_pipeline_output(result)

  # featureType is NULL, DS override forces all to state
  expected <- setNames(rep("state", 21), org19_markers)
  validate_marker_type_output(
    result,
    expected_marker_classes = expected,
    expected_n_contrasts = 3,
    expect_ds_markers = org19_markers,
    expect_ds_saved = TRUE
  )
})

# ── B3: All type ─────────────────────────────────────────────────────────────

test_that("integration: all type markers (DS forces to state)", {
  skip_pipeline_deps()
  skip_if(Sys.getenv("NOT_CRAN") != "true", "Integration tests require NOT_CRAN=true")

  result <- run_realistic_pipeline_test(
    n_cells = 300,
    marker_types = rep("type", 21),
    test_name = "MarkerTypeAllType"
  )

  validate_pipeline_output(result)

  # After DS override, all markers become "state" in the DS SCE copy
  # But rowData still shows "type" (override is only on sceDS copy)
  expected <- setNames(rep("type", 21), org19_markers)
  validate_marker_type_output(
    result,
    expected_marker_classes = expected,
    expected_n_contrasts = 3,
    # DS should test all 21 markers (all forced to state for DS)
    expect_ds_markers = org19_markers,
    expect_ds_saved = TRUE
  )
})

# ── B4: Cluster by type ──────────────────────────────────────────────────────

test_that("integration: cluster by type markers only", {
  skip_pipeline_deps()
  skip_if(Sys.getenv("NOT_CRAN") != "true", "Integration tests require NOT_CRAN=true")

  result <- run_realistic_pipeline_test(
    n_cells = 300,
    params = list(markersToClusterBy = "type"),
    test_name = "MarkerTypeClusterByType"
  )

  validate_pipeline_output(result)

  expected <- setNames(
    c(rep("type", 13), rep("state", 8)),
    org19_markers
  )
  validate_marker_type_output(
    result,
    expected_marker_classes = expected,
    expected_n_contrasts = 3,
    expect_ds_markers = org19_state_markers,
    expect_ds_saved = TRUE
  )
})

# ── B5: Cluster by state ─────────────────────────────────────────────────────

test_that("integration: cluster by state markers only", {
  skip_pipeline_deps()
  skip_if(Sys.getenv("NOT_CRAN") != "true", "Integration tests require NOT_CRAN=true")

  result <- run_realistic_pipeline_test(
    n_cells = 300,
    params = list(markersToClusterBy = "state"),
    test_name = "MarkerTypeClusterByState"
  )

  validate_pipeline_output(result)

  expected <- setNames(
    c(rep("type", 13), rep("state", 8)),
    org19_markers
  )
  validate_marker_type_output(
    result,
    expected_marker_classes = expected,
    expected_n_contrasts = 3,
    expect_ds_markers = org19_state_markers,
    expect_ds_saved = TRUE
  )
})

# ── B6: Simple all-state (8 markers) ─────────────────────────────────────────

test_that("integration: simple 8-marker all-state", {
  skip_pipeline_deps()
  skip_if(Sys.getenv("NOT_CRAN") != "true", "Integration tests require NOT_CRAN=true")

  result <- run_pipeline_test(
    n_cells = 500,
    n_markers = 8,
    marker_types = rep("state", 8),
    test_name = "MarkerTypeSimpleAllState"
  )

  validate_pipeline_output(result)

  marker_names <- paste0("Marker_", 1:8)
  expected <- setNames(rep("state", 8), marker_names)
  validate_marker_type_output(
    result,
    expected_marker_classes = expected,
    expected_n_contrasts = 1,
    expect_ds_markers = marker_names,
    expect_ds_saved = TRUE
  )
})

# ── B7: Parquet round-trip preserves marker_class ─────────────────────────────

test_that("integration: Parquet round-trip preserves marker_class", {
  skip_pipeline_deps()
  skip_if(Sys.getenv("NOT_CRAN") != "true", "Integration tests require NOT_CRAN=true")

  result <- run_realistic_pipeline_test(
    n_cells = 300,
    test_name = "MarkerTypeH5adRT"
  )

  h5ad_path <- result$h5ad_path

  # Load SCE from h5ad
  sce <- reconstruct_sce_from_h5ad(h5ad_path)
  rd <- SummarizedExperiment::rowData(sce)

  # marker_class preserved
  expect_true("marker_class" %in% colnames(rd))
  classes <- setNames(as.character(rd$marker_class), rownames(rd))

  # Check specific markers
  expect_equal(classes[["CD4"]], "type")
  expect_equal(classes[["KLRG1"]], "state")

  # DA results present
  ad <- anndataR::read_h5ad(h5ad_path)
  expect_equal(length(ad$uns$da_results), 3)

  # DS results present (now saved thanks to the fix)
  expect_true(!is.null(ad$uns$ds_results) && length(ad$uns$ds_results) > 0,
              info = "DS results saved after fix")
})
