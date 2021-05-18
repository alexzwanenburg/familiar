# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

# Default
familiar:::test_export(export_function=familiar:::export_risk_stratification_info,
                       data_element="risk_stratification_info",
                       always_available=TRUE,
                       outcome_type_available=c("survival"),
                       debug=debug_flag)

# With ensemble detail level.
familiar:::test_export(export_function=familiar:::export_risk_stratification_info,
                       data_element="risk_stratification_info",
                       detail_level="ensemble",
                       always_available=TRUE,
                       outcome_type_available=c("survival"),
                       debug=debug_flag)

# Without aggregation
familiar:::test_export(export_function=familiar:::export_risk_stratification_info,
                       data_element="risk_stratification_info",
                       always_available=TRUE,
                       outcome_type_available=c("survival"),
                       export_args=list("aggregate_results"=FALSE),
                       debug=debug_flag)
