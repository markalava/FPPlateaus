@echo off

ECHO.
ECHO.

ECHO. ================================================================================
ECHO. DATA
ECHO. ================================================================================

Rscript -e "setwd('data-raw'); example(source); sourceDir('.')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. DOCUMENT
ECHO. ================================================================================

Rscript -e "devtools::document()"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. INSTALL
ECHO. ================================================================================

Rscript -e "devtools::install(build_vignettes = FALSE, upgrade = 'never')"
if %ERRORLEVEL% GEQ 1 PAUSE


rem ECHO. ================================================================================
rem ECHO. TESTS
rem ECHO. ================================================================================

rem rem Rscript -e "testthat::test_local('tests/testthat')"
rem rem if %ERRORLEVEL% GEQ 1 PAUSE

rem Rscript -e "testthat::test_package('FPPlateaus')"
rem if %ERRORLEVEL% GEQ 1 PAUSE

rem ECHO.
rem ECHO.
rem ECHO.
rem ECHO.
rem ECHO.
rem ECHO.
rem ECHO.


ECHO. ================================================================================
PAUSE
