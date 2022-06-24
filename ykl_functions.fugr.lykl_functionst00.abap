*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 06.06.2022 at 12:54:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YKL_PLATE.......................................*
DATA:  BEGIN OF STATUS_YKL_PLATE                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YKL_PLATE                     .
CONTROLS: TCTRL_YKL_PLATE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YKL_PLATE                     .
TABLES: YKL_PLATE                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
