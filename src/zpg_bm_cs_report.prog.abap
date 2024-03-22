*&---------------------------------------------------------------------*
*& Report ZPG_BM_CS_REPORT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

INCLUDE ZIN_BM_CS_REPORTTOP                     .    " Global Data

INCLUDE ZIN_BM_CS_REPORTO01                     .  " PBO-Modules

INCLUDE ZIN_BM_CS_REPORTI01                     .  " PAI-Modules

INCLUDE ZIN_BM_CS_REPORTF01                     .  " FORM-Routines

AT SELECTION-SCREEN OUTPUT.
  PERFORM LIST_STATUS.

START-OF-SELECTION.
  PERFORM MAIN_PROCESS.
