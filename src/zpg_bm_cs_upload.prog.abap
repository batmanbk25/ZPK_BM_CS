*&---------------------------------------------------------------------*
*& Report ZPG_BM_CS_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZPG_BM_CS_UPLOAD.
INCLUDE ZIN_BM_CS_UPLOADTOP                     .    " Global Data

INCLUDE ZIN_BM_CS_UPLOADO01                     .  " PBO-Modules
INCLUDE ZIN_BM_CS_UPLOADI01                     .  " PAI-Modules
INCLUDE ZIN_BM_CS_UPLOADF01                     .  " FORM-Routines

AT SELECTION-SCREEN.
  PERFORM 1000_PAI.

AT SELECTION-SCREEN OUTPUT.
  PERFORM 1000_PBO.

START-OF-SELECTION.
  PERFORM 0000_MAIN_PROC.
