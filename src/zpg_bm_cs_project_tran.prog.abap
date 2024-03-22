*&---------------------------------------------------------------------*
*& Report  ZPG_CE104_08
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZPG_BM_CS_PROJECT_TRAN.
INCLUDE ZIN_BM_CS_PROJECT_TRAN_TOP.                .  " global Data

* INCLUDE ZIN_BM_CS_PROJECT_TRAN_O01                        .  " PBO-Modules
* INCLUDE ZIN_BM_CS_PROJECT_TRAN_I01                        .  " PAI-Modules
INCLUDE ZIN_BM_CS_PROJECT_TRAN_F01.                .  " FORM-Routines

AT SELECTION-SCREEN OUTPUT.
  PERFORM 1000_PBO.

START-OF-SELECTION.
  PERFORM MAIN_PROC.
