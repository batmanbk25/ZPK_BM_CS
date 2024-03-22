*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_UPLOADI01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CALL FUNCTION 'ZFM_SCR_SIMPLE_FC_PROCESS'.

  CASE SY-UCOMM.
    WHEN 'SAVE'.
      PERFORM 0100_SAVE.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAVE' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  CASE SY-UCOMM.
    WHEN 'CANCEL' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
