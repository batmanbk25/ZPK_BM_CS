
*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_PROJECTI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1001 INPUT.

  CASE SY-UCOMM.
    WHEN '&F03' OR '&BACK' OR '&F12' OR '&F15'.
      LEAVE TO SCREEN 0.
    WHEN '&CREATE'.
      G_HIDE = '0'.
*      PERFORM HIDE_ALV.
      PERFORM CREATE_PROJECT.

    WHEN '&EDIT'.
      CLEAR
      G_LOCK.
      PERFORM LOCK_PROJECT.

      IF G_LOCK = '0'.
        PERFORM DISPLAY.
      ELSE.
        G_HIDE = '1'.
        PERFORM EDIT.
      ENDIF.
    WHEN '&DELETE'.
      PERFORM DELETE_PROJECT.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1002 INPUT.
  CASE SY-UCOMM.
    WHEN '&F03' OR '&F12' OR '&F15'.
      PERFORM UNLOCK_PROJECT.
      LEAVE TO SCREEN 0.
    WHEN '&DATA_SAVE'.
      PERFORM CREATE.
    WHEN '&DETAIL'.
      PERFORM DETAIL.
    WHEN '&EXPORT'.
      PERFORM EXPORT.
    WHEN '&TOGGLE'.
      IF G_MODE = '3'.
        CLEAR
      G_LOCK.
        PERFORM LOCK_PROJECT.

        IF G_LOCK = '0'.
          G_MODE = '3'.
        ELSE.
          G_HIDE = '1'.
          G_MODE = '2'.
        ENDIF.
      ELSEIF G_MODE = '2'.
        PERFORM UNLOCK_PROJECT.
        G_MODE = '3'.
        G_LOCK = '0'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
