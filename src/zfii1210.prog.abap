TABLES: t001, tvko, vbak.
SELECT-OPTIONS: s_bukrs FOR t001-bukrs OBLIGATORY,
                s_vkorg FOR tvko-vkorg,
                s_auart FOR vbak-auart,
                s_kunnr FOR vbak-kunnr,
                s_erdat FOR vbak-erdat OBLIGATORY.

AT SELECTION-SCREEN.
  LOOP AT s_erdat.
    IF  s_erdat-low < '20230101'.
      MESSAGE TEXT-V01 TYPE 'E'.
    ENDIF.
  ENDLOOP.

CLASS lcl_data_input DEFINITION FINAL.

  PUBLIC SECTION.
    DATA: ar_bukrs TYPE RANGE OF bukrs,
          ar_vkorg TYPE RANGE OF vkorg,
          ar_auart TYPE RANGE OF auart,
          ar_kunnr TYPE RANGE OF kunnr,
          ar_erdat TYPE RANGE OF erdat.

    CLASS-METHODS get_instance RETURNING VALUE(ro_data_input) TYPE REF TO lcl_data_input.
    METHODS: constructor.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_data_input IMPLEMENTATION.

  METHOD constructor.
    ar_bukrs = s_bukrs[].
    ar_vkorg = s_vkorg[].
    ar_auart = s_auart[].
    ar_kunnr = s_kunnr[].
    ar_erdat = s_erdat[].
  ENDMETHOD.

  METHOD get_instance.
    ro_data_input = NEW #( ).
  ENDMETHOD.

ENDCLASS.
