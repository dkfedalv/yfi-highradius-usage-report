
CLASS lcl_data_processing DEFINITION FINAL.

  PUBLIC SECTION.
    DATA: ao_input  TYPE REF TO lcl_data_input,
          ao_output TYPE REF TO lcl_data_output,
          at_data   TYPE tt_vbak.

    CLASS-METHODS get_instance RETURNING VALUE(ro_data_processing) TYPE REF TO lcl_data_processing.
    METHODS: constructor,
      get_data,
      prepare_data,
      process_data.

  PRIVATE SECTION.
    METHODS:
      display_data.

ENDCLASS.

CLASS lcl_data_processing IMPLEMENTATION.
  METHOD get_instance.
    ro_data_processing = NEW #( ).
  ENDMETHOD.

  METHOD constructor.
    ao_input = lcl_data_input=>get_instance( ).
  ENDMETHOD.

  METHOD process_data.
    get_data( ).
    prepare_data( ).
    display_data( ).
  ENDMETHOD.

  METHOD get_data.
* Fill at_data
    SELECT * FROM vbak INTO TABLE @at_data
    WHERE bukrs_vf  IN @ao_input->ar_bukrs AND
          vkorg     IN @ao_input->ar_vkorg AND
          auart     IN @ao_input->ar_auart AND
          erdat     IN @ao_input->ar_erdat AND
          kunnr     IN @ao_input->ar_kunnr.
  ENDMETHOD.

  METHOD prepare_data.
* Transform, process, etc
  ENDMETHOD.

  METHOD display_data.
    ao_output = NEW lcl_data_output( at_data ).
    IF ao_output IS BOUND.
      ao_output->display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
