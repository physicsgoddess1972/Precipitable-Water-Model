***************
Data Formatting
***************

==========================
Data Collection Guidelines
==========================
First and foremost, collect honest data.

==================
Data Formatting
==================
As we have previously mentioned, PMAT required two input data files. In this chapter we will throughly explain how to properly complete either data file.

-----------------------
Configuration Input
-----------------------

Provided below is a template of the ``_pmat.yml`` file that is given in the template repository. While there are some optional fields, most are required.

::

    - sensor:
        name:
        error:
        color:
        ratio:
        range:
        emissivity:
        poster:
        active:
    - train_fraction:
      value:
    - rel_difference:
      value:
    - iteration:
      step:
      seed:
    ---
    - noaa:
      - label:
        id:
    - wyoming:
      - id:

