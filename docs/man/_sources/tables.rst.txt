***************
Tables
***************

==================
Input Data File
==================

-----------------------
``cool_data.csv``
-----------------------

+---------------------+----------------------+----------------------+
| Data Type           | Header               | Format               |
+---------------------+----------------------+----------------------+
| Date                | Date                 | YYYY-MM-DD           |
+---------------------+----------------------+----------------------+
| Time                | Time                 | HH:MM                |
+---------------------+----------------------+----------------------+
| String              | Condition            | clear sky / overcast |
+---------------------+----------------------+----------------------+
| Sky Temperature     | Sensor Name (Sky)    | float                |
+---------------------+----------------------+----------------------+
| Ground Temperature  | Sensor Name (Ground) | float                |
+---------------------+----------------------+----------------------+

.. csv-table::
    :header: "Data Type", "Header", "Format"
    :widths: 20, 20, 20

    "datetime", "Date", "YYYY-MM-DD"
    "datetime", "Time", "HH:MM"
    "string", "Condition", "clear sky / overcast"
    "float", "Sensor Name (Sky)", "float"
    "float", "Sensor Name (Ground)", "float"

-----------------------
``_pmat.yml``
-----------------------

.. csv-table::
    :header: "Field Name", "Description", "Example"
    :widths: 20, 20, 20

    "name", "The name of the sensor. If there are multiple of the same sensor use the notation `_N` with N being the index of the sensor.", "Sensor 09 Sensor 10_1 Sensor 10_2"

+---------+---------+-----------+
| error | The manufacturer reported error on the sensor.  (OPTIONAL) | 2.5
5.0 |
+---------+---------+-----------+
| color | A hexadecimal color code that will be used to identify the sensor on visualizations. | FF0000 0000FF |
+---------+---------+-----------+
| ratio | The distance to spot ratio that is reported by the manufacturer. (OPTIONAL) | 12 to 1 21 to 1 |
+---------+---------+-----------+
| emissivity | The emissivity of the sensor as reported by the manufacturer. (OPTIONAL) | 0.95 |
+---------+---------+-----------+
| poster | A boolean that will decide whether the sensor will be shown in the poster-specific plots. | true false |
+---------+---------+-----------+
| active | A boolean that will decide whether the sensor will be used in the analysis | true false |
+---------+---------+-----------+
