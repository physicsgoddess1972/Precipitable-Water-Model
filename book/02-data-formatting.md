# Data Collection


## Data Collection Guidelines
First and foremost, collect honest data. 

## Data Formatting
As we have previously mentioned, PMAT required two input data files. In this chapter we will throughly explain how to properly complete either data file.

### Configuration Input
Provided below is a template of the `_pmat.yml` file that is given in the template repository. While there are some optional fields, most are required.
```yaml
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
```

The first field is for the sensor definition. For every sensor that is used to collect data, one of these entries must be completed.

| Field        | Description                                                                                        | Example               |
|--------------|----------------------------------------------------------------------------------------------------|-----------------------|
| `name`       | The name or identifer of the sensor, for identical sensors use an underscore and a number          | 'TE 1610' or 'AMES_1' |
| `error`      | This is the error on the temperature readings, refer to the technical manual for this information. | 2.5                   |
| `color`      | This is the HEX color that will be used to identify the sensor. Leave as all caps.                 | 'FFF' or 'FA5301'     |
| `ratio`      | The distance to spot ratio                                                                         | '12 to 1'             |
| `range`      | The manufactuers range of the temperature sensor                                                   | '-20 ~ 537'           |
| `emissivity` | The emissivity given by the temperature sensor                                                     | 0.95                  |
| `poster`     | A TRUE/FALSE for whether the sensor will be included in poster plots                               | true or false         |
| `active`     | A TRUE/FALSE for whether the sensor will be invluded in any analysis                               | true or false         |
