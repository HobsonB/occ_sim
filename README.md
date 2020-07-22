Data and scripts relating to simulation of occupant-centrioc controls

roomData.csv is organized as follow:

B# <- building number (either 1 or 2, to ensure correct outdoor air temperature is used)
B#_Tout <- outdoor air temperature for each building (degC)

For each private office, the following datatypes are recorded:
B###_md <- motion detector (0 = no trigger, 1 = trigger)
B###_key <- thermostat keypress data (3 = temperature setpoint down, 4 = temperature setpoint up)
B####_temp <- zone air temperature (degC)
B###_light <- lighting keypress data (0 = artificial lighting off, 1 = artificial lighting on)
B###_lux <- ceiling illuminance meter reading (lux)
