Data and scripts relating to simulation of occupant-centrioc controls<br/>

roomData.csv is organized as follow:<br/>

B# <- building number (either 1 or 2, to ensure correct outdoor air temperature is used)<br/>
B#_Tout <- outdoor air temperature for each building (degC)<br/>

For each private office, the following datatypes are recorded:<br/>
B###_md <- motion detector (0 = no trigger, 1 = trigger)<br/>
B###_key <- thermostat keypress data (3 = temperature setpoint down, 4 = temperature setpoint up)<br/>
B####_temp <- zone air temperature (degC)<br/>
B###_light <- lighting keypress data (0 = artificial lighting off, 1 = artificial lighting on)<br/>
B###_lux <- ceiling illuminance meter reading (lux)
