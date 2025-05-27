#### Runing
Following will install all the project dependecies, compile application and run it.
```bash
cabal run

# if following OpenGL error is encountered
DoublePendulum: user error (unknown GLUT entry glutInit)

# add glut to the system and rerun cabal
sudo apt-get install freeglut3-dev
```

#### Config
The simulation can be condigured with the JSON config file:
```json
{
  "mass_1": 1,
  "length_1": 1,
  "theta_1": 45,
  "omega_1": 0,
  "mass_2": 1,
  "length_2": 1,
  "theta_2": 90,
  "omega_2": 0,
  "g": 9.81,
  "max_trace_length": 100
}
```
Where
- mass - mass of each arm
- length - length of each arm
- theta - initial angle of each arm from the vertial in degrees, so -90 would be full left swing
- omega - initial angular velocity of each arm
- max_trace_length - length of the trace of the pendulum head in simulation steps

Then the simulation can be run with:
```bash
cabal run DoublePendulum example_config.json
```