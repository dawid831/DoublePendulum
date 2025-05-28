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
  "max_trace_length": 100,
  "color_head": [255, 0, 255],
  "color_trace": [0, 255, 255]
}
```
Where
- mass - mass of each arm
- length - length of each arm
- theta - initial angle of each arm from the vertial in degrees, so -90 would be full left swing
- omega - initial angular velocity of each arm
- max_trace_length - length of the trace of the pendulum head in simulation steps
- color_head - color of the pendulum head, as RGB values
- color_trace - color of the pendulum trace, as RGB values

Then the simulation can be run with, where `example_config.json` describes one pendulum and `another_config.json` describes another pendulum and :
```bash
cabal run DoublePendulum example_config.json <another_config.json>
```