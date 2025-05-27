#### Runing
Following will install all the project dependecies, compile application and run it.
```bash
cabal run

# if following OpenGL error is encountered
DoublePendulum: user error (unknown GLUT entry glutInit)

# add glut to the system and rerun cabal
sudo apt-get install freeglut3-dev
```