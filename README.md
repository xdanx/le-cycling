le-cycling
==========

Cycling is awesome!

```
       /\   
      /  \/\     ____   __o  
   /\/      \  ---__  _ \<_  
 _/          \_______(_)/(_)  
```

The modules of the cycling simulation are:

Checks.hs
Contains the quickcheck tests we used to verify our software.

Coop.hs
Where cooperation functions for the cyclist can be implemented.
The only one implemented is standardCoop the probabilistic
cooperation strategy.
Implementation of psychological model.

Cyclist.hs
Definition of the Cyclist type and function to create and modify
them.

Main.hs
Parses Command line.
Calls turn repeatedly while actively managing rendering and if necessary
rendering. Also plots graphs at the end.

Modeling.hs
Contains implementation of the physiological model.

Pack.hs
Contains Pack data type and helper functions

Parser.hs
Contains function to parse input file to create a race.

Population.hs
Describes the distribution of various properties of the cyclist
population.

Rendering.hs
Contains functions to render a Race using SDL.

RungeKutta.hs
Contains implementation of physical model.

Simulation.hs
Contains functions to simulate one turn.

Stats.hs
Contains functions for generating normaly distibuted random variables
from the information in the Population module.

Units.hs
Contains type declerations and tick variable.

Utils.hs
Various utility functions.



The syntax of the input file is as follows:
• The first line contains only one number: the length of the race.
10
• Each of the following lines contains the description of one type of cyclist,
and the number of that type of cyclist you want to assign to a number of
teams.
   – We first write a list of team number:number of cyclists
   – then a | to act as separator
   – the name of the strategy function, possibly followed by arguments between brackets. (e.g. standardCoop takes a distribution as argu-
ment)
   – another |
   – a list of property:value to define the properties of the cyclist, property must be one of the variable of the Cyclist type, except for the strategy variables. If a property is omitted, a default value is assigned.

   An example of such a line could be :
   1:20 5:10 | standardCoop avg | pcp:1.28



The plot in nocoop.png shows the finish positions of the cyclists against their pmax, in a race where nobody cooperates. As expected we can see that the cyclists with the bigger pmax achieve better results.
