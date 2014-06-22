merqtt
======
merqtt is an MQTT broker that full implements the [mqtt 3.1][1] spec.

Building
-----------
The build is based on [rebar 2.5.0][2], a compiled copy of which is included. The usual:

````
make deps
````

````
make compile
````

````
make rel
````

Are all available and result in a built release in `rel/merqtt`



----
[1]: http://mqtt.org/
[2]: https://github.com/rebar/rebar/releases/tag/2.5.0


