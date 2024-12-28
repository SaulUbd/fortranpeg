# Ejemplo de archivo de entrada para fase 3

Este ejemplo, reconoce desde un archivo de entrada números enteros seguidos por el caracter de nueva línea.
Luego de reconocerlos, el parser debe tener el código necesario para crear una clase stack para ir apilando los números.
Es importante mencionar que la lista de números aparecerá de manera invertida al imprimirla.

## Gramática

Es el conjunto de reglas y expresiones en sintaxis PEG para reconocer números seguidos por el caracter de nueva línea.
Se agreagan las acciones semánticas necesarias a cada regla, escritas en Fortran entre llaves. 
Considerar que al inicio de la gramática también se puede agregar código Fortran entre llaves.

[Gramática](grammar.peg)

![Alt text](img/1.png?raw=true "grammar.peg")

## Parser generado

Al ingresar la gramática en el sitio debe generar un módulo en Fortran llamado "parser.f90" (no cambiarle nombre).
En este ejemplo se siguió la estructura que utiliza PeggyJS para generar el parser (este enfoque puede variar).

[Módulo generado](parser.f90)

![Alt text](img/2.png?raw=true "parser.f90")

## Código para pruebas

Es un archivo en Fortran que utiliza el módulo parser y sirve para hacer pruebas.
Recibe mediante argumento en la línea de comandos el nombre de archivo a leer.
Leído el stream del archivo se invoca y se envía a la función del módulo parser llamada parse(). (de nuevo no cambiarle nombre)
Para este ejemplo se imprime el contenido del Stack cargado.

[Archivo de pruebas](test.f90)

![Alt text](img/3.png?raw=true "test.f90")

## Archivo de entrada

Es un archivo de texto simple, con tres números y tres caracteres de nueva línea.

[Archivo de entrada](test.txt)

![Alt text](img/4.png?raw=true "test.txt")


## Compilación

Se utilizó el bash build para compilar tanto el módulo como el programa test.

[Bash](build)

![Alt text](img/5.png?raw=true "build")


## Ejecución

El ejecutar el archivo test, se debe mostrar el listado de números invertidos.

![Alt text](img/6.png?raw=true "Ejecución")
